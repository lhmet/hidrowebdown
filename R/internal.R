
# parse lon or lat to decimal----------------------- --------------------------
.coords_dec <- function(x, type = "Longitude") {
  coord <- as.numeric(
    unlist(
      stringr::str_split(
        stringr::str_trim(x[grep(type, x) + 1])
        , ":"
      )
    )
  )
  
  coord_sinal <- sign(coord[1])
  coord <- round(sum(abs(coord) / c(1, 60, 3600)) * (coord_sinal), 7)
  return(coord)
}

# clean station attributes: code (Código), name (Nome) or river ---------------
.station_attr <- function(x, type = "Codigo") {
  # type = "Codigo"
  info_char <- c(
    "Codigo", "Nome", "Rio", "Bacia", "Sub-bacia", "Estado",
    "Municipio", "Operadora", "Responsavel", "Codigo Adicional"
  )
  
  stopifnot(type %in% info_char)
  is_type <- stringr::str_detect(x, pattern = paste0(stringr::fixed(type), "$"))
  
  #  when station not found in database
  if (!length(which(is_type)) == 1) {
    station <- readr::parse_number(x[stringr::str_detect(x, pattern = "Estacao [0-9]{4,8}")])
    msg <- gsub("</p>", "", gsub("<p class='aviso'>", "", x[length(x)]))
    warning(" ", msg)
    if (type == "Codigo") return(station)
    return(NA_character_)
  }
  
  # take next row
  next_row <- x[which(is_type) + 1]
  res <- stringr::str_trim(next_row)
  res[res == "-"] <- NA_character_
  return(res)
}
# Set cboTipoReg value according to option selected  --------------------------
.get_cboTipoReg <- function(option = "Chuva"){
  
  cboTipoReg_values <- c("Cota" = 8
                         ,"Cotas" = 8
                         ,"Vazao" = 9
                         ,"Vazoes" = 9
                         ,"Chuva" = 10
                         ,"Chuvas" = 10
                         ,"Clima" = 11
                         ,"Qualidade" = 12
                         ,"Resumo" = 13
                         ,"Sedimento" = 14
                         ,"Sedimentos" = 14
                         ,"Perfil" = 16)
  
  stopifnot(option %in% names(cboTipoReg_values))
  sel_value_l <- list(cboTipoReg = c(t(cboTipoReg_values[option])))
  return(sel_value_l)
}



# get hidroweb url for a station ----------------------------------------------
.hidroweb_url <- function(.station_code) {
  # .station_code = "3253005"
  hidroweb_url <- 
    "http://hidroweb.ana.gov.br/Estacao.asp?Codigo=XXXXXXXX&CriaArq=true&TipoArq=1"
  # hidroweb_url <- "http://hidroweb.ana.gov.br/Estacao.asp?Codigo=XXXXXXXX"
  hidroweb_url <- stringr::str_replace(
    hidroweb_url,
    "XXXXXXXX",
    as.character(.station_code)
  )
  #.check_response(hidroweb_url)
  return(hidroweb_url)
}

# get station content from hidroweb page---------------------------------------
.hidroweb_post <- function(.url, .b, .verbose = TRUE){
  
  # take time of request ------------------------------------------------------
  st <- system.time(
    HIDROWEB <-  httr::POST(.url, body = .b, encode = "form")
  )
  if(.verbose) print(st["elapsed"])
  
  HIDROWEB <- httr::content(HIDROWEB, as = "text", encoding = "ISO-8859-1")
  HIDROWEB <- no_accent(HIDROWEB)
  return(HIDROWEB)
}

# parse options of data type---------------------------------------------------
parse_options <- function(txt){
  # txt = x_bellow
  #options <- txt[opt_detect]
  options_num <- as.integer(readr::parse_number(txt))
  stopifnot(options_num %in% c(8:14, 16))
  # extract options
  options_str <- unlist(
    lapply(stringr::str_extract_all(txt, "[A-Z]{1}[a-z]{2,40}"),
           function(x){
             paste(x, collapse = " ")
           }
    )
  )
  # selected option
  which_selected <- options_str[grep("selected", txt)]
  option_sel <- rep(FALSE, length(options_str))
  
  if (length(which_selected) > 0) {
    option_sel[options_str %in% which_selected] <- TRUE
  } 
  
  out_opts <- tibble::tibble(option = options_str, 
                             option_num = options_num, 
                             selected = option_sel)
  return(out_opts)
}

# extract option ---------------------------------------------------------------
.get_station_options <- function(cont, station) {

  x <- readLines(textConnection(cont))
  
  row_selector <- grep("Consultar serie de", x)
  
  # station cannot YET be found in the database
  if (length(row_selector) == 0) {
    warning("Station: ", station, " not found in database.")
    out_opts <- tibble::tibble(option = NA_character_,
                               option_num = NA, 
                               select = FALSE)
    return(out_opts)
  }
  
  x_bellow <- x[row_selector:length(x)]
  # detect options ------------------------------------------------------------
  pat <- "option.*value="
  opt_detect <- stringr::str_detect(x_bellow, pattern = pat)
  #opt_detect <- stringr::str_detect(x, pattern = pat)
  # any option found!
  if (sum(!opt_detect) == 0) {
    out_opts <- tibble::tibble(option = NA_character_,
                               option_num = NA, 
                               select = FALSE)
    return(out_opts)
  }
  
  # continue if there is any option --------------------------------------------
  #if (verbose) print(x_bellow[opt_detect])
  
  out_opts <- parse_options(x_bellow[opt_detect])
  
  return(out_opts)
}

# extract metadata of sydrological stations -----------------------------------
.extract_metadata <- function(cont) {
  
  # cont <- hidroweb_cont; .verbose = TRUE
  x <- readLines(textConnection(cont))
  
  x <- stringr::str_replace(x, "<td valign=\"top\">", "")
  x <- stringr::str_replace(x, "</td>", "")
  #closeAllConnections()
  
  # station code
  stn <- .station_attr(x, type = "Codigo")
  
  # lon, lat, alt, area
  lon <- .coords_dec(x, type = "Longitude")
  lat <- .coords_dec(x, type = "Latitude")
  
  alt <- gsub(",", ".", stringr::str_trim(x[grep("Altitude", x) + 1]))
  if (length(alt) == 0) alt <- ""
  if (stringr::str_detect(alt, "-")) {
    alt <- NA
  } else {
    alt <- readr::parse_number(alt)
  }  
  
  adren <- stringr::str_trim(x[grep("Drenagem", x) + 1])
  if (length(adren) == 0) adren <- ""
  if (stringr::str_detect(adren, "-")) {
    adren <- NA
  } else {
    adren <- readr::parse_number(adren)
  }
  
  #opts <- .get_station_options(x, stn, .verbose)
  
  # dataframe com resultados
  stn_info <- tibble::tibble(
    station = stn,
    #options = opts$string,
    #cboTipoReg = opts$number,
    #selected = opts$select,
    lon = lon,
    lat = lat,
    alt = alt,
    area = adren,
    name = .station_attr(x, type = "Nome"),
    state = .station_attr(x, type = "Estado"),
    city = .station_attr(x, type = "Municipio"),
    river = .station_attr(x, type = "Rio"),
    basin = .station_attr(x, type = "Bacia"),
    subbasin = .station_attr(x, type = "Sub-bacia")
  )
  #stn_info <- tidyr::nest(stn_info, options, cboTipoReg, .key = "data_type")
  # stn_info[["data_type"]]
  return(stn_info)
}


# get hydroweb data file address for a station---------------------------------
#' @importFrom utils download.file 
.hydroweb_file <- function(content){
  # content <- hidroweb_cont
  content_split <- stringr::str_split(content, "href=")[[1]]
  position <- unlist(
    lapply(
      content_split,
      function(x) {
        stringr::str_detect(x, "ARQ.*ZIP")
      }
    )
  )
  if (sum(position) < 1) return(NA_character_)
  zip_file_sufix <- stringr::str_split(content_split[position], "\\.ZIP")[[1]][1]
  zip_file_sufix <- paste0(gsub('\\"', "", zip_file_sufix), ".ZIP")
  return(zip_file_sufix)
}


# download hydroweb data file for a station------------------------------------
.hydroweb_down_file <- function(.hidro_file, 
                                .station,
                                .option,
                                .dest.dir, 
                                .verbose){
  # arquivo de destino
  dest_file <-  paste0(.station, "_", .option, ".zip")
  dest_dir <- normalizePath(.dest.dir)
  dest_file <- file.path(dest_dir, dest_file)
  utils::download.file(.hidro_file, dest_file, mode = "wb")
  # check
  if (file.exists(dest_file)) {
    if (.verbose) message("File saved in \n ", dest_file, ".\n")
  } else {
    if (.verbose) warning("Cannot save file of  ", .station, ".\n")
  }
  return(dest_file)
}

# show more options of data type for the station-------------------------------
.show_data_options <- function(x, .station){
  
  other_opts <- dplyr::pull(x, option)
  #other_opts <- other_opts[charmatch(substr(.option, 1, 3), other_opts)]
  message("Station: ", .station, " have data of: ")
  message(paste(other_opts, collapse = ", "),"\n")
  invisible(NULL)
}


# check if station was not found in database -----------------------------------
.hidroweb_not_found_station <- function(cont){
  # cont = hidroweb_cont
  not_found <- stringr::str_detect(cont, "nao encontrada no banco de dados")
  return(not_found)
}

# build template output with NAs -----------------------------------------------
hidroweb_template <- function(.stn, .meta){
  if (.meta) {
    res <- tibble::tibble(station = .stn,
                          lon = NA,
                          lat = NA,
                          alt = NA,
                          area = NA,
                          name = NA_character_,
                          state = NA_character_,
                          city = NA_character_,
                          river = NA_character_,
                          basin = NA_character_,
                          subbasin = NA_character_,
                          options = NA_character_,
                          cboTipoReg = NA,
                          file = NA_character_)
    return(res)
  }
  res <- tibble::tibble(station = .stn,
                        file = NA_character_)
  return(res)
}


# dowload a station data file from hidroweb ------------------------------------
.hydroweb_down_station <- function(station = "3253016"
                          , option = "Chuva"
                          , dest.dir = "../"
                          , metadata = TRUE
                          , verbose = TRUE
                          ) {
  
  # station = "35275000"; option = "Cotas"; verbose = TRUE; metadata = TRUE
  # station = "36020000"; option = "Vazoes"; verbose = TRUE
  # station = "03160001"; option = "Clima"; verbose = TRUE; metadata = TRUE  # EMPTY
  # station = "02242067"; option = "Chuva"; verbose = TRUE
  # station = "02242067"; option = "Clima"; verbose = TRUE
  # station = "02352066" ; option = "Clima"; verbose = TRUE; dest.dir = "../"  # EMPTY
  # station = "42395000"; option = "Vazoes"; verbose = TRUE; dest.dir = "../"
  # station = "42751000"; option = "Vazoes"; verbose = TRUE; dest.dir = "../"
  # station = "00252001"; option = "Chuva"; verbose = TRUE; dest.dir = "../"
  # station = "02447049"; option = "Clima"; metadata = TRUE; verbose = TRUE; dest.dir = "../"
  # 36458000, cadastrada mas sem dados
  # station = "60473000"; option = "Vazao"; metadata = TRUE; verbose = TRUE; dest.dir = "../"
  # station = "55747000"; option = "Vazao"; metadata = TRUE; verbose = TRUE; dest.dir = "../"
  station <- as.character(station)
  hidroweb_url <- .hidroweb_url(station)
    # form to POST
  b <- .get_cboTipoReg(option)
  #zfile <- .get_zip_file(opt = option)
  if (verbose) {
    message("-----------------------------------------------\n",
            "> ", station, ": ", option)
  }
  hidroweb_cont <- .hidroweb_post(hidroweb_url, b, verbose)
  
  # check if station is found in database -------------------------------------
  hidroweb_not_found <- .hidroweb_not_found_station(hidroweb_cont)
  if (hidroweb_not_found) {
    # return template
    tbl_template <- hidroweb_template(station, metadata)
    return(tbl_template)
  }
  
  # get options required for POST request of data file-------------------------
  hidroweb_opts_all <- .get_station_options(hidroweb_cont, station)
  hidroweb_opts_all <- dplyr::mutate(hidroweb_opts_all, station = station)
  hidroweb_opts_all <- dplyr::select(hidroweb_opts_all, station, option:selected)
  hidroweb_opts_current <- dplyr::filter(hidroweb_opts_all, selected)
  hidroweb_opts_others <- dplyr::filter(hidroweb_opts_all, !(selected))

  # get metadata
  if (metadata) {
    hidroweb_meta <- .extract_metadata(hidroweb_cont)
  }

  # pode ocorrer da estação não ter dados para a opção solicitada?
  # check
  if (!any(hidroweb_opts_current$selected)) {
    # RETORNAR TEMPLATE?
    message("RETURN TEMPLATE?")
  }
  
  if ((nrow(hidroweb_opts_others) > 1 ) && verbose) {
    # there is other data for this station
    .show_data_options(hidroweb_opts_others, station)
  }
  
  hidroweb_file <- .hydroweb_file(hidroweb_cont)
  hidroweb_file <- ifelse(!is.na(hidroweb_file), 
                          file.path(dirname(hidroweb_url), hidroweb_file),
                          hidroweb_file
                          )
  # check path to download file
  if (is.na(hidroweb_file)) {
    hidroweb_down_file <- hidroweb_file
    if (verbose) warning("No path to download data was found for station ",
                        station, ", option ", option,". \n")
  } else {
    hidroweb_down_file <- .hydroweb_down_file(hidroweb_file, 
                                             station, 
                                             option, 
                                             dest.dir, 
                                             verbose)
  }
  
  hidroweb_out <- dplyr::select(hidroweb_opts_current, station, option)
  hidroweb_out <- dplyr::mutate(hidroweb_out, file = hidroweb_down_file)
  if (metadata) {
    hidroweb_out <- dplyr::left_join(hidroweb_out, hidroweb_meta, by = "station")
  }

  return(hidroweb_out)
}


# test_p <- .hydroweb_down_station(station = "02243151" , option = "Chuva", verbose = TRUE, dest.dir = "../")
# test_c <- .hydroweb_down_station(station = "02352066" , option = "Clima", verbose = TRUE, dest.dir = "../")


# > 02447049: Clima
# elapsed 
# 120.934 
# Error: length(which(is_type)) == 1 is not TRUE
# In addition: Warning message:
# In if (!stringr::str_detect(hidroweb_meta$options, substr(option,  :
# Show Traceback
# Rerun with Debug
# Error: length(which(is_type)) == 1 is not TRUE 
