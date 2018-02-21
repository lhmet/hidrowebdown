
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
  info_char <- c(
    "Codigo", "Nome", "Rio", "Bacia", "Sub-bacia", "Estado",
    "Municipio", "Operadora", "Responsavel", "Codigo Adicional"
  )
  
  stopifnot(type %in% info_char)
  is_type <- stringr::str_detect(x, pattern = paste0(stringr::fixed(type), "$"))
  stopifnot(length(which(is_type)) == 1)
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

# get zip file according to the given option ----------------------------------
.get_zip_file <- function(opt) {

  # check user input
  stopifnot(
    opt %in% c(
      "Cota", "Vazao", "Chuva",
      "Qualidade", "Resumo", "Sedimento",
      "Perfil"
    ),
    length(opt) == 1,
    is.character(opt)
  )

  switch(opt
    , Cota = "COTAS.ZIP"
    , Cotas = "COTAS.ZIP"
    , Vazao = "VAZOES.ZIP"
    , Vazoes = "VAZOES.ZIP"
    , Chuva = "CHUVAS.ZIP"
    , Chuvas = "CHUVAS.ZIP"
    , Clima = "CLIMA.ZIP"
    , Qualidade = "QUALAGUA.ZIP"
    , Resumo = "RESUMODESC.ZIP"
    , Sedimento = "SEDIMENTOS.ZIP"
    , Sedimentos = "SEDIMENTOS.ZIP"
    , Perfil = "PERFIL.ZIP"
  )
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

# extract option ---------------------------------------------------------------
.get_station_options <- function(x) {
  #pat <- "option  value="
  pat <- "option.*value="
  options <- x[stringr::str_detect(x, pattern = pat)]
  if (length(options) == 0){
    # not found any option!
    return(list(string = NA_character_, number = NA))
  }
  options_num <- as.integer(readr::parse_number(options))
  stopifnot(options_num %in% c(8:14, 16))
  options_str <- unlist(
    lapply(stringr::str_extract_all(options, "[A-Z]{1}[a-z]{2,40}"),
           function(x){
             paste(x, collapse = " ")
           }
    )
  )
  # names(options_num) <- options_str
  # return(options_num)
  return(list(string = options_str, number = options_num))
}

# extract metadata of sydrological stations -----------------------------------
.extract_metadata <- function(cont) {
  
  # cont <- hidroweb_cont
  x <- readLines(textConnection(cont))
  x <- stringr::str_replace(x, "<td valign=\"top\">", "")
  x <- stringr::str_replace(x, "</td>", "")
  closeAllConnections()
  
  # lon, lat, alt, area
  lon <- .coords_dec(x, type = "Longitude")
  lat <- .coords_dec(x, type = "Latitude")
  
  alt <- gsub(",", ".", stringr::str_trim(x[grep("Altitude", x) + 1]))
  if (stringr::str_detect(alt, "-")) {
    alt <- NA
  } else {
    alt <- readr::parse_number(alt)
  }  
  
  adren <- stringr::str_trim(x[grep("Drenagem", x) + 1])
  if (stringr::str_detect(adren, "-")) {
    adren <- NA
  } else {
    adren <- readr::parse_number(adren)
  }
  
  opts <- .get_station_options(x)
  
  # dataframe com resultados
  stn_info <- tibble::tibble(
    station = .station_attr(x, type = "Codigo"),
    options = opts$string,
    cboTipoReg = opts$number,
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
  stn_info <- tidyr::nest(stn_info, options, cboTipoReg, .key = "data_type")
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
  zip_file_sufix <- stringr::str_split(content_split[position], "\\.ZIP")[[1]][1]
  zip_file_sufix <- paste0(gsub('\\"', "", zip_file_sufix), ".ZIP")
  return(zip_file_sufix)
}


# download hydroweb data file for a station------------------------------------
.hydroweb_down_file <- function(.hidro_file, .station, .option, .dest.dir){
  # arquivo de destino
  dest_file <-  paste0(station, "_", option, ".zip")
  dest.dir <- normalizePath(.dest.dir)
  dest_file <- file.path(dest.dir, dest_file)
  utils::download.file(.hidro_file, destfile = dest_file, mode = "wb")
  # check
  if (file.exists(dest_file)) {
    if (verbose) message("File saved in \n ", dest_file, "\n")
  } else {
    if (verbose) warning("Can not save file of  ", .station, ".\n")
  }
  return(dest_file)
}

# show more options when hydroweb have more than one data type per station-----
.show_data_options <- function(.station, .metadata){
  message("Station", .station, " also have data of: \n")
  message(
    paste(
      dplyr::pull(
        dplyr::select(.metadata, options)
      ),
      collapse = ", ")
  )
}


# dowload a station data file from hidroweb ------------------------------------
.hydroweb_down_station <- function(station = "3253016"
                          , option = "Chuva"
                          , verbose = TRUE
                          , dest.dir = "../") {
  
  # station = "35275000"; option = "Cotas"; verbose = TRUE
  # station = "36020000"; option = "Vazoes"; verbose = TRUE
  # station = "03160001"; option = "Clima"; verbose = TRUE  # EMPTY
  # station = "02242067"; option = "Chuva"; verbose = TRUE
  # station = "02242067"; option = "Clima"; verbose = TRUE
  # station = "02352066" ; option = "Clima"; verbose = TRUE; dest.dir = "../"  # EMPTY
  hidroweb_url <- .hidroweb_url(station)
    # form to POST
  b <- .get_cboTipoReg(option)
  #zfile <- .get_zip_file(opt = option)

  hidroweb_cont <- .hidroweb_post(hidroweb_url, b, verbose)
  
  hidroweb_meta <- .extract_metadata(hidroweb_cont)
  # print(hidroweb_meta[["data_type"]][[1]])
  hidroweb_meta <- tidyr::unnest(hidroweb_meta)
  
  if(is.na(hidroweb_meta$options)) {
    if(verbose) warning("No data was found for station ", station, ". \n")
    return(hidroweb_meta)
  }
  
  if (nrow(hidroweb_meta) > 1 & verbose) {
    .show_data_options(station, hidroweb_meta)
  }
  
  hidroweb_file <- .hydroweb_file(hidroweb_cont)
  hidroweb_file <- file.path(dirname(hidroweb_url), hidroweb_file)
  
  hidroweb_down_file <- 
    .hydroweb_down_file(hidroweb_file, station, option, dest.dir)

  hidroweb_meta <- dplyr::filter(hidroweb_meta, options == option)
  hidroweb_meta <- dplyr::mutate(hidroweb_meta, file = hidroweb_down_file)
  
  #gc()
  #closeAllConnections()
  
  return(hidroweb_meta)
} # end download_file_hidroweb


# test_p <- .hydroweb_down_station(station = "02243151" , option = "Chuva", verbose = TRUE, dest.dir = "../")
test_c <- .hydroweb_down_station(station = "02352066" , option = "Clima", verbose = TRUE, dest.dir = "../")
