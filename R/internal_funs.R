
# parse lon or lat to decimal----------------------- --------------------------
.coords_dec <- function(x, type = "Longitude"){
  coord <- as.numeric(
    unlist(
      stringr::str_split(
        stringr::str_trim(x[grep(type, x) + 1])
        , ":")
    )
  )
  
  coord_sinal <- sign(coord[1])
  coord <-  round(sum(abs(coord)/c(1, 60, 3600)) * (coord_sinal), 7)
  return(coord)
}

# clean station attributes: code (Código), name (Nome) or river 
.station_attr <- function(x, type = "Codigo"){
  
  info_char <- c("Codigo", "Nome", "Rio", "Bacia", "Sub-bacia", "Estado", 
                 "Municipio", "Operadora", "Responsavel", "Codigo Adicional")
  
  stopifnot(type %in% info_char)
  
  res <- stringr::str_trim(
    x[
      which(
        stringr::str_detect(x, 
                            stringr::fixed(type)
        )
      )[1] + 1
      ]
  )
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


# extract metadata of sydrological stations -----------------------------------
.extract_metadata <- function(cont, verbose = FALSE){

  x <- readLines(textConnection(cont))
  x <-stringr::str_replace(x, "<td valign=\"top\">", "") 
  x <- stringr::str_replace(x, "</td>", "")
  closeAllConnections()
  
  # lon, lat, alt, area
  lon <- .coords_dec(x, type = "Longitude")
  lat <- .coords_dec(x, type = "Latitude")
  
  alt  <- gsub(",", ".", x[grep("Altitude", x) + 1])
  if(verbose) print(alt)
  alt <- ifelse("-" %in% unlist(stringr::str_split(alt, "")), NA, as.numeric(alt))
  
  adren  <- x[grep("Drenagem", x) + 1] 
  if(verbose) print(adren)
  adren <- ifelse("-" %in% unlist(stringr::str_split(adren, "")), NA, as.numeric(adren))

  # dataframe com resultados
  stn_info <- data.frame(code = .station_attr(x, type = "Codigo"),
                         lon = lon,
                         lat = lat,
                         alt = alt,
                         area = adren, 
                         name = .station_attr(x, type = "Nome"),
                         state = .station_attr(x, type = "Estado"),
                         city = .station_attr(x, type = "Municipio"),
                         river = .station_attr(x, type = "Rio"),
                         basin = .station_attr(x, type = "Bacia"),
                         subbasin = .station_attr(x, type = "Sub-bacia"),
                         stringsAsFactors = FALSE)
  return(stn_info)
}


# get hydroweb data for a station------------------------------------------------------------
#' @importFrom utils download.file 
.hydro_data <- function(content, url, stn, dest.folder, verbose = TRUE){
  # cont <- content
  # nova forma de obter sufixo para hidroweb_url
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
  
  #Error in stringr::str_split(content_split[position], "\\.ZIP")[[1]] : 
  #  subscript out of bounds
  
  # stringi::stri_unescape_unicode(zip_file_sufix)
  zip_file_sufix <- paste0(gsub('\\"', "", zip_file_sufix), ".ZIP")
  
  
  if (! length(zip_file_sufix) > 0) {
    if (verbose) message("No data found in hidroweb for the stn ",  stn ,". \n")
    dest_file <- NA
  }
  # apenda zip file a url
  file_url <-  file.path(dirname(url), zip_file_sufix)
  # arquivo de destino
  dest_file <- basename(file_url) 
  dest_file <- gsub(zfile, paste0(dest.folder, stn, "_", option, ".zip"), dest_file)
  utils::download.file(file_url, destfile = dest_file, mode = "wb")
  
  # messages
  if (file.exists(dest_file)) {
    if (verbose) message("File for stn ", stn, " saved.\n")
  } else {
    if (verbose) warning("File for stn ", stn, " can not be saved.\n")
  }
  return(dest_file)
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
  pat <- "option  value="
  options <- x[stringr::str_detect(x, pattern = pat)]
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




# dowload a station data file from hidroweb ------------------------------------
.get_hidroweb <- function(station = "3253016"
                          , option = "Chuva"
                          , verbose = TRUE
                          , dest.dir = "../"
                          , only.info = FALSE) {
  
  # station = "35275000"; option = "Cotas"; verbose = TRUE
  # station = "36020000"; option = "Vazoes"; verbose = TRUE
  hidroweb_url <- .hidroweb_url(station)
    # form to POST
  b <- .get_cboTipoReg(option)
  #zfile <- .get_zip_file(opt = option)

  hidroweb_cont <- .hidroweb_post(hidroweb_url, b, verbose)
  
  meta <- .extract_metadata(hidroweb_cont)

  if (only.options) meta <- dplyr::select(meta, station, options, cboTipoReg)
  if (!.nest) meta <- tidyr::unnest(meta)
  
  
  

  if (httr::status_code(r) != 200) {
    stop("\nThe Hidroweb website does not appear to be responding.\n",
                                          "Please try again later.\n")
  }

    cont <- httr::content(r, as = "text", encoding = "latin1")
    cont <- no_accent(cont)
    # writeLines(cont); cont <- httr::content(r, encoding = "latin1")
    
    # station coordinates and metadata ----------------------------------------
    #cat("metadata", "\n")
    stn_info <- .extract_metadata(cont)
    if (only.info) return(stn_info)
    
    # hydrological data--------------- ----------------------------------------
    dest_file <- .hydro_data(content = cont, 
                             url = hidroweb_url, 
                             stn = station, 
                             dest.folder = dest.dir)

  gc()
  out <- transform(stn_info, file = dest_file)
  return(out)
} # end download_file_hidroweb




