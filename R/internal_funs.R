.hydro_data <- function(content, stn){
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
  # stringi::stri_unescape_unicode(zip_file_sufix)
  zip_file_sufix <- paste0(gsub('\\"', "", zip_file_sufix), ".ZIP")
  
  
  if (! length(zip_file_sufix) > 0) {
    if (verbose) message("No data found in hidroweb for the stn ",  stn ,". \n")
    dest_file <- NA
  }
  # apenda zip file a url
  file_url <-  file.path(dirname(hidroweb_url), zip_file_sufix)
  # arquivo de destino
  dest_file <- basename(file_url) 
  dest_file <- gsub(zfile, paste0(dest.dir, stn, "_", option, ".zip"), dest_file)
  download.file(file_url, destfile = dest_file, mode = "wb")
  
  # messages
  if (file.exists(dest_file)) {
    if (verbose) message("File for stn ", stn, " saved.\n")
  } else {
    if (verbose) warning("File for stn ", stn, " can not be saved.\n")
  }
  return(dest_file)
} 

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

# clean station attributes: code (Código), name (Nome) or river ()
.station_attr <- function(x, type = "Código"){
  
  info_char <- c("Código", "Nome", "Rio", "Bacia", "Sub-bacia", "Estado", "Município",
            "Operadora", "Responsável", "Código Adicional")
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
                         ,"Vazao" = 9
                         ,"Chuva" = 10
                         ,"Clima" = 11
                         ,"Qualidade" = 12
                         ,"Resumo" = 13
                         ,"Sedimento" = 14
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
    , Vazao = "VAZOES.ZIP"
    , Chuva = "CHUVAS.ZIP"
    , Clima = "CLIMA.ZIP"
    , Qualidade = "QUALAGUA.ZIP"
    , Resumo = "RESUMODESC.ZIP"
    , Sedimento = "SEDIMENTOS.ZIP"
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
  
  # info <- c("Código", "Nome", "Rio", "Bacia", "Sub-bacia", "Estado", "Município",
  #   "Operadora", "Responsável", "Código Adicional")
  #purrr::map(info, function(itype) .station_attr(x, type = itype))

  # dataframe com resultados
  stn_info <- data.frame(code = .station_attr(x, type = "Código"),
                         lon = lon,
                         lat = lat,
                         alt = alt,
                         area = adren, 
                         name = .station_attr(x, type = "Nome"),
                         state = .station_attr(x, type = "Estado"),
                         city = .station_attr(x, type = "Município"),
                         river = .station_attr(x, type = "Rio"),
                         basin = .station_attr(x, type = "Bacia"),
                         subbasin = .station_attr(x, type = "Sub-bacia"),
                         stringsAsFactors = FALSE)
  return(stn_info)
}


# dowload a station data file from hidroweb ------------------------------------
download_file_hidroweb <- function(station = 3152014
                                   , option = "Chuva"
                                   , verbose = TRUE
                                   , dest.dir = "../data/zips/"
                                   , only.info = FALSE) {
  
  hidroweb_url <- "http://hidroweb.ana.gov.br/Estacao.asp?Codigo=XXXXXXXX&CriaArq=true&TipoArq=1"
  hidroweb_url <- stringr::str_replace(hidroweb_url, 
                                       "XXXXXXXX", 
                                       as.character(station)
  )
  # form to POST
  option_num_l <- .get_cboTipoReg(option)
  zfile <- .get_zip_file(opt = option)

  # request
  r <-  httr::POST(hidroweb_url, body = option_num_l, encode = "form")

  if (httr::status_code(r) != 200) {
    stop("\nThe Hidroweb website does not appear to be responding.\n",
                                          "Please try again later.\n")
  }
    cont <- base::rawToChar(r$content) 
    base::Encoding(cont) <- "latin1"
    # writeLines(cont); cont <- httr::content(r, encoding = "latin1")
    
    # station coordinates and metadata ----------------------------------------
    stn_info <- .extract_metadata(cont)
    if (only.info) return(stn_info)
    
    # hydrological data--------------- ----------------------------------------
    dest_file <- .hydro_data(cont, station)

  gc()
  out <- mutate(stn_info, file = dest_file)
  return(out)
} # end download_file_hidroweb


