
#' Get metadata of hydrological stations from Hidroweb database 
#'
#' @return data frame with station metadata 
#' @export
#'
hidroweb_meta <- function(){
  
  # data of last update of metada ----------------------------------------------
  # http://www.snirh.gov.br/hidroweb/HidroWeb.asp?TocItem=6010
  # Inventário atualizado em 09/02/2018 (last update).
  survey_page <- xml2::read_html("http://hidroweb.ana.gov.br/HidroWeb.asp?TocItem=6010#inventario",
                                 encoding = "latin1")
  last_updt <- rvest::html_text(rvest::xml_nodes(survey_page, css = "font"))
  last_updt <- stringr::str_extract(last_updt, "[0-9]{2}/[0-9]{2}/[0-9]{4}")
  last_updt <- last_updt[!is.na(last_updt)]
  message("Metadata updated in ", last_updt)
  
  # file of stations metadata --------------------------------------------------
  survey_url_file <- 
    "http://www.snirh.gov.br/hidroweb/Baixar/Software/Invent%C3%A1rio.zip"
  
  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(file.remove(tmp_zip))
  download.file(survey_url_file, destfile = tmp_zip)
  
  extracted_file <- unzip(tmp_zip, list = TRUE)[["Name"]]
  on.exit(file.remove(extracted_file))
  unzip(tmp_zip, exdir = dirname(tmp_zip))
        
  mdb_file <- file.path(dirname(tmp_zip), extracted_file)
  # file.exists(mdb_file)
  
  #d <- Hmisc::mdb.get(mdb_file)
  # Hmisc::contents(d)
  # Hmisc::mdb.get(mdb_file, tables=TRUE)
  stn_table <-  tibble::as_tibble(Hmisc::mdb.get(mdb_file, tables = "Estacao"))
  stn_table <- dplyr::mutate_if(stn_table, is.factor, as.character)
  stn_table[, c("Codigo", "Longitude", "Latitude", "Altitude",
                "AreaDrenagem",
                "RioCodigo", "BaciaCodigo", "SubBaciaCodigo", "EstadoCodigo",
                "MunicipioCodigo", "TipoEstacao")]
  stn_table <- tibble::as_tibble(stn_table)
 
  mncps <- Hmisc::mdb.get(
    mdb_file,
    tables = "Municipio")[, c("Nome", "Codigo", "CodigoIBGE")]
  mncps <- tibble::as_tibble(mncps)
  mncps <- dplyr::mutate_if(mncps, is.factor, as.character)
  mncps <- dplyr::rename(mncps, 
                         "Municipio" = Nome,
                         "MunicipioCodigo" = Codigo,
                         "MunicipioCodigoIBGE" = CodigoIBGE)
  
  stn_table <- dplyr::full_join(stn_table, mncps, by = c("MunicipioCodigo"))
  stn_table$MunicipioCodigo <- NULL
  
  # states
  states <- Hmisc::mdb.get(
    mdb_file,
    tables = "Estado")[, c("Nome", "Codigo", "CodigoIBGE", "Sigla")]
  states <- tibble::as_tibble(states)
  states <- dplyr::mutate_if(states, is.factor, as.character)
  states <- dplyr::rename(states, 
                          "Estado" = Nome,
                          "EstadoCodigo" = Codigo,
                          "EstadoCodigoIBGE" = CodigoIBGE,
                          "UF" = Sigla)
  stn_table <- dplyr::full_join(stn_table, states, by = c("EstadoCodigo"))
  stn_table$EstadoCodigo <- NULL
  
  stn_table <- dplyr::select(Codigo:Altitude, Municipio, Estado, UF, TipoEstacao)
  stn_table
}
