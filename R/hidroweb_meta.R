
.check_response <- function(url) {
  tryCatch(
    unlist(httr::http_status(httr::GET(url))),
    error = function(e) {
      e$message <-
        paste0(
          "\nThe Hidroweb website does not appear to be responding.\n",
          "Please try again later.\n"
        )
      stop(e)
    }
  )
}

.last_update_metadata <- function() {
  # data of last update of metadata ----------------------------------------------
  # http://www.snirh.gov.br/hidroweb/HidroWeb.asp?TocItem=6010
  # Inventário atualizado em 09/02/2018 (last update).
  survey_url <- "http://hidroweb.ana.gov.br/HidroWeb.asp?TocItem=6010#inventario"
  .check_response(survey_url)
  
  survey_page <- xml2::read_html(survey_url, encoding = "latin1")
  last_updt <- rvest::html_text(rvest::xml_nodes(survey_page, css = "font"))
  last_updt <- stringr::str_extract(last_updt, "[0-9]{2}/[0-9]{2}/[0-9]{4}")
  last_updt <- last_updt[!is.na(last_updt)]
  # message("Metadata updated in ", last_updt)
  
  return(as.Date(last_updt, "%d/%m/%Y"))
}


#' Get metadata of hydrological stations from Hidroweb database
#' @importFrom utils download.file unzip
#' @importFrom rlang `!!` `:=`
#' @return data frame with stations metadata
#' @export
#'
hidroweb_meta <- function() {
  
  # file of stations metadata --------------------------------------------------
  survey_url_file <-
    "http://www.snirh.gov.br/hidroweb/Baixar/Software/Invent%C3%A1rio.zip"
  
  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(file.remove(tmp_zip))
  utils::download.file(survey_url_file, destfile = tmp_zip)
  
  extracted_file <- utils::unzip(tmp_zip, list = TRUE)[["Name"]]
  unzip(tmp_zip, exdir = dirname(tmp_zip))
  
  mdb_file <- file.path(dirname(tmp_zip), extracted_file)
  on.exit(file.remove(mdb_file))
  # file.exists(mdb_file)
  
  # d <- Hmisc::mdb.get(mdb_file)
  # Hmisc::contents(d)
  # Hmisc::mdb.get(mdb_file, tables=TRUE)
  sel_tables <- c("Estacao", "Municipio", "Estado", "Bacia", 
                  "SubBacia", "Rio")
  tables <- Hmisc::mdb.get(mdb_file, tables = sel_tables)
  tables_u <- lapply(
    sel_tables,
    function(itbl_nm) {
      # itbl_nm <- "Estacao"
      itbl <- tables[[itbl_nm]]
      itbl <- dplyr::mutate_if(itbl, is.factor, no_accent)
      # vars to be renamed for merge
      var_cod <- paste0(itbl_nm, "Codigo")
      var_nm <- paste0(itbl_nm, "Nome")
      var_ibge <- paste0(itbl_nm, "CodigoIBGE")
      if ("CodigoIBGE" %in% names(itbl)) {
        itbl <- dplyr::rename(
          itbl,
          !! var_cod := Codigo,
          !! var_nm := Nome,
          !! var_ibge := CodigoIBGE
        )
      } else {
        itbl <- dplyr::rename(
          itbl,
          !! var_cod := Codigo,
          !! var_nm := Nome
        )
      }
      if (!("EstacaoNome" %in% names(itbl) | "EstacaoCodigo" %in% names(itbl))) {
        itbl <- dplyr::select(
          itbl,
          dplyr::contains("Codigo"),
          dplyr::contains("Nome"),
          dplyr::contains("Sigla")
        )
      } else { # if is the stations df (or 1st df)
        itbl <- dplyr::select(
          itbl,
          dplyr::one_of(
            c(
              "EstacaoCodigo", "Longitude", "Latitude", "Altitude",
              "AreaDrenagem", "EstacaoNome", "EstadoCodigo", "MunicipioCodigo",
              "BaciaCodigo", "SubBaciaCodigo", "RioCodigo", "TipoEstacao"
            )
            # "RioCodigo", "SubBaciaCodigo",
          )
        ) # end select
      } # end if
      return(itbl)
    } # end anonymous function
  ) # end lapply
  
  attributes(tables_u) <- attributes(tables)
  # str(tables_u)
  
  # lapply(
  #   tables_u,
  #   function(x) {
  #     names(x)
  #   }
  # )
  # merge all dfs
  m <- Reduce(function(x, y) merge(x, y, all = TRUE), tables_u)
  # head(m)
  rm(tables); rm(tables_u)
  m <- dplyr::select(m, -(BaciaCodigo:MunicipioCodigo), 
                     -MunicipioCodigoIBGE,
                     -EstadoCodigoIBGE,
  )
  m <- dplyr::rename(m,
                     "UF" = Sigla,
                     "Rio" = RioNome,
                     "SubBacia" = SubBaciaNome,
                     "Bacia" = BaciaNome,
                     "Estado" = EstadoNome,
                     "Municipio" = MunicipioNome,
                     "Nome" = EstacaoNome,
                     "Codigo" = EstacaoCodigo
  )
  

  comment(m) <- paste0(
    "Updated on Hidroweb in:",
    .last_update_metadata()
  )
  return(tibble::as_tibble(m))
}

# hidro_metada <- hidroweb_meta()
# usethis::use_data(hidro_metada) 


# know issue
# when mdbtools is not installed - required by Hmisc package to read .mdb file in unix
# trying URL 'http://www.snirh.gov.br/hidroweb/Baixar/Software/Invent%C3%A1rio.zip'
# Content type 'application/x-zip-compressed' length 215330561 bytes (205.4 MB)
# ==================================================
#  downloaded 205.4 MB

# sh: 1: mdb-schema: not found
# Show Traceback
# Rerun with Debug
# Error in system(paste("mdb-schema -T", shQuote(tab), file), intern = TRUE) :
#  error in running command
# my session info devtools::session_info()
#
# Reply:
# hidroweb website provide stations metadata in a Microsoft Access Database (mdb).
# To read tables in a mdb from hidroweb site, hidrowebdown uses the function mdb.get()
# from Hmisc R-package. Please see ?Hmisc::mdb.get
# In Debian/Ubuntu Linux run apt get install mdbtools.
