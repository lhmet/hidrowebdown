#  source("utils-test.R")



.last_update_metadata <- function() {
  # data of last update of metadata ----------------------------------------------
  # http://www.snirh.gov.br/hidroweb/HidroWeb.asp?TocItem=6010
  # Inventário atualizado em 09/02/2018 (last update).
  survey_url <- "http://hidroweb.ana.gov.br/HidroWeb.asp?TocItem=6010#inventario"
  #.check_response(survey_url)
  
  survey_page <- xml2::read_html(survey_url, encoding = "latin1")
  last_updt <- rvest::html_text(rvest::xml_nodes(survey_page, css = "font"))
  last_updt <- stringr::str_extract(last_updt, "[0-9]{2}/[0-9]{2}/[0-9]{4}")
  last_updt <- last_updt[!is.na(last_updt)]
  # message("Metadata updated in ", last_updt)
  
  return(as.Date(last_updt, "%d/%m/%Y"))
}


# Function for prep tbls before merge -----------------------------------------
.prep_tbls_list <- function(tbls.sel, tbls){
  lapply(
    tbls.sel,
    function(itbl_nm) {
      # itbl_nm <- "Estacao"
      itbl <- tbls[[itbl_nm]]
      # factor to char
      itbl <- dplyr::mutate_if(itbl, is.factor, no_accent)
      # rename vars  for merge
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
      } else {# if is the stations df (or 1st df)
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
}


# function to tidy "Estacao" table returning a df with Codigo, Classe, Tipo
.tidy_stn_tbl <- function(x){
  # x <- tables[["Estacao"]]
  
  # variables selection
  x <- dplyr::select(x,
                     dplyr::one_of(
                       c("Codigo", "TipoEstacao", 
                         "TipoEstacaoEscala", "TipoEstacaoRegistradorNivel", 
                         "TipoEstacaoDescLiquida", "TipoEstacaoSedimentos", 
                         "TipoEstacaoQualAgua", "TipoEstacaoPluviometro", 
                         "TipoEstacaoRegistradorChuva", 
                         "TipoEstacaoTanqueEvapo", 
                         "TipoEstacaoClimatologica", "TipoEstacaoPiezometria",
                         "TipoEstacaoTelemetrica"
                       )
                     )
  ) # end select
  
  # replace and make short names
  names(x) <-  gsub("TipoEstacao","", names(x))
  names(x)[names(x) == ""] <- "Classe"
  
  x <- dplyr::arrange(x, Codigo)
  
  # replace values for Classe: stations are Pluviometric or Fluviometric  
  x <- 
    dplyr::mutate(x, 
                  Classe = ifelse(Classe == 1,
                                  "Fluviometric",
                                  "Pluviometric"
                  )
    )
  
  # folowwing 
  # http://arquivos.ana.gov.br/infohidrologicas/InventariodasEstacoesPluviometricas.pdf
  # http://arquivos.ana.gov.br/infohidrologicas/InventariodasEstacoesFluviometricas.pdf
  
  # x <- dplyr::rename(x,
  #                    "P" = Pluviometro,
  #                    "Pr" = RegistradorChuva,
  #                    "E" = TanqueEvapo,
  #                    "C" = Climatologica,
  #                    "T" = Telemetrica,
  #                    "Q" = QualAgua,
  #                    "S" = Sedimentos,
  #                    "D" = DescLiquida,
  #                    "R" = RegistradorNivel,
  #                    "F" = Escala
  # )
  
  # xs in a long format to group Tipo
  #xs <- tidyr::gather(x, Tipo, value, -Codigo, -Classe)
  #xs <- dplyr::filter(xs, value == 1) 
  #xs <- dplyr::select(xs, -value) 
  #xs <- dplyr::arrange(xs, Codigo) 
  #by_code <- dplyr::group_by(xs, Codigo) 
  #by_code <- dplyr::summarise(by_code, 
  #                            Classe = unique(Classe), 
  #                            Tipo = str_collapse(Tipo))
  #by_code
  #return(by_code)
  return(x)
}



#' Get metadata of hydrological stations from Hidroweb database
#' 
#' @param dest.dir optional, character with the path to save the file (HIDRO.mdb)
#' tha has all stations metadata.
#' @importFrom utils download.file unzip
#' @importFrom rlang !! :=
#' @return data frame with stations metadata
#' @export 
#' @examples 
#' \dontrun{
#' # how hidro_metadata was generated
#' # donwload a mdb file of  ~200 Mb
#' hidro_metadata <- hidroweb_metadata_live()
#' }
#'
hidroweb_metadata_live <- function(dest.dir) {
  
  # file of stations metadata --------------------------------------------------
  survey_url_file <-
    "http://www.snirh.gov.br/hidroweb/Baixar/Software/Invent%C3%A1rio.zip"
  
  
  if(missing(dest.dir)) {
    tmp_zip <- tempfile(fileext = ".zip")
  } else {
    tmp_zip <- file.path(dest.dir, basename(survey_url_file))
  }
  on.exit(file.remove(tmp_zip))
  
  utils::download.file(survey_url_file, destfile = tmp_zip)
  
  extracted_file <- utils::unzip(tmp_zip, list = TRUE)[["Name"]]
  unzip(tmp_zip, exdir = dirname(tmp_zip))
  
  mdb_file <- file.path(dirname(tmp_zip), extracted_file)
  if(missing(dest.dir)) on.exit(file.remove(mdb_file))
  # file.exists(mdb_file)
  
  # mdb_file <- "inst/extdata/HIDRO.mdb"
  # Hmisc::contents(d)
  # Hmisc::mdb.get(mdb_file, tables=TRUE)
  sel_tables <- c("Estacao", "Municipio", "Estado", "Bacia", 
                  "SubBacia", "Rio")
  tables <- Hmisc::mdb.get(mdb_file, tables = sel_tables)
  
  # update tables 
  tables_u <- .prep_tbls_list(tbls.sel = sel_tables, tbls = tables)
  
  attributes(tables_u) <- attributes(tables)
  # str(tables_u)
  
  # lapply(
  #   tables,
  #   function(x) {
  #     names(x)
  #   }
  # )
  # merge all dfs
  m <- tibble::as_tibble(Reduce(function(x, y) merge(x, y, all = TRUE), tables_u))
  # head(m)
  #rm(tables); rm(tables_u)
  m <- dplyr::select(m, -(BaciaCodigo:MunicipioCodigo), 
                     -MunicipioCodigoIBGE,
                     -EstadoCodigoIBGE,
                     -TipoEstacao
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
  m <- dplyr::arrange(m, Codigo)
  
  # merge with station
  stn_tbl_type <- tibble::as_tibble(.tidy_stn_tbl(tables[["Estacao"]]))
  m <- dplyr::right_join(m, stn_tbl_type, by = "Codigo")
  
  comment(m) <- paste0(
    "Last update on Hidroweb in: ",
    .last_update_metadata()
  )
  
  # rename variables
  m <- dplyr::rename(m,
                     "station" = Codigo,
                     "lon" = Longitude,
                     "lat" = Latitude,
                     "alt" = Altitude,
                     "area" = AreaDrenagem,
                     "name" = Nome,
                     "city" = Municipio,
                     "state" = Estado,
                     "uf" = UF,
                     "basin" = Bacia,
                     "subbasin" = SubBacia,
                     "river" = Rio,
                     "class" = Classe
                     )
  # to do translation for othe vars
  return(m)
}

# hidroweb_metadata <- hidroweb_metadata_live(
#   dest.dir = system.file("extdata", package = "hidrowebdown")
#   )
# usethis::use_data(hidroweb_metadata, overwrite = TRUE) 


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

