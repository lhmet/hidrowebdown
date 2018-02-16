# check if POWER website is responding -----------------------------------------
#' @noRd
.check_response <- function(url) {
  tryCatch(
    httr::http_status(httr::GET(url)),
    error = function(c) {
      c$message <-
        paste0("\nThe hidroweb website does not appear to be responding.\n",
               "Please try again later.\n")
      stop(c)
    }
  )
}


# Set zip file name according to the given option -----------------------------
.switch_zip_file_name <- function(opt) {

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


#####################################################################
## dowload a station data file from hidroweb
#####################################################################
download_file_hidroweb <- function( # stn = "78400000"
                                   # stn = "02950051"
                                   # stn = "02951067"
                                   # stn = "2951071"  # http://hidroweb.ana.gov.br/ARQ/A20170214-012611-908/CLIMA.ZIP
                                   stn = 3256003
                                   , option = "Chuva"
                                   , verbose = TRUE
                                   , dest.dir = "../data/zips/"
                                   , only.info = FALSE) {
  hidroweb_url <- "http://hidroweb.ana.gov.br/Estacao.asp?Codigo=XXXXXXXX&CriaArq=true&TipoArq=1"
  # lista com cboTipoReg para função POST
  option_num_l <- get_cboTipoReg(option) %>%
    list() %>%
    setNames(nm = "cboTipoReg")
  # nome do arquivo zip depende de option
  zfile <- zip_file_name(opt = option)

  r <- hidroweb_url %>%
    stringr::str_replace("XXXXXXXX", stn) %>%
    httr::POST(
      body = option_num_l
      , encode = "form"
      # ,encode = "multipart"
    )

  if (status_code(r) == 200) {
    cont <- rawToChar(r$content) %>% descr::toUTF8()
    # writeLines(cont)
    # cont <- r %>% httr::content(as = "text")
    # coordendas da estação
    stn_info <- extract_coords(cont)

    if (only.info) return(stn_info)
    ## NEW
    # nova forma de obter sufixo para hidroweb_url
    cont_split <- stringr::str_split(cont, "href=")[[1]]
    position <- lapply(
      cont_split,
      function(x) {
        str_detect(x, "ARQ.*ZIP")
      }
    ) %>% unlist()
    zip_file_sufix <- stringr::str_split(cont_split[position], "\\.ZIP")[[1]][1]
    # stringi::stri_unescape_unicode(zip_file_sufix)
    zip_file_sufix <- paste0(gsub('\\"', "", zip_file_sufix), ".ZIP")

    ## OLD
    # sufixo para hidroweb_url
    # zip_file_sufix <- cont %>%
    #  #gregexpr("CHUVAS.ZIP", .) %>%
    #  gregexpr(paste0("ARQ.+/", zfile), .) %>%
    #  regmatches(cont, .) %>%
    #  unlist()
  } # end if status_code

  if (length(zip_file_sufix) > 0) {
    # apenda zip file a url
    file_url <- zip_file_sufix %>% paste0(dirname(hidroweb_url), "/", .)
    # arquivo de destino
    dest_file <- file_url %>%
      basename() %>%
      gsub(zfile, paste0(dest.dir, stn, "_", option, ".zip"), .)
    # download se only.info = FALSE
    # if(!only.info){
    file_url %>% download.file(destfile = dest_file, mode = "wb")
    # avisos
    if (file.exists(dest_file)) {
      if (verbose) cat("Arquivo", stn, "salvo com sucesso.\n")
    } else {
      if (verbose) cat("*** Arquivo", stn, "sem dados de Vazão.***\n")
    }
    # } # end if !only.info
  } else {
    if (verbose) cat("*** Arquivo", stn, "sem dados de Vazão no site.***\n")
    dest_file <- NA
  }
  gc()
  out <- mutate(stn_info, file = dest_file)
  return(out)
} # end download_file_hidroweb
