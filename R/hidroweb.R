
#' Download station data files from Hidroweb
#'
#' @param stations character vector with code of stations
#' @param options character vector with one or more options: "Cota", "Vazao",
#'  "Chuva", "Qualidade", "Resumo", "Sedimento", "Perfil".
#' @param verbose logical. if TRUE R report extra information on progress.
#' @param dest.dir A character string with the path where the downloaded file 
#' is saved.
#' @param meta logical, if TRUE return information including station metadata. 
#'
#' @return tibble with information of dowloaded stations files.
#' @export
#'
hidroweb_down <- function(stations = "42450750",
                          options = "Chuva",
                          verbose = TRUE,
                          dest.dir = "./",
                          meta = TRUE) {

  # check inputs

  # options and stn combinations
  arg_combs <- expand.grid(options, stations, stringsAsFactors = FALSE)
  arg_combs <- setNames(arg_combs, nm = c("options", "stations"))
  stns_l <- arg_combs$stations
  opts_l <- arg_combs$options
  
  purrr::map2_df(
    stns_l, opts_l,
    ~.hydroweb_down_station(
      .x,
      .y,
      dest.dir,
      verbose,
      metadata = meta
    )
  )
}

# issue #1
# stns <- c("42600000", "42751000")
# opts <- c("Vazao", "Cotas", "Chuva")
# x <- hidroweb_down(stations = stns, options = opts, 
#                    verbose = TRUE, dest.dir = "../", meta = TRUE)


