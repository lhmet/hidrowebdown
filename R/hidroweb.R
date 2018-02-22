
#' Download station data files from Hidroweb database
#'
#' @param stations character vector with code of stations
#' @param options character vector with one or more options: "Cota", "Vazao",
#'  "Chuva", "Qualidade", "Resumo", "Sedimento", "Perfil".
#' @param combo Logical, if TRUE a unique combination of all elements of 
#' stations and options is used. Otherwise, donwload is executed to each
#' station-option pair. 
#' @param verbose logical. if TRUE R report extra information on progress.
#' @param dest.dir A character string with the path where the downloaded file 
#' is saved.
#' @param meta logical, if TRUE return information including station metadata. 
#'
#' @return a \code{tibble} with information of downloaded stations files.
#' @export
#'
hidroweb_down <- function(stations = "42450750",
                          options = "Chuva",
                          combo = TRUE,
                          verbose = TRUE,
                          dest.dir = "./",
                          meta = TRUE) {

  # check inputs

  if(combo){
    # options and stn combinations
    arg_combs <- expand.grid(options, stations, stringsAsFactors = FALSE)
    arg_combs <- setNames(arg_combs, nm = c("options", "stations"))
    stns_l <- arg_combs$stations
    opts_l <- arg_combs$options  
  } else {
    stopifnot(length(stations) == length(options))
    stns_l <- stations
    opts_l <- options  
  }
  
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


