# hidroweb_down <- function(code,
#                           data.type = c(
#                             "Cota", "Vazao", "Chuva",
#                             "Qualidade", "Resumo", "Sedimento",
#                             "Perfil"
#                           ),
#                           verbose,
#                           dest.dir = "../",
#                           ){
#   # change args names, expand.grid, check inputs
#   purrr::map_df(id, ~.get_hidroweb(.x, 
#                                    option = data.type,
#                                    .verbose = verbose,
#                                    .dest.dir = dest.dir
#                                    )
#                 )
# }