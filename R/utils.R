
utils::globalVariables(c("verbose", "zfile", "option", 
                         "Codigo", "Nome", "CodigoIBGE",
                         "BaciaCodigo", "MunicipioCodigo",
                         "MunicipioCodigoIBGE", "EstadoCodigoIBGE",
                         "Sigla", "RioNome", "SubBaciaNome",
                         "BaciaNome", "EstadoNome", "EstacaoCodigo"
                         ))


# function to deal with accents
no_accent <- function(x) stringi::stri_trans_general(x, "latin-ascii")