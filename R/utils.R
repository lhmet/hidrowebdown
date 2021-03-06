
utils::globalVariables(c("verbose", "zfile", "option", 
                         "Codigo", "Nome", "CodigoIBGE",
                         "BaciaCodigo", "MunicipioCodigo",
                         "MunicipioNome", "EstacaoNome",
                         "MunicipioCodigoIBGE", "EstadoCodigoIBGE",
                         "Sigla", "RioNome", "SubBaciaNome",
                         "BaciaNome", "EstadoNome", "EstacaoCodigo",
                         "cboTipoReg", "options", "cboTipoReg", "selected",
                         "Classe", "Pluviometro", "RegistradorChuva",
                         "TanqueEvapo", "Climatologica", "Telemetrica",
                         "Escala", "Tipo", "value", "TipoEstacao",
                         "QualAgua", "Sedimentos", "DescLiquida",
                         "RegistradorNivel")
                       )


# handling special characters e.g. accents in R--------------------------------
no_accent <- function(x) stringi::stri_trans_general(x, "latin-ascii")

# function to collapse string --------------------------------------------------
str_collapse <- function(x, by = "-") paste(x, collapse = by)
