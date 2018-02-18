#' Metadata of hydrological stations on Hidroweb database
#'
#' A dataset containing coordinates and additional attributes of 46036
#'  hydrological stations
#'
#'@name hidro_metadata
#'@keywords datasets
#'@docType data
#'
#' @format A data frame with 46036 rows and 13 variables:
#' \describe{
#' \item{Codigo}{station code ID}
#' \item{Longitude}{ in decimal degrees}
#' \item{Latitude}{in decimal degrees}
#' \item{Altitude}{masl}
#' \item{AreaDrenagem}{Drainage area, Km2}
#' \item{TipoEstacao}{station type}
#' \item{Municipio}{city}
#' \item{Estado}{state name}
#' \item{UF}{Federal Unit}
#' \item{Bacia}{river basin}
#' \item{SubBacia}{sub basin}
#' \item{Rio}{river name}
#' }
#' @source \url{http://hidroweb.ana.gov.br/HidroWeb.asp?TocItem=6010#inventario}
"hidro_metadata"