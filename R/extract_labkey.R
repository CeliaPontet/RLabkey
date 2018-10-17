#' Extract data from Labkey
#'
#' @param rodbc_chan channel odbc created by labkey_connect{RLabkey}
#' @param protocols a vector of character (length 3 = protocol code)
#' @param annees a vector of years
#' @param cultures a vector of character (length 1 = code culture)
#'
#' @return a dataframe
#' @export
#' @import glue
#' @import magrittr
#' @importFrom RODBC sqlQuery
#' @examples
#' lbk <- labkey_connect(login="consult", mdp="cetiom")
#' data <- extract_labkey(lbk, cultures=c("T","C"), protocols="VCE", annees=c(2017,2018))
#'
extract_labkey <- function(rodbc_chan, cultures=NULL, protocols=NULL, annees=NULL){
  experiment <- extract_experiment(rodbc_chan, cultures=cultures, protocols=protocols, annees=annees)
}
