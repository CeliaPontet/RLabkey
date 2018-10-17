#' Extract EXPERIMENT table from Labkey
#'
#' @param rodbc_chan channel odbc created by labkey_connect{RLabkey}
#' @param protocols a vector of character (length 3 = protocol code)
#' @param annees a vector of years
#' @param cultures a vector of character (length 1 = code culture)
#'
#' @return a experiment dataframe
#' @export
#' @import glue
#' @import magrittr
#' @importFrom RODBC sqlQuery
#' @examples
#' lbk <- labkey_connect(login="consult", mdp="cetiom")
#' experiment <- extract_experiment(lbk, cultures=c("T","C"), protocols="VCE", annees=c(2017,2018), essais=c("VCE31","VCE40"))
#'
extract_experiment <- function(rodbc_chan, cultures=NULL, protocols=NULL, annees=NULL, essais=NULL){

  #Ecriture de la requête sur EXPERIMENT
  request <- glue("SELECT * FROM EXPERIMENT WHERE PROTOCOL_NAME LIKE 'SLE%'")

  #Si un ou des protocols sont renseignés
  req_culture <- paste0("'",cultures,"%'") %>% paste(collapse = " OR EXPERIMENT_NAME LIKE ")
  request <- glue("{request} AND EXPERIMENT_NAME LIKE {req_culture}")

  #Si un ou des protocols sont renseignés
  req_proto <- paste0("'%",protocols,"%'") %>% paste(collapse = " OR EXPERIMENT_NAME LIKE ")
  request <- glue("{request} AND EXPERIMENT_NAME LIKE {req_proto}")

  #Si une ou des annees sont renseignées
  if (!is.null(annees)){
    request <- glue("{request} AND YEAR IN ({paste(annees,collapse=',')})")
  }

  #Si un ou des protocols sont renseignés
  req_essai <- paste0("'%",essais,"%'") %>% paste(collapse = " OR EXPERIMENT_NAME LIKE ")
  request <- glue("{request} AND EXPERIMENT_NAME LIKE {req_essai}")

  experiment <- sqlQuery(rodbc_chan, request)
}
