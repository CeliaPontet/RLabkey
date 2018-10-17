#' labkey_connect
#'
#' @param login a character
#' @param mdp a character
#' @param dsn a character
#'
#' @return a RODBC
#' @export
#' @importFrom RODBC odbcConnect
#' @examples
#' labkey_connect(login="consult", mdp="cetiom")
labkey_connect <- function(login="consult", mdp="cetiom", dsn="LK_CETIOM"){
  lbk <- odbcConnect(dsn, uid=login, pwd=mdp)
}
