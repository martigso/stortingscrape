#' Parliamentary committees over all sessions
#' 
#' A function for retrieving Norwegian parliamentary committees for all parliamentary sessions
#' 
#' @usage get_all_committees()
#' 
#' 
#' @return A data.frame with response date, version ...
#' 
#' @family get_mp_data
#' 
#' 
#' @examples 
#' coms <- get_all_committees()
#' 
#' @import rvest
#' @export
#' 



get_all_committees <- function(){
  
  url <- "https://data.stortinget.no/eksport/allekomiteer"
  
  tmp <- read_html(url)
  
  tmp <- data.frame(response_date = tmp %>% html_nodes("komiteer_liste > komite > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_nodes("komiteer_liste > komite > versjon") %>% html_text(),
                    id = tmp %>% html_nodes("komiteer_liste > komite > id") %>% html_text(),
                    name = tmp %>% html_nodes("komiteer_liste > komite > navn") %>% html_text(),
                    sessionid = tmp %>% html_nodes("sesjon_id") %>% html_text())
  
  return(tmp)
  
}


