#' Get Parliamentary Sessions
#' 
#' A function for retrieving dates of the parliamentary sessions after WWII
#' 
#' @usage get_parlsessions() 
#' 
#' @return A data.frame with from/to date and id for all parliamentary sessions after WWII
#' 
#' @family get_mp_data
#' 
#' 
#' @examples 
#' 
#' parlses <- get_parlsessions()
#' parlses
#' 
#' @import rvest
#' 
#' @export
#' 



get_parlsessions <- function(){
  
  tmp <- read_html("https://data.stortinget.no/eksport/sesjoner")
  
  tmp <- data.frame(response_date = tmp %>% html_nodes("sesjoner_liste > sesjon > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_nodes("sesjoner_liste > sesjon > versjon") %>% html_text(),
                    from = tmp %>% html_nodes("sesjoner_liste > sesjon > fra") %>% html_text(),
                    id = tmp %>% html_nodes("sesjoner_liste > sesjon > id") %>% html_text(),
                    to = tmp %>% html_nodes("sesjoner_liste > sesjon > til") %>% html_text())
  
  return(tmp)
  
  
}