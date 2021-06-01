#' Parliamentary delegations in specified session
#' 
#' A function for retrieving delegations for a specified parliamentary session
#' 
#' @usage get_session_delegations(sessionid = NA, good_manners = 0)
#' 
#' @param sessionid Character string indicating the id of the parliamentary session to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with response date, version ...
#' 
#' @family get_mp_data
#' 
#' 
#' @examples 
#' 
#' 
#' 
#' 
#' 
#' @import rvest
#' @export
#' 



get_session_delegations <- function(sessionid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/delegasjoner?", sessionid)
  
  tmp <- read_html(url)
  
  tmp <- data.frame(response_date = tmp %>% html_nodes("delegasjoner_liste > delegasjon > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_nodes("delegasjoner_liste > delegasjon > versjon") %>% html_text(),
                    id = tmp %>% html_nodes("delegasjoner_liste > delegasjon > id") %>% html_text(),
                    name = tmp %>% html_nodes("delegasjoner_liste > delegasjon > navn") %>% html_text(),
                    sessionid = tmp %>% html_nodes("sesjon_id") %>% html_text())
  
  Sys.sleep(good_manners)
  
  return(tmp)
  
}


