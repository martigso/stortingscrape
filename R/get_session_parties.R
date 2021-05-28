#' Parliamentary parties in specified session
#' 
#' A function for retrieving Norwegian parliamentary parties for a specified parliamentary session
#' 
#' @usage get_session_parties(sessionid = NA, good_manners = 0)
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



get_session_parties <- function(sessionid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/partier?sesjonid=", sessionid)
  
  tmp <- read_html(url)
  
  tmp <- data.frame(response_date = tmp %>% html_nodes("partier_liste > parti > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_nodes("partier_liste > parti > versjon") %>% html_text(),
                    id = tmp %>% html_nodes("partier_liste > parti > id") %>% html_text(),
                    name = tmp %>% html_nodes("partier_liste > parti > navn") %>% html_text(),
                    represented_party = tmp %>% html_nodes("partier_liste > parti > representert_parti") %>% html_text(),
                    sessionid = tmp %>% html_nodes("sesjon_id") %>% html_text(),
                    period_id = tmp %>% html_nodes("stortingsperiode_id") %>% html_text())
  
  Sys.sleep(good_manners)
  
  return(tmp)
  
}


