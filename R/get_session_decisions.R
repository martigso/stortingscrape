#' Retrieve all decisions for a specified session
#' 
#' A function for retrieving all decisions from a specific parliamentary session.
#' 
#' @usage get_session_decisions(sessionid = NA, good_manners = 0)
#' 
#' @param sessionid Character string indicating the id of the session to request all votes from
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with response date, version ...
#' 
#' @family get_mp_data
#' 
#' @examples 
#' 
#'  
#' @import rvest
#' 
#' @export
#' 



get_session_decisions <- function(sessionid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/stortingsvedtak?sesjonid=", sessionid)
  
  tmp <- read_html(url)
  # response_date = tmp %>% html_elements("voteringsforslag_oversikt > respons_dato_tid") %>% html_text(),  
  
  tmp2 <- data.frame(response_date = tmp %>% html_elements("stortingsvedtak_oversikt > respons_dato_tid") %>% html_text(),
                     version = tmp %>% html_elements("stortingsvedtak_oversikt > versjon") %>% html_text(),
                     session_id = tmp %>% html_elements("stortingsvedtak_oversikt > sesjon_id") %>% html_text(),
                     decision_id = tmp %>% html_elements("stortingsvedtak > id") %>% html_text(),
                     case_id = tmp %>% html_elements("stortingsvedtak > sak_id") %>% html_text(),
                     case_link_url = tmp %>% html_elements("stortingsvedtak > sak_lenke_url") %>% html_text(),
                     decision_date = tmp %>% html_elements("stortingsvedtak > stortingsvedtak_dato_tid") %>% html_text(),
                     decision_link_url = tmp %>% html_elements("stortingsvedtak > stortingsvedtak_lenke_url") %>% html_text(),
                     decision_number = tmp %>% html_elements("stortingsvedtak > stortingsvedtak_nummer") %>% html_text(),
                     decision_text = tmp %>% html_elements("stortingsvedtak > stortingsvedtak_tekst") %>% html_text(),
                     decision_title = tmp %>% html_elements("stortingsvedtak > stortingsvedtak_tittel") %>% html_text(),
                     decision_type_id = tmp %>% html_elements("stortingsvedtak > stortingsvedtak_type > id") %>% html_text(),
                     decision_type_name = tmp %>% html_elements("stortingsvedtak > stortingsvedtak_type > navn") %>% html_text())

  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

