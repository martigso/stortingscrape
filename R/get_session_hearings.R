#' Retrieve hearings in specified session
#' 
#' A function for retrieving all hearings in a specified parliamentary session
#' 
#' @usage get_session_hearings(sessionid = NA, good_manners = 0, cores = 1)
#' 
#' @param sessionid Character string indicating the id of the parliamentary session to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' @param cores Integer...
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
#' @import rvest parallel
#' @export
#' 



get_session_hearings <- function(sessionid = NA, good_manners = 0, cores = 1){
  
  url <- paste0("https://data.stortinget.no/eksport/horinger?sesjonid=", sessionid)
  
  tmp <- read_html(url)
  
  tmp2 <- list(
    root = data.frame(
      response_date = tmp %>% html_elements("horinger_oversikt > respons_dato_tid") %>% html_text(),
      version = tmp %>% html_elements("horinger_oversikt > versjon") %>% html_text(),
      session_id = tmp %>% html_elements("horinger_oversikt > sesjon_id") %>% html_text()
    ),
    hearing = data.frame(
      deadline_date = tmp %>% html_elements("horing > anmodningsfrist_dato_tid") %>% html_text(),
      status = tmp %>% html_elements("horing > horing_status") %>% html_text(),
      hearing_id = tmp %>% html_elements("horing > id") %>% html_text(),
      input_deadline = tmp %>% html_elements("horing > innspillsfrist") %>% html_text(),
      written = tmp %>% html_elements("horing > skriftlig") %>% html_text(),
      application_deadline = tmp %>% html_elements("horing > soknadfrist_dato") %>% html_text(),
      start_date = tmp %>% html_elements("horing > start_dato") %>% html_text(),
      status = tmp %>% html_elements("horing > status") %>% html_text(),
      status_info_text = tmp %>% html_elements("horing > status_info_tekst") %>% html_text(),
      type = tmp %>% html_elements("horing > type") %>% html_text(),
      committee_id = tmp %>% html_elements("horing > komite > id") %>% html_text()
    ))
  
  tmp2$hearing_case_info <- mclapply(tmp %>% html_elements("horing_sak_info_liste"), function(x){
    data.frame(case_reference = x %>% html_elements("horing_sak_info > sak_henvisning") %>% html_text(),
               case_id = x %>% html_elements("horing_sak_info > sak_id") %>% html_text(),
               case_short_title = x %>% html_elements("horing_sak_info > sak_korttittel") %>% html_text(),
               case_publication = x %>% html_elements("horing_sak_info > sak_publikasjon") %>% html_text(),
               case_title = x %>% html_elements("horing_sak_info > sak_tittel") %>% html_text())
  }, mc.cores = cores)
  
  names(tmp2$hearing_case_info) <- tmp2$hearing$hearing_id
  
  
  tmp2$hearing_date <- mclapply(tmp %>% html_elements("horingstidspunkt_liste"), function(x){
    data.frame(place = x %>% html_elements("horingstidspunkt > sted") %>% html_text(),
               date = x %>% html_elements("horingstidspunkt > tidspunkt") %>% html_text())
  })
  
  names(tmp2$hearing_date) <- tmp2$hearing$hearing_id
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}


