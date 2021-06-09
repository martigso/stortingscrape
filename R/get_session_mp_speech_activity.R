#' Retrieve all speech activity from one MP for a given session
#' 
#' A function for retrieving all speech activity from an MP during a specific parliamentary session. 
#' Only available from the 2011-2012 session and onwards.
#' 
#' @usage get_session_mp_speech_activity(sessionid = NA, mp_id = NA, good_manners = 0)
#' 
#' @param sessionid Character string indicating the session to retrieve speeches from.
#' @param mp_id Character string for the MP to retreive all speeches of in a given session.
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
#' @import rvest 
#' @export
#' 



get_session_mp_speech_activity(sessionid = NA, mp_id = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/representanttaleaktiviteter?personid=", mp_id, "&sesjonid=", sessionid)
  
  tmp <- read_html(url)
  
  tmp2 <- data.frame(response_date = tmp %>% html_elements("representant_tale_aktivitet_oversikt > respons_dato_tid") %>% html_text(),
                     version = tmp %>% html_elements("representant_tale_aktivitet_oversikt > versjon") %>% html_text(),
                     session_id = tmp %>% html_elements("representant_tale_aktivitet_oversikt > sesjon_id") %>% html_text(),
                     agenda_case_number = tmp %>% html_elements("representant_tale_aktivitet > dagsorden_sak_nummer") %>% html_text(),
                     meeting_id = tmp %>% html_elements("representant_tale_aktivitet > mote_id") %>% html_text(),
                     speech_start_time = tmp %>% html_elements("representant_tale_aktivitet > tale_start_tid") %>% html_text(),
                     speech_type = tmp %>% html_elements("representant_tale_aktivitet > tale_type") %>% html_text(),
                     speech_length_secs = tmp %>% html_elements("representant_tale_aktivitet > tale_varighet_sekunder") %>% html_text())
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}



