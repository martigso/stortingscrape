#' Retreive agenda for a specified meeting
#' 
#' A function for retrieving the agenda for a specific meeting
#' 
#' @usage get_meeting_agenda(meetingid = NA, good_manners = 0)
#' 
#' @param meetingid Character string indicating the id of the meeting to request all votes from
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



get_meeting_agenda <- function(meetingid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/dagsorden?moteid=", meetingid)
  
  tmp <- read_html(url)
  
  tmp2 <- data.frame(response_date = tmp %>% html_elements("mote_dagsorden_oversikt > respons_dato_tid") %>% html_text(),
                     version = tmp %>% html_elements("mote_dagsorden_oversikt > versjon") %>% html_text(),
                     agenda_number = tmp %>% html_elements("mote_dagsorden_oversikt > dagsorden_nummer") %>% html_text(),
                     meeting_date = tmp %>% html_elements("mote_dagsorden_oversikt > mote_dato_tid") %>% html_text(),
                     meeting_id = tmp %>% html_elements("mote_dagsorden_oversikt > mote_id") %>% html_text(),
                     meeting_place = tmp %>% html_elements("mote_dagsorden_oversikt > mote_ting") %>% html_text(),
                     agenda_case_reference = tmp %>% html_elements("dagsordensak > dagsordensak_henvisning") %>% html_text(),
                     agenda_case_number = tmp %>% html_elements("dagsordensak > dagsordensak_nummer") %>% html_text(),
                     agenda_case_text = tmp %>% html_elements("dagsordensak > dagsordensak_tekst") %>% html_text(),
                     agenda_case_type = tmp %>% html_elements("dagsordensak > dagsordensak_type") %>% html_text(),
                     footnote = tmp %>% html_elements("dagsordensak > fotnote") %>% html_text(),
                     proposition_id = tmp %>% html_elements("dagsordensak > innstilling_id") %>% html_text(),
                     committee_id = tmp %>% html_elements("dagsordensak > komite_id") %>% html_text(),
                     loose_proposals = tmp %>% html_elements("dagsordensak > loseforslag") %>% html_text(),
                     case_id = tmp %>% html_elements("dagsordensak > sak_id") %>% html_text(),
                     question_hour_type = tmp %>% html_elements("dagsordensak > sporretime_type") %>% html_text(),
                     question_id = tmp %>% html_elements("dagsordensak > sporsmal_id") %>% html_text())
  
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

