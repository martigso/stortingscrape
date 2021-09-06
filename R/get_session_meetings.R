#' Retreive all meetings for a specified parliamentary session
#' 
#' A function for retrieving meetings from a specific parliamentary session
#' 
#' @usage get_session_meetings(sessionid = NA, good_manners = 0)
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



get_session_meetings <- function(sessionid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/moter?sesjonid=", sessionid)
  
  tmp <- read_html(url)
  # response_date = tmp %>% html_elements("voteringsforslag_oversikt > respons_dato_tid") %>% html_text(),  
  
  tmp2 <- data.frame(response_date = tmp %>% html_elements("mote_oversikt > respons_dato_tid") %>% html_text(),
                     version = tmp %>% html_elements("mote_oversikt > versjon") %>% html_text(),
                     session_id = tmp %>% html_elements("mote_oversikt > sesjon_id") %>% html_text(),
                     agenda_number = tmp %>% html_elements("mote > dagsorden_nummer") %>% html_text(),
                     footnote = tmp %>% html_elements("mote > fotnote") %>% html_text(),
                     meeting_id = tmp %>% html_elements("mote > id") %>% html_text(),
                     no_meeting_text = tmp %>% html_elements("mote > ikke_motedag_tekst") %>% html_text(),
                     evening_meeting = tmp %>% html_elements("mote > kveldsmote") %>% html_text(),
                     note = tmp %>% html_elements("mote > merknad") %>% html_text(),
                     meeting_date = tmp %>% html_elements("mote > mote_dato_tid") %>% html_text(),
                     meeting_order = tmp %>% html_elements("mote > mote_rekkefolge") %>% html_text(),
                     meeting_place = tmp %>% html_elements("mote > mote_ting") %>% html_text(),
                     transcript_id = tmp %>% html_elements("mote > referat_id") %>% html_text(),
                     additional_agenda = tmp %>% html_elements("mote > tilleggsdagsorden") %>% html_text())
  
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

