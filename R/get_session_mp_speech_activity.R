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
#' @return A data.frame with the following variables:
#' 
#'    |                        |                                                  |
#'    |:-----------------------|:-------------------------------------------------|
#'    | **response_date**      | Date of data retrieval                           |
#'    | **version**            | Data version from the API                        |
#'    | **session_id**         | Session id                                       |
#'    | **agenda_case_number** | Number indicating the agenda number for the case |
#'    | **meeting_id**         | Meeting id                                       |
#'    | **speech_start_time**  | Start time of speech                             |
#'    | **speech_type**        | Type of speech                                   |
#'    | **speech_length_secs** | Lenght of speech in seconds                      |
#' 
#' @md
#' 
#' @seealso [get_mp] [get_mp_bio] [get_publication]
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' activ <- get_session_mp_speech_activity("2012-2013", "ALYS")
#' head(activ)
#' }
#' 
#' 
#' @import rvest httr
#' @export
#' 



get_session_mp_speech_activity <- function(sessionid = NA, mp_id = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/representanttaleaktiviteter?personid=", mp_id, "&sesjonid=", sessionid)
  
  base <- GET(url)
  
  resp <- http_type(base)
  if(resp != "text/xml") stop(paste0("Response of ", url, " is not text/xml."), call. = FALSE)
  
  status <- http_status(base)
  if(status$category != "Success") stop(paste0("Response of ", url, " returned as '", status$message, "'"), call. = FALSE)
  
  tmp <- read_html(base)
  
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



