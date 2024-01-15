#' Retrieve all meetings for a specified parliamentary session
#' 
#' A function for retrieving meetings from a specific parliamentary session
#' 
#' @usage get_session_meetings(sessionid = NA, good_manners = 0)
#' 
#' @param sessionid Character string indicating the id of the session to request all votes from
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                       |                                                                          |
#'    |:----------------------|:-------------------------------------------------------------------------|
#'    | **response_date**     | Date of data retrieval                                                   |
#'    | **version**           | Data version from the API                                                |
#'    | **session_id**        | Session id                                                               |
#'    | **agenda_number**     | Agenda number within the session                                         |
#'    | **footnote**          | Footnotes for the meeting                                                |
#'    | **meeting_id**        | Meeting id                                                               |
#'    | **no_meeting_text**   | Description of why there was no meeting, if relevant                     |
#'    | **evening_meeting**   | Whether the meeting was an evening meeting or not                        |
#'    | **note**              | Note for the meeting                                                     |
#'    | **meeting_date**      | Date the meeting took place                                              |
#'    | **meeting_order**     | Indicator for meeting order                                              |
#'    | **meeting_place**     | Where the meeting took place                                             |
#'    | **transcript_id**     | Id for transcript (usually empty)                                        |
#'    | **additional_agenda** | Logical indicator for whether there was additional agenda to the meeting |
#' 
#' @seealso [get_meeting_agenda] [get_question_hour]
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' meet <- get_session_meetings("2013-2014")
#' head(meet)
#' 
#' }
#'  
#' @import rvest httr2
#' 
#' @export
#' 
get_session_meetings <- function(sessionid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/moter?sesjonid=", sessionid)
  
  base <- request(url)
  
  resp <- base |> 
    req_error(is_error = function(resp) FALSE) |> 
    req_perform()
  
  if(resp$status_code != 200) {
    stop(
      paste0(
        "Response of ", 
        url, 
        " is '", 
        resp |> resp_status_desc(),
        "' (",
        resp$status_code,
        ")."
      ), 
      call. = FALSE)
  }
  
  if(resp_content_type(resp) != "text/xml") {
    stop(
      paste0(
        "Response of ", 
        url, 
        " returned as '", 
        resp_content_type(resp), 
        "'.",
        " Should be 'text/xml'."), 
      call. = FALSE) 
  }
  
  tmp <- resp |> 
    resp_body_html(check_type = FALSE, encoding = "utf-8") 
  
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

