#' Retreive agenda for a specified meeting
#' 
#' @description A function for retrieving the agenda for a specific meeting.
#' 
#' @usage get_meeting_agenda(meetingid = NA, good_manners = 0)
#' 
#' @param meetingid Character string indicating the id of the meeting to request all votes from
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                           |                                               |
#'    |:--------------------------|:----------------------------------------------|
#'    | **response_date**         | Date of data retrieval                        |
#'    | **version**               | Data version from the API                     |
#'    | **agenda_number**         | The agenda number for the session             |
#'    | **meeting_date**          | Date of the meeting                           |
#'    | **meeting_id**            | Meeting id                                    |
#'    | **meeting_place**         | Where the meeting was held                    |
#'    | **agenda_case_reference** | Reference for the case on the agenda          |
#'    | **agenda_case_number**    | Case number                                   |
#'    | **agenda_case_text**      | Case description                              |
#'    | **agenda_case_type**      | Case type                                     |
#'    | **footnote**              | Footnote for the case                         |
#'    | **proposition_id**        | If relevant, belonging proposition id         |
#'    | **committee_id**          | If relevant, id of the responsible committee  |
#'    | **loose_proposals**       | Whether there are loose proposals to the case |
#'    | **case_id**               | Id of the case                                |
#'    | **question_hour_type**    | If relevant, type of question hour            |
#'    | **question_id**           | If relevant, question id                      |
#' 
#' @md
#' 
#' @seealso [get_session_meetings] [get_case] [get_question] [get_question_hour]
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' meetings0910 <- get_session_meetings("2009-2010")
#' meeting_agenda <- get_meeting_agenda(meetings0910$meeting_id[161])
#' meeting_agenda
#' }
#'  
#' @import httr2 rvest
#' 
#' @export
#' 
get_meeting_agenda <- function(meetingid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/dagsorden?moteid=", meetingid)
  
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
    resp_body_html(check_type = F, encoding = "utf-8") 
  
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

