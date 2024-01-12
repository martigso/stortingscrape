#' Retrieve question hour details for a specified meeting
#' 
#' A function for retrieving detailed overview of the question hour for a specific meeting
#' 
#' @usage get_question_hour(meetingid = NA, good_manners = 0)
#' 
#' @param meetingid Character string indicating the id of the meeting to request all votes from
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A list with ten data frames:
#' 
#' 1. **$root** (download meta data)
#' 
#'    |                      |                           |
#'    |:---------------------|:--------------------------|
#'    | **response_date**    | Date of data retrieval    |
#'    | **version**          | Data version from the API |
#'    | **meetingid**        | The called meeting id     |
#'        
#' 2. **$question_hour_ministers** (id of ministers in parliament during question hour/time)
#' 
#'    |           |                           |
#'    |:----------|:--------------------------|
#'    | **id**    | Id of ministers attending |
#'    
#'    
#' 3. **$question_time**
#' 
#'    |                                    |                                                            |
#'    |:-----------------------------------|:-----------------------------------------------------------|
#'    | question_justification             | Justification for question                                 |
#'    | answer_by_id                       | Id for answering minister                                  |
#'    | answer_by_minister_id              | Id for department of answering minister                    |
#'    | answer_by_minister_title           | Title for department of answering minister                 |
#'    | answer_date                        | Date of receiving answer                                   |
#'    | answer_on_behalf_of_id             | Id of minister answered on behalf of, when relevant        |
#'    | answer_on_behalf_of_minister_id    | Id of department answered on behalf of, when relevant      |
#'    | answer_on_behalf_of_minister_title | Title of department answered on behalf of, when relevant   |
#'    | agenda_case_number                 | Case number on agenda                                      |
#'    | date                               | Date question hour was held                                |
#'    | moved_to                           | Date moved to                                              |
#'    | asked_by_other_id                  | Id for minister asking on behalf of another, when relevant |
#'    | question_id                        | Question id                                                |
#'    | correct_person                     | Not documented in API                                      |
#'    | correct_person_minister_id         | Not documented in API                                      |
#'    | correct_person_minister_title      | Not documented in API                                      |
#'    | sent_date                          | Date question was sent                                     |
#'    | session_id                         | Session id                                                 |
#'    | question_text                      | Full question text                                         |
#'    | question_from_id                   | Id of MP asking the question                               |
#'    | question_number                    | Question number                                            |
#'    | question_to_id                     | Id of minister the question was asked to                   |
#'    | question_to_minister_id            | Department id of minister the question was asked to        |
#'    | question_to_minister_title         | Department title of minister the question was asked to     |
#'    | status                             | Question status                                            |
#'    | answer                             | Answer text (often empty)                                  |
#'    | title                              | Question title                                             |
#'    | type                               | Question type                                              |
#'    
#' 4. **$publication_reference**
#' 
#'    |               |                                                    |
#'    |:--------------|:---------------------------------------------------|
#'    | **export_id** | Export id for publication (see get_publication())  |
#'    | **link_text** | Description text for publication                   |
#'    | **link_url**  | URL for publication                                |
#'    | **type**      | Type of publication                                |
#'    | **sub_type**  | Subtype for publication (location)                 |
#'
#' @md
#' 
#' @seealso [get_question] [get_session_questions] [get_publication]
#' 
#' @examples 
#' 
#' \dontrun{
#' get_question_hour(10232)
#' }
#' 
#'  
#' @import rvest httr2
#' 
#' @export
#' 
get_question_hour <- function(meetingid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/sporretime?moteid=", meetingid)
  
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
  
  tmp2 <- list(
    root = data.frame(
      response_date = tmp %>% html_elements("sporretime_oversikt > respons_dato_tid") %>% html_text(),
      version = tmp %>% html_elements("sporretime_oversikt > versjon") %>% html_text(),
      meetingid = meetingid
    ),
    question_hour_ministers = data.frame(
      id = tmp %>% html_elements("muntlig_sporretime > statsraader > person > id") %>% html_text()
    ),
    question_time = data.frame(
      question_justification = tmp %>% html_elements("ordinaer_sporretime_sporsmal_liste > detaljert_sporsmal > begrunnelse") %>% html_text(),
      answer_by_id = tmp %>% html_elements("detaljert_sporsmal > besvart_av > id") %>% html_text(),
      answer_by_minister_id = tmp %>% html_elements("detaljert_sporsmal > besvart_av_minister_id") %>% html_text(),
      answer_by_minister_title = tmp %>% html_elements("detaljert_sporsmal > besvart_av_minister_tittel") %>% html_text(),
      answer_date = tmp %>% html_elements("detaljert_sporsmal > besvart_dato") %>% html_text(),
      answer_on_behalf_of_id = sapply(tmp %>% html_elements("detaljert_sporsmal > besvart_pa_vegne_av"), 
                                      function(x) x %>% html_elements("id") %>% html_text() %>% paste0("_h")),
      answer_on_behalf_of_minister_id = tmp %>% html_elements("detaljert_sporsmal > besvart_pa_vegne_av_minister_id") %>% html_text(),
      answer_on_behalf_of_minister_title = tmp %>% html_elements("detaljert_sporsmal > besvart_pa_vegne_av_minister_tittel") %>% html_text(),
      agenda_case_number = tmp %>% html_elements("detaljert_sporsmal > dagsorden_saknummer") %>% html_text(),
      date = tmp %>% html_elements("detaljert_sporsmal > datert_dato") %>% html_text(),
      moved_to = tmp %>% html_elements("detaljert_sporsmal > flyttet_til") %>% html_text(),
      asked_by_other_id = sapply(tmp %>% html_elements("detaljert_sporsmal > fremsatt_av_annen"),
                                 function(x) x %>% html_elements("id") %>% html_text() %>% paste0("_h")),
      question_id = tmp %>% html_elements("detaljert_sporsmal > id") %>% html_text(),
      correct_person = tmp %>% html_elements("detaljert_sporsmal > rette_vedkommende") %>% html_text(),
      correct_person_minister_id = tmp %>% html_elements("detaljert_sporsmal > rette_vedkommende_minister_id") %>% html_text(),
      correct_person_minister_title = tmp %>% html_elements("detaljert_sporsmal > rette_vedkommende_minister_tittel") %>% html_text(),
      sent_date = tmp %>% html_elements("detaljert_sporsmal > sendt_dato") %>% html_text(),
      session_id = tmp %>% html_elements("detaljert_sporsmal > sesjon_id") %>% html_text(),
      question_text = tmp %>% html_elements("detaljert_sporsmal > sporsmal") %>% html_text(),
      question_from_id = tmp %>% html_elements("detaljert_sporsmal > sporsmal_fra > id") %>% html_text(),
      question_number = tmp %>% html_elements("detaljert_sporsmal > sporsmal_nummer") %>% html_text(),
      question_to_id = tmp %>% html_elements("detaljert_sporsmal > sporsmal_til > id") %>% html_text(),
      question_to_minister_id = tmp %>% html_elements("detaljert_sporsmal > sporsmal_til_minister_id") %>% html_text(),
      question_to_minister_title = tmp %>% html_elements("detaljert_sporsmal > sporsmal_til_minister_tittel") %>% html_text(),
      status = tmp %>% html_elements("detaljert_sporsmal > status") %>% html_text(),
      answer = tmp %>% html_elements("detaljert_sporsmal > svar") %>% html_text(),
      title = tmp %>% html_elements("detaljert_sporsmal > tittel") %>% html_text(),
      type = tmp %>% html_elements("detaljert_sporsmal > type") %>% html_text()
    ),
    publication_reference = data.frame(
      export_id = tmp %>% html_elements("publikasjon_referanse > eksport_id") %>% html_text(),
      link_text = tmp %>% html_elements("publikasjon_referanse > lenke_tekst") %>% html_text(),
      link_url = tmp %>% html_elements("publikasjon_referanse > lenke_url") %>% html_text(),
      type = tmp %>% html_elements("publikasjon_referanse > type") %>% html_text(),
      sub_type = tmp %>% html_elements("publikasjon_referanse > undertype") %>% html_text()
    )
  )
  
  tmp2$question_time$answer_on_behalf_of_id <- gsub("_h", "", tmp2$question_time$answer_on_behalf_of_id)
  tmp2$question_time$asked_by_other_id <- gsub("_h", "", tmp2$question_time$asked_by_other_id)
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

