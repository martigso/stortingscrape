#' Parliamentary question
#' 
#' A function for retrieving single parliamentary questions by id. For retrieving the whole debate
#' over a question, the [get_publication] function has to be used
#' 
#' @usage get_question(questionid = NA, good_manners = 0)
#' 
#' @param questionid Character string indicating the id of the session to request interpellations from
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                                       |                                                                               |
#'    |:--------------------------------------|:------------------------------------------------------------------------------|
#'    | **response_date**                     | Date of data retrieval                                                        |
#'    | **version**                           | Data version from the API                                                     |
#'    | **justification**                     | Justification for question                                                    |  
#'    | **answ_by_id**                        | Id for answering minister                                                     |
#'    | **answ_by_minister_id**               | Id for department of answering minister                                       |
#'    | **answ_by_minister_title**            | Title for department of answering minister                                    |
#'    | **answ_date**                         | Date question was asked                                                       |
#'    | **answ_on_belhalf_of**                | Id of minister answered on behalf of, when relevant                           |
#'    | **answ_on_belhalf_of_minister_id**    | Id of department answered on behalf of, when relevant                         |
#'    | **answ_on_belhalf_of_minister_title** | Title of department answered on behalf of, when relevant                      |
#'    | **agenda_number**                     | Agenda number in meeting                                                      |
#'    | **moved_to**                          | Date moved to                                                                 |
#'    | **id**                                | Question id                                                                   |
#'    | **legacy_id**                         | Question legacy id (only use to match with data downloaded before 10.12.2024) |
#'    | **correct_person_id**                 | Not documented in API                                                         |
#'    | **correct_person_minister_id**        | Not documented in API                                                         |
#'    | **correct_person_minister_title**     | Not documented in API                                                         |
#'    | **sendt_date**                        | Date question was sent                                                        |
#'    | **session_id**                        | Session id                                                                    |
#'    | **question_text**                     | Full question text                                                            |
#'    | **question_from_id**                  | Id of MP asking the question                                                  |
#'    | **qustion_number**                    | Question number                                                               |
#'    | **qustion_to_id**                     | Id of minister the question was asked to                                      |
#'    | **qustion_to_minister_id**            | Department id of minister the question was asked to                           |
#'    | **qustion_to_minister_title**         | Department title of minister the question was asked to                        |
#'    | **answer_text**                       | Answer text (often empty)                                                     |
#'    | **title**                             | Question title                                                                |
#'    | **type**                              | Question type                                                                 |
#'    | **asked_by_other_id**                 | ID of person that asked the question on behalf of `question_from_id`          |
#'    
#' @md
#' 
#' @seealso [get_question_hour] [get_publication] [get_meeting_agenda]
#' 
#' @examples 
#' 
#' \dontrun{
#' # An example of a possible workflow
#' 
#' ## Retreive sessions
#' sessions <- get_parlsessions()
#' 
#' ## Retreive all interpellations for a particular session
#' qsesh <- get_session_questions(sessions$id[9], q_type = "interpellasjoner")
#' 
#' ## Retreve detailed information on all interpellations in that session
#' library(pbmcapply) # for progress bar. never use paralell on scraping
#' int1213 <- pbmclapply(qsesh$id, function(x){
#'     get_question(x, good_manners = 2)
#' }, mc.cores = 1)
#' 
#' quest1213 <- do.call(rbind, int1213)
#' }
#' 
#' @import rvest httr2
#' 
#' @export
#' 
get_question <- function(questionid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/enkeltsporsmal?NSporsmalId=", questionid)
  
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
  
  if(identical((tmp |> html_elements("rette_vedkommende > id") |> html_text()), character())) {
    correct_person_id <- NA
  } else {
    correct_person_id <- tmp |> html_elements("rette_vedkommende > id") |> html_text()
  }
  
  status <- tmp |> html_elements("status") |> html_text()
  
  tmp2 <- data.frame(
    justification = tmp |> html_elements("begrunnelse") |> html_text(),
    answ_by_id = ifelse(status != "besvart", NA, tmp |> html_elements("besvart_av > id") |> html_text()),
    answ_by_minister_id = ifelse(status != "besvart", NA, tmp |> html_elements("besvart_av_minister_id") |> html_text()),
    answ_by_minister_title = ifelse(status != "besvart", NA, tmp |> html_elements("besvart_av_minister_tittel") |> html_text()),
    answ_date = tmp |> html_elements("besvart_dato") |> html_text(),
    answ_on_belhalf_of = tmp |> html_elements("besvart_pa_vegne_av") |> html_text(),
    answ_on_belhalf_of_minister_id = tmp |> html_elements("besvart_pa_vegne_av_minister_id") |> html_text(),
    answ_on_belhalf_of_minister_title = tmp |> html_elements("besvart_pa_vegne_av_minister_tittel") |> html_text(),
    agenda_number = tmp |> html_elements("dagsorden_saknummer") |> html_text(),
    moved_to = tmp |> html_elements("flyttet_til") |> html_text(),
    id = tmp |> html_elements("detaljert_sporsmal > id") |> html_text(),
    legacy_id = tmp |> html_elements("detaljert_sporsmal > legacy_id") |> html_text(),
    correct_person_id,
    correct_person_minister_id = tmp |> html_elements("rette_vedkommende_minister_id") |> html_text(),
    correct_person_minister_title = tmp |> html_elements("rette_vedkommende_minister_tittel") |> html_text(),
    sendt_date = tmp |> html_elements("sendt_dato") |> html_text(),
    session_id = tmp |> html_elements("sesjon_id") |> html_text(),
    question_text = tmp |> html_elements("sporsmal") |> html_text(),
    response_date = tmp |> html_elements("sporsmal_fra > respons_dato_tid") |> html_text(),
    version = tmp |> html_elements("sporsmal_fra > versjon") |> html_text(),
    question_from_id = tmp |> html_elements("sporsmal_fra > id") |> html_text(), # get more info with get_mp()
    question_from_county_id = tmp |> html_elements("sporsmal_fra > fylke > id") |> html_text(),
    question_from_party_id = tmp |> html_elements("sporsmal_fra > parti > id") |> html_text(),
    question_from_deputy = tmp |> html_elements("sporsmal_fra > vara_representant") |> html_text(),
    qustion_number = tmp |> html_elements("sporsmal_nummer") |> html_text(),
    qustion_to_id = tmp |> html_elements("sporsmal_til > id") |> html_text(),
    qustion_to_minister_id = tmp |> html_elements("sporsmal_til_minister_id") |> html_text(),
    qustion_to_minister_title = tmp |> html_elements("sporsmal_til_minister_tittel") |> html_text(),
    status = status,
    answer_text = tmp |> html_elements("svar") |> html_text(),
    title = tmp |> html_elements("tittel") |> html_text(),
    type = tmp |> html_elements("type") |> html_text()
  )
  
  tmp2$asked_by_other_id <- sapply((tmp |> html_elements("fremsatt_av_annen")), function(x){
    x |> html_element("id") |> html_text()
  })
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

