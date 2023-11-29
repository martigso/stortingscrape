#' Parliamentary questions in a session
#' 
#' A function for retrieving all questions within a parliamentary session.
#' 
#' @usage get_session_questions(sessionid = NA, q_type = NA, status = NA, good_manners = 0)
#' 
#' @param sessionid Character string indicating the id of the session to request interpellations from
#' @param q_type Character string indicating type of question to retrieve. 
#' Options are "interpellasjoner" (interpellations), "sporretimesporsmal" (oral questions), or "skriftligesporsmal" (written questions).
#' @param status Character string question status extraction. Possible values are NA (extract all questions), "til_behandling" (pending questions), 
#' "trukket" (withdrawn questions), "bortfalt" (lapsed questions), or "alle" (all questions)
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                                       |                                                        |
#'    |:--------------------------------------|:-------------------------------------------------------|
#'    | **response_date**                     | Date of data retrieval                                 |
#'    | **version**                           | Data version from the API                              |
#'    | **answ_by_id**                        | Id of minister answering question                      |
#'    | **answ_by_minister_id**               | Department id of answering minister                    |
#'    | **answ_by_minister_title**            | Department title of answering minister                 |
#'    | **answ_date**                         | Date answer was given                                  |
#'    | **answ_on_belhalf_of**                | Answer given on behalf of                              |
#'    | **answ_on_belhalf_of_minister_id**    | Department id of minister given answer on behalf of    |
#'    | **answ_on_belhalf_of_minister_title** | Department title of minister given answer on behalf of |
#'    | **topic_ids**                         | Id of relevant topics for question                     |
#'    | **moved_to**                          | Question moved to                                      |
#'    | **asked_by_other_id**                 | MP id, if question was not asked by the questioning MP |
#'    | **id**                                | Question id                                            |
#'    | **correct_person**                    | Not documented in API                                  |
#'    | **correct_person_minister_id**        | Not documented in API                                  |
#'    | **correct_person_minister_title**     | Not documented in API                                  |
#'    | **sendt_date**                        | Date the question was sent                             |
#'    | **session_id**                        | Session id                                             |
#'    | **question_from_id**                  | Question from MP id                                    |
#'    | **question_number**                   | Question number within session                         |
#'    | **question_to_id**                    | Question directed to minister id                       |
#'    | **question_to_minister_id**           | Question directed to minister department id            |
#'    | **question_to_minister_title**        | Question directed to minister department title         |
#'    | **status**                            | Question status                                        |
#'    | **title**                             | Question title                                         |
#'    | **type**                              | Question type                                          |
#'    
#' @md
#' 
#' @seealso [get_question_hour] [get_question]
#' 
#' @examples 
#' 
#' \dontrun{
#' interp <- get_session_questions(sessionid = "2013-2014", 
#'                                 q_type = "interpellasjoner", 
#'                                 status = "trukket")
#' interp
#' }
#' @import rvest httr
#' 
#' @export
#' 
get_session_questions <- function(sessionid = NA, 
                                  q_type = NA, 
                                  status = NA, 
                                  good_manners = 0){
  
  if(is.na(status) == TRUE){
    
    url <- paste0("https://data.stortinget.no/eksport/", 
                  q_type, 
                  "?sesjonid=", 
                  sessionid)  
    
    base <- GET(url)
    
    resp <- http_type(base)
    if(resp != "text/xml") stop(paste0("Response of ", url, " is not text/xml."), 
                                call. = FALSE)
    
    httpstat <- http_status(base)
    if(httpstat$category != "Success") stop(paste0("Response of ", 
                                                   url, 
                                                   " returned as '", 
                                                   httpstat$message, "'"), 
                                            call. = FALSE)
    
  
  } else if(status %in% c("til_behandling", "trukket", "bortfalt", "alle") == FALSE){
    
    stop('Status needs to be one of "til_behandling", "trukket", "bortfalt", or "alle"')
    
  } else {
    
    url <- paste0("https://data.stortinget.no/eksport/", q_type, "?sesjonid=", sessionid, "&status=", status)
    base <- GET(url)
    
    resp <- http_type(base)
    if(resp != "text/xml") stop(paste0("Response of ", url, " is not text/xml."), call. = FALSE)
    
    httpstat <- http_status(base)
    if(httpstat$category != "Success") stop(paste0("Response of ", url, " returned as '", httpstat$message, "'"), call. = FALSE)
    
  }
  
  message(paste("Downloading data for", sessionid))
  tmp <- read_html(base)
  
  message(paste("Structuring data for", sessionid))
  tmp2 <- data.frame(
    response_date                     = tmp %>% html_elements("sporsmal_liste > sporsmal > respons_dato_tid") %>% html_text(),
    version                           = tmp %>% html_elements("sporsmal_liste > sporsmal > versjon") %>% html_text(),
    answ_by_minister_id               = tmp %>% html_elements("sporsmal_liste > sporsmal > besvart_av_minister_id") %>% html_text(),
    answ_by_minister_title            = tmp %>% html_elements("sporsmal_liste > sporsmal > besvart_av_minister_tittel") %>% html_text(),
    answ_date                         = tmp %>% html_elements("sporsmal_liste > sporsmal > besvart_dato") %>% html_text(),
    answ_on_belhalf_of                = tmp %>% html_elements("sporsmal_liste > sporsmal > besvart_pa_vegne_av") %>% html_text(),
    answ_on_belhalf_of_minister_id    = tmp %>% html_elements("sporsmal_liste > sporsmal > besvart_pa_vegne_av_minister_id") %>% html_text(),
    answ_on_belhalf_of_minister_title = tmp %>% html_elements("sporsmal_liste > sporsmal > besvart_pa_vegne_av_minister_tittel") %>% html_text(),
    moved_to                          = tmp %>% html_elements("sporsmal_liste > sporsmal > flyttet_til") %>% html_text(),
    id                                = tmp %>% html_elements("sporsmal_liste > sporsmal > id") %>% html_text(),
    correct_person_minister_id        = tmp %>% html_elements("sporsmal_liste > sporsmal > rette_vedkommende_minister_id") %>% html_text(),
    correct_person_minister_title     = tmp %>% html_elements("sporsmal_liste > sporsmal > rette_vedkommende_minister_tittel") %>% html_text(),
    sendt_date                        = tmp %>% html_elements("sporsmal_liste > sporsmal > sendt_dato") %>% html_text(),
    session_id                        = tmp %>% html_elements("sporsmal_liste > sporsmal > sesjon_id") %>% html_text(),
    question_from_id                  = tmp %>% html_elements("sporsmal_liste > sporsmal > sporsmal_fra > id") %>% html_text(), # get more info with get_mp()
    question_number                   = tmp %>% html_elements("sporsmal_liste > sporsmal > sporsmal_nummer") %>% html_text(),
    # question_to_id                    = tmp %>% html_elements("sporsmal_liste > sporsmal > sporsmal_til > id") %>% html_text(),
    question_to_minister_id           = tmp %>% html_elements("sporsmal_liste > sporsmal > sporsmal_til_minister_id") %>% html_text(),
    question_to_minister_title        = tmp %>% html_elements("sporsmal_liste > sporsmal > sporsmal_til_minister_tittel") %>% html_text(),
    status                            = tmp %>% html_elements("sporsmal_liste > sporsmal > status") %>% html_text(),
    title                             = tmp %>% html_elements("sporsmal_liste > sporsmal > tittel") %>% html_text(),
    type                              = tmp %>% html_elements("sporsmal_liste > sporsmal > type") %>% html_text()
  )
  
  tmp2$answ_by_id <- sapply((tmp %>% html_elements("sporsmal_liste > sporsmal > besvart_av")), function(x){
    tmp3 <- x %>% html_elements("id") %>% html_text()
    if(identical(character(), tmp3)) {
      tmp3 <- NA
    }
    tmp3
  })
  
  tmp2$question_to_id <- sapply((tmp %>% html_elements("sporsmal_liste > sporsmal > sporsmal_til")), function(x){
    tmp3 <- x %>% html_elements("id") %>% html_text()
    if(identical(character(), tmp3)) {
      tmp3 <- NA
    }
    tmp3
  })
  
  
  tmp2$topic_ids <- sapply((tmp %>% html_elements("sporsmal_liste > sporsmal > emne_liste")), function(x){
    paste((x %>% html_elements("id") %>% html_text()), collapse = "/")
  })
  
  tmp2$asked_by_other_id <- sapply((tmp %>% html_elements("sporsmal_liste > sporsmal > fremsatt_av_annen")), function(x){
    x %>% html_element("id") %>% html_text()
  })
  
  tmp2$correct_person <- sapply((tmp %>% html_elements("sporsmal_liste > sporsmal > rette_vedkommende")), function(x){
    
    tmp3 <- x %>% html_elements("id") %>% html_text()
    
    if(identical(character(), tmp3)) {
      tmp3 <- NA
    }
    
    tmp3
  })
  
  
  tmp2 <- tmp2[, c(
    "response_date",
    "version",
    "answ_by_id",
    "answ_by_minister_id",
    "answ_by_minister_title",
    "answ_date",
    "answ_on_belhalf_of",
    "answ_on_belhalf_of_minister_id",
    "answ_on_belhalf_of_minister_title",
    "topic_ids",
    "moved_to",
    "asked_by_other_id",
    "id",
    "correct_person",
    "correct_person_minister_id",
    "correct_person_minister_title",
    "sendt_date",
    "session_id",
    "question_from_id",
    "question_number",
    "question_to_id",
    "question_to_minister_id",
    "question_to_minister_title",
    "status",
    "title",
    "type")]
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}
