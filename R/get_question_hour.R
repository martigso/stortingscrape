#' Retreive question hour details for a specified meeting
#' 
#' A function for retrieving detailed overview of the question hour for a specific meeting
#' 
#' @usage get_question_hour(meetingid = NA, good_manners = 0)
#' 
#' @param meetingid Character string indicating the id of the meeting to request all votes from
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A list with response date, version ...
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



get_question_hour <- function(meetingid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/sporretime?moteid=", meetingid)
  
  tmp <- read_html(url)
  
  tmp2 <- list(root = data.frame(
    response_date = tmp %>% html_elements("sporretime_oversikt > respons_dato_tid") %>% html_text(),
    version = tmp %>% html_elements("sporretime_oversikt > versjon") %>% html_text()
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
                            function(x) x %>% html_element("id") %>% html_text() %>% paste("_h")),
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

