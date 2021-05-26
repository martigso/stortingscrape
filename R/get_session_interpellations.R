#' Get list of topics and sub-topics for the Norwegian parliament
#' 
#' A function for retrieving topic keys used to label various data from the Norwegian parliament.
#' 
#' @usage get_session_interpellations(sessionid = NA, status = NA, good_manners = 0)
#' 
#' @param sessionid Character string indicating the id of the session to request interpellations from
#' @param status Character string question status extraction. Possible values are NA (extract all questions), "til_behandling" (pending questions), 
#' "trukket" (withdrawn questions), "bortfalt" (lapsed questions), or "alle" (all questions)
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with response date, version ...
#' 
#' @family get_mp_data
#' 
#' @examples 
#' interp <- get_session_interpellations(sessionid = "2013-2014", status = "trukket")
#' 
#' @import rvest
#' 
#' @export
#' 



get_session_interpellations <- function(sessionid = NA, status = NA, good_manners = 0){
  
  if(is.na(status) == TRUE){
    
    url <- paste0("https://data.stortinget.no/eksport/interpellasjoner?sesjonid=", sessionid)  
    
  } else if(status %in% c("til_behandling", "trukket", "bortfalt", "alle") == FALSE){
    
    stop('Status needs to be one of "til_behandling", "trukket", "bortfalt", or "alle"')
    
  } else {
    
    url <- paste0("https://data.stortinget.no/eksport/interpellasjoner?sesjonid=", sessionid, "&status=", status)
    
  }
  
  message(paste("Downloading data for", sessionid))
  tmp <- read_html(url)
  
  message(paste("Structuring data for", sessionid))
  tmp2 <- data.frame(
    response_date = tmp %>% html_elements("sporsmal_liste > sporsmal > respons_dato_tid") %>% html_text(),
    version = tmp %>% html_elements("sporsmal_liste > sporsmal > versjon") %>% html_text(),
    answ_by_id = tmp %>% html_elements("sporsmal_liste > sporsmal > besvart_av > fornavn") %>% html_text(),
    answ_by_minister_id = tmp %>% html_elements("sporsmal_liste > sporsmal > besvart_av_minister_id") %>% html_text(),
    answ_by_minister_title = tmp %>% html_elements("sporsmal_liste > sporsmal > besvart_av_minister_tittel") %>% html_text(),
    answ_date = tmp %>% html_elements("sporsmal_liste > sporsmal > besvart_dato") %>% html_text(),
    answ_on_belhalf_of = tmp %>% html_elements("sporsmal_liste > sporsmal > besvart_pa_vegne_av") %>% html_text(),
    answ_on_belhalf_of_minister_id = tmp %>% html_elements("sporsmal_liste > sporsmal > besvart_pa_vegne_av_minister_id") %>% html_text(),
    answ_on_belhalf_of_minister_title = tmp %>% html_elements("sporsmal_liste > sporsmal > besvart_pa_vegne_av_minister_tittel") %>% html_text(),
    moved_to = tmp %>% html_elements("sporsmal_liste > sporsmal > flyttet_til") %>% html_text(),
    id = tmp %>% html_elements("sporsmal_liste > sporsmal > id") %>% html_text(),
    correct_person_minister_id = tmp %>% html_elements("sporsmal_liste > sporsmal > rette_vedkommende_minister_id") %>% html_text(),
    correct_person_minister_title = tmp %>% html_elements("sporsmal_liste > sporsmal > rette_vedkommende_minister_tittel") %>% html_text(),
    sendt_date = tmp %>% html_elements("sporsmal_liste > sporsmal > sendt_dato") %>% html_text(),
    session_id = tmp %>% html_elements("sporsmal_liste > sporsmal > sesjon_id") %>% html_text(),
    question_from_id = tmp %>% html_elements("sporsmal_liste > sporsmal > sporsmal_fra > id") %>% html_text(), # get more info with get_mp()
    qustion_number = tmp %>% html_elements("sporsmal_liste > sporsmal > sporsmal_nummer") %>% html_text(),
    qustion_to_id = tmp %>% html_elements("sporsmal_liste > sporsmal > sporsmal_til > id") %>% html_text(),
    qustion_to_minister_id = tmp %>% html_elements("sporsmal_liste > sporsmal > sporsmal_til_minister_id") %>% html_text(),
    qustion_to_minister_title = tmp %>% html_elements("sporsmal_liste > sporsmal > sporsmal_til_minister_tittel") %>% html_text(),
    status = tmp %>% html_elements("sporsmal_liste > sporsmal > status") %>% html_text(),
    title = tmp %>% html_elements("sporsmal_liste > sporsmal > tittel") %>% html_text(),
    type = tmp %>% html_elements("sporsmal_liste > sporsmal > type") %>% html_text()
  )
  
  tmp2$topic_ids <- sapply((tmp %>% html_elements("sporsmal_liste > sporsmal > emne_liste")), function(x){
    paste((x %>% html_elements("id") %>% html_text()), collapse = "/")
  })
  
  tmp2$asked_by_other_id <- sapply((tmp %>% html_elements("sporsmal_liste > sporsmal > fremsatt_av_annen")), function(x){
    x %>% html_element("id") %>% html_text()
  })
  
  tmp2$correct_person <- sapply((tmp %>% html_elements("sporsmal_liste > sporsmal > rette_vedkommende")), function(x){
    tmp3 <- x %>% html_elements("id") %>% html_text()
    tmp3 <- ifelse(identical(character(), tmp3), NA, tmp3)
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
    "qustion_number",
    "qustion_to_id",
    "qustion_to_minister_id",
    "qustion_to_minister_title",
    "status",
    "title",
    "type")]
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

