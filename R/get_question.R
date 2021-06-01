#' Parliamentary question
#' 
#' A function for retrieving single parliamentary questions by id
#' 
#' @usage get_question(questionid = NA, good_manners = 0)
#' 
#' @param questionid Character string indicating the id of the session to request interpellations from
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with response date, version ...
#' 
#' @family get_mp_data
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
#' @import rvest
#' 
#' @export
#' 



get_question <- function(questionid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/enkeltsporsmal?sporsmalid=", questionid)
  
  tmp <- read_html(url)
  
  tmp2 <- data.frame(
    response_date = tmp %>% html_elements("besvart_av > respons_dato_tid") %>% html_text(),
    version = tmp %>% html_elements("besvart_av > versjon") %>% html_text(),
    justification = tmp %>% html_elements("begrunnelse") %>% html_text(),
    answ_by_id = tmp %>% html_elements("besvart_av > id") %>% html_text(),
    answ_by_minister_id = tmp %>% html_elements("besvart_av_minister_id") %>% html_text(),
    answ_by_minister_title = tmp %>% html_elements("besvart_av_minister_tittel") %>% html_text(),
    answ_date = tmp %>% html_elements("besvart_dato") %>% html_text(),
    answ_on_belhalf_of = tmp %>% html_elements("besvart_pa_vegne_av") %>% html_text(),
    answ_on_belhalf_of_minister_id = tmp %>% html_elements("besvart_pa_vegne_av_minister_id") %>% html_text(),
    answ_on_belhalf_of_minister_title = tmp %>% html_elements("besvart_pa_vegne_av_minister_tittel") %>% html_text(),
    agenda_number = tmp %>% html_elements("dagsorden_saknummer") %>% html_text(),
    moved_to = tmp %>% html_elements("flyttet_til") %>% html_text(),
    id = tmp %>% html_elements("detaljert_sporsmal > id") %>% html_text(),
    correct_person_id = ifelse(identical((tmp %>% html_elements("rette_vedkommende > id") %>% html_text()), character()), NA,
                               tmp %>% html_elements("rette_vedkommende > id") %>% html_text()),
    correct_person_minister_id = tmp %>% html_elements("rette_vedkommende_minister_id") %>% html_text(),
    correct_person_minister_title = tmp %>% html_elements("rette_vedkommende_minister_tittel") %>% html_text(),
    sendt_date = tmp %>% html_elements("sendt_dato") %>% html_text(),
    session_id = tmp %>% html_elements("sesjon_id") %>% html_text(),
    question_text = tmp %>% html_elements("sporsmal") %>% html_text(),
    question_from_id = tmp %>% html_elements("sporsmal_fra > id") %>% html_text(), # get more info with get_mp()
    qustion_number = tmp %>% html_elements("sporsmal_nummer") %>% html_text(),
    qustion_to_id = tmp %>% html_elements("sporsmal_til > id") %>% html_text(),
    qustion_to_minister_id = tmp %>% html_elements("sporsmal_til_minister_id") %>% html_text(),
    qustion_to_minister_title = tmp %>% html_elements("sporsmal_til_minister_tittel") %>% html_text(),
    status = tmp %>% html_elements("status") %>% html_text(),
    answer_text = tmp %>% html_elements("svar") %>% html_text(),
    title = tmp %>% html_elements("tittel") %>% html_text(),
    type = tmp %>% html_elements("type") %>% html_text()
  )
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

