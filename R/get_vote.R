#' Retreive votes for a specific case
#' 
#' A function for retrieving all votes from a case. Vote data are only available from the 2011-2012 session
#' 
#' @usage get_vote(caseid = NA, good_manners = 0)
#' 
#' @param caseid Character string indicating the id of the case to request all votes from
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                           |                                                                          |
#'    |:--------------------------|:-------------------------------------------------------------------------|
#'    | **response_date**         | Date of data retrieval                                                   |
#'    | **version**               | Data version from the API                                                |
#'    | **case_id**               | Case id up for vote                                                      |
#'    | **alternative_vote**      | Whether vote is an alternative vote                                      |
#'    | **n_for**                 | Number of votes for                                                      |
#'    | **n_absent**              | Number of MPs absent                                                     |
#'    | **n_against**             | Number of votes against                                                  |
#'    | **treatment_order**       | Order of treated votes                                                   |
#'    | **agenda_case_number**    | Case number on the agenda of the meeting                                 |
#'    | **free_vote**             | Logical indication of whether the vote is related to the case as a whole |
#'    | **comment**               | Vote comment                                                             |
#'    | **meeting_map_number**    | Number on the meeting map                                                |
#'    | **personal_vote**         | Logical indication of whether vote was recorded as roll call or not      |
#'    | **president_id**          | Id of president holding president chair at the time of voting            |
#'    | **president_party_id**    | Party of the sitting president                                           |
#'    | **adopted**               | Logical indication of whether the proposal voted on was adopted          |
#'    | **vote_id**               | Id of vote                                                               |
#'    | **vote_method**           | Voting method                                                            |
#'    | **vote_result_type**      | Result type (enstemmig_vedtatt = unanimously adopted)                    |
#'    | **vote_result_type_text** | See __vote_result_type__                                                 |
#'    | **vote_topic**            | Description of the proposal voted upon                                   |
#'    | **vote_datetime**         | Date and time of vote                                                    |
#' 
#' @md
#' 
#' @seealso [get_decision_votes] [get_proposal_votes] [get_vote] [get_session_cases] [get_case]
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' get_vote(63033)
#' 
#' }
#' 
#'  
#' @import rvest httr
#' 
#' @export
#' 
get_vote <- function(caseid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/voteringer?sakid=", caseid)
  
  base <- GET(url)
  
  resp <- http_type(base)
  if(resp != "text/xml") stop(paste0("Response of ", url, " is not text/xml."), call. = FALSE)
  
  status <- http_status(base)
  if(status$category != "Success") stop(paste0("Response of ", url, " returned as '", status$message, "'"), call. = FALSE)
  
  tmp <- read_html(base)
  
  tmp2 <- data.frame(response_date = tmp %>% html_elements("sak_votering_oversikt > respons_dato_tid") %>% html_text(),
                     version = tmp %>% html_elements("sak_votering_oversikt > versjon") %>% html_text(),
                     case_id = tmp %>% html_elements("sak_votering_oversikt > sak_id") %>% html_text(),
                     alternative_vote = tmp %>% html_elements("sak_votering > alternativ_votering_id") %>% html_text(),
                     n_for = tmp %>% html_elements("sak_votering > antall_for") %>% html_text(),
                     n_absent = tmp %>% html_elements("sak_votering > antall_ikke_tilstede") %>% html_text(),
                     n_against = tmp %>% html_elements("sak_votering > antall_mot") %>% html_text(),
                     treatment_order = tmp %>% html_elements("sak_votering > behandlingsrekkefoelge") %>% html_text(),
                     agenda_case_number = tmp %>% html_elements("sak_votering > dagsorden_sak_nummer") %>% html_text(),
                     free_vote = tmp %>% html_elements("sak_votering > fri_votering") %>% html_text(),
                     comment = tmp %>% html_elements("sak_votering > kommentar") %>% html_text(),
                     meeting_map_number = tmp %>% html_elements("sak_votering > mote_kart_nummer") %>% html_text(),
                     personal_vote = tmp %>% html_elements("sak_votering > personlig_votering") %>% html_text(),
                     president_id = tmp %>% html_elements("sak_votering > president > id") %>% html_text(),
                     president_party_id = tmp %>% html_elements("sak_votering > president > parti > id") %>% html_text(),
                     adopted = tmp %>% html_elements("sak_votering > vedtatt") %>% html_text(),
                     vote_id = tmp %>% html_elements("sak_votering > votering_id") %>% html_text(),
                     vote_method = tmp %>% html_elements("sak_votering > votering_metode") %>% html_text(),
                     vote_result_type = tmp %>% html_elements("sak_votering > votering_resultat_type") %>% html_text(),
                     vote_result_type_text = tmp %>% html_elements("sak_votering > votering_resultat_type_tekst") %>% html_text(),
                     vote_topic = tmp %>% html_elements("sak_votering > votering_tema") %>% html_text(),
                     vote_datetime = tmp %>% html_elements("sak_votering > votering_tid") %>% html_text())
    

  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

