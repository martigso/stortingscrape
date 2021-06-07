#' Retreive votes for a specific case
#' 
#' A function for retrieving all votes from a case. Vote data are only available from the 2011-2012 session
#' 
#' @usage get_vote(caseid = NA, good_manners = 0)
#' 
#' @param caseid Character string indicating the id of the case to request all votes from
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with response date, version ...
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



get_vote <- function(caseid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/voteringer?sakid=", caseid)
  
  tmp <- read_html(url)
  
  if(identical(tmp %>% html_elements("sak_votering") %>% html_text(), character()) == FALSE){
    
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
    
  } else {
    
    tmp2 <- data.frame(response_date = tmp %>% html_elements("sak_votering_oversikt > respons_dato_tid") %>% html_text(),
                       version = tmp %>% html_elements("sak_votering_oversikt > versjon") %>% html_text(),
                       case_id = tmp %>% html_elements("sak_votering_oversikt > sak_id") %>% html_text(),
                       alternative_vote = NA,
                       n_for = NA,
                       n_absent = NA,
                       n_against = NA,
                       treatment_order = NA,
                       agenda_case_number = NA,
                       free_vote = NA,
                       comment = NA,
                       meeting_map_number = NA,
                       personal_vote = NA,
                       president_id = NA,
                       president_party_id = NA,
                       adopted = NA,
                       vote_id = NA,
                       vote_method = NA,
                       vote_result_type = NA,
                       vote_result_type_text = NA,
                       vote_topic = NA,
                       vote_datetime = NA)
    
  }
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

