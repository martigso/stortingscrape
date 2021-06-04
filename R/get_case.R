#' Retreive parliamentary case
#' 
#' A function for retrieving single parliamentary case by id
#' 
#' @usage get_case(caseid = NA, good_manners = 0)
#' 
#' @param caseid Character string indicating the id of the case to request
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
#' }
#' @import rvest
#' 
#' @export
#' 



get_case <- function(caseid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/sak?sakid=", caseid)
  
  tmp <- read_html(url)
  
  tmp2 <- list(root = data.frame(response_date = tmp %>% html_elements("detaljert_sak > respons_dato_tid") %>% html_text(),
                                 version = tmp %>% html_elements("detaljert_sak > versjon") %>% html_text(),
                                 document_group = tmp %>% html_elements("detaljert_sak > dokumentgruppe") %>% html_text(),
                                 finalized = tmp %>% html_elements("ferdigbehandlet") %>% html_text(),
                                 reference = tmp %>% html_elements("henvisning") %>% html_text(), 
                                 id = tmp %>% html_elements("detaljert_sak > id") %>% html_text(),
                                 req_text = tmp %>% html_elements("innstillingstekst") %>% html_text(),
                                 committee_id = ifelse(identical(tmp %>% html_elements("komite > id") %>% html_text(), character()), "",
                                                                 tmp %>% html_elements("komite > id") %>% html_text()),
                                 title_short = tmp %>% html_elements("korttittel") %>% html_text(),
                                 decision_short = tmp %>% html_elements("kortvedtak") %>% html_text(),
                                 parenthesis_text = tmp %>% html_elements("parentestekst") %>% html_text(),
                                 case_number = tmp %>% html_elements("sak_nummer") %>% html_text(),
                                 session_id = tmp %>% html_elements("sak_sesjon") %>% html_text(),
                                 proceedings_id = tmp %>% html_elements("saksgang > id") %>% html_text(),
                                 proceedings_name = tmp %>% html_elements("saksgang > navn") %>% html_text(),
                                 status = tmp %>% html_elements("detaljert_sak > status") %>% html_text(),
                                 title = tmp %>% html_elements("detaljert_sak > tittel") %>% html_text(),
                                 type = tmp %>% html_elements("detaljert_sak > type") %>% html_text(),
                                 decision_text = tmp %>% html_elements("detaljert_sak > vedtakstekst") %>% html_text()),
               topic = data.frame(is_main_topic = tmp %>% html_elements("emne > er_hovedemne") %>% html_text(),
                                  main_topic_id = tmp %>% html_elements("emne > hovedemne_id") %>% html_text(),
                                  id = tmp %>% html_elements("emne > id") %>% html_text(),
                                  navn = tmp %>% html_elements("emne > navn") %>% html_text()),
               publication_references = data.frame(export_id = tmp %>% html_elements("publikasjon_referanse > eksport_id") %>% html_text(),
                                                   link_text = tmp %>% html_elements("publikasjon_referanse > lenke_tekst") %>% html_text(),
                                                   link_url = tmp %>% html_elements("publikasjon_referanse > lenke_url") %>% html_text(),
                                                   type = tmp %>% html_elements("publikasjon_referanse > type") %>% html_text(),
                                                   subtype = tmp %>% html_elements("publikasjon_referanse > undertype") %>% html_text()),
               proposers = data.frame(mp_id = tmp %>% html_elements("forslagstiller_liste > representant > id") %>% html_text(),
                                      party_id = tmp %>% html_elements("forslagstiller_liste > representant > parti > id") %>% html_text(),
                                      sub_mp = tmp %>% html_elements("forslagstiller_liste > representant > vara_representant") %>% html_text()),
               proceeding_steps = data.frame(step_name = tmp %>% html_elements("saksgang_steg > navn") %>% html_text(),
                                             step_number = tmp %>% html_elements("saksgang_steg > steg_nummer") %>% html_text(),
                                             outdated = tmp %>% html_elements("saksgang_steg > uaktuell") %>% html_text()),
               spokespersons = data.frame(mp_id = tmp %>% html_elements("saksordfoerer_liste > representant > id") %>% html_text(),
                                          party_id = tmp %>% html_elements("saksordfoerer_liste > representant > parti > id") %>% html_text(),
                                          sub_mp = tmp %>% html_elements("saksordfoerer_liste > representant > vara_representant") %>% html_text()),
               keywords = data.frame(keyword = tmp %>% html_elements("stikkord_liste > string") %>% html_text()))
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

