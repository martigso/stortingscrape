#' Retreive vote decision for a specified vote
#' 
#' A function for retrieving vote decisions from a specific vote. Vote data are only available from the 2011-2012 session
#' 
#' @usage get_decision_votes(voteid = NA, good_manners = 0)
#' 
#' @param voteid Character string indicating the id of the vote to request all votes from
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



get_decision_votes <- function(voteid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/voteringsvedtak?voteringid=", voteid)
  
  tmp <- read_html(url)
  
  if(identical(html_elements(tmp, "voteringsvedtak_liste > voteringsvedtak") %>% html_text(), character()) == TRUE){
    tmp2 <- data.frame(response_date = tmp %>% html_elements("voteringsvedtak_oversikt > respons_dato_tid") %>% html_text(),
                       version = tmp %>% html_elements("voteringsvedtak_oversikt > versjon") %>% html_text(),
                       vote_id = tmp %>% html_elements("voteringsvedtak_oversikt > votering_id") %>% html_text(),
                       decision_code = NA,
                       decision_comment = NA,
                       decision_number = NA,
                       decision_reference = NA,
                       decision_text = NA)
  } else {
    tmp2 <- data.frame(response_date = tmp %>% html_elements("voteringsvedtak_oversikt > respons_dato_tid") %>% html_text(),
                       version = tmp %>% html_elements("voteringsvedtak_oversikt > versjon") %>% html_text(),
                       vote_id = tmp %>% html_elements("voteringsvedtak_oversikt > votering_id") %>% html_text(),
                       decision_code = tmp %>% html_elements("voteringsvedtak_liste > voteringsvedtak > vedtak_kode") %>% html_text(),
                       decision_comment = tmp %>% html_elements("voteringsvedtak > vedtak_kommentar") %>% html_text(),
                       decision_number = tmp %>% html_elements("voteringsvedtak > vedtak_nummer") %>% html_text(),
                       decision_reference = tmp %>% html_elements("voteringsvedtak > vedtak_referanse") %>% html_text(),
                       decision_text = tmp %>% html_elements("voteringsvedtak > vedtak_tekst") %>% html_text())
    
  }

  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

