#' Retrieve publications of a type in a parliamentary session
#' 
#' A function for retrieving one of several publication types within a parliamentary session
#' 
#' @usage get_session_publications(sessionid = NA, type = "referat", good_manners = 0)
#' 
#' @param sessionid Character string indicating the id of the hearing to retrieve.
#' @param type Character specifying type of publication to download. Available types are "referat" (minutes), 
#' "innstilling" (proposition), "innberetning" (report), "lovvedtak" (law decision), "lovanmerkning" (law note),
#' "dok8" (MP proposal) "dok12" (Constitutional proposal), and "dokumentserie" (document series). 
#' Defaults to "referat".
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with response date, version ...
#' 
#' @family get_mp_data
#' 
#' 
#' @examples 
#' 
#' 
#' 
#' @import rvest 
#' @export
#' 



get_session_publications <- function(sessionid = NA, type = "referat", good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/publikasjoner?publikasjontype=", type, "&sesjonid=", sessionid)
  
  tmp <- read_html(url)
  
  if(identical(tmp %>% html_elements("publikasjon > id") %>% html_text(), character())){
    message(paste0("No '", type, "' in ", sessionid, ". Returning NA data frame"))
    tmp2 <- data.frame(response_date = tmp %>% html_elements("publikasjoner_oversikt > respons_dato_tid") %>% html_text(),
                       version = tmp %>% html_elements("publikasjoner_oversikt > versjon") %>% html_text(),
                       session_id = tmp %>% html_elements("publikasjoner_oversikt > sesjon_id") %>% html_text(),
                       publication_date = NA,
                       publication_id = NA,
                       publication_format = NA,
                       publication_available_date = NA,
                       publication_title = NA,
                       publication_type = NA)
    
  } else {
    tmp2 <- data.frame(response_date = tmp %>% html_elements("publikasjoner_oversikt > respons_dato_tid") %>% html_text(),
                       version = tmp %>% html_elements("publikasjoner_oversikt > versjon") %>% html_text(),
                       session_id = tmp %>% html_elements("publikasjoner_oversikt > sesjon_id") %>% html_text(),
                       publication_date = tmp %>% html_elements("publikasjon > dato") %>% html_text(),
                       publication_id = tmp %>% html_elements("publikasjon > id") %>% html_text(),
                       publication_format = sapply(tmp %>% html_elements("publikasjon > publikasjonformat_liste"), function(x){
                         paste0(x %>% html_elements("string") %>% html_text(), collapse = "//")
                       }),
                       publication_available_date = tmp %>% html_elements("publikasjon > tilgjengelig_dato") %>% html_text(),
                       publication_title = tmp %>% html_elements("publikasjon > tittel") %>% html_text(),
                       publication_type = tmp %>% html_elements("publikasjon > type") %>% html_text())
    
  }
  
  
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}



