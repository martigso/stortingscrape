#' All parliamentary parties
#' 
#' A function for retrieving all Norwegian parliamentary parties after WWII
#' 
#' @usage get_all_parties()
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
#' 
#' 
#' @import rvest
#' @export
#' 



get_all_parties <- function(){
  
  url <- "https://data.stortinget.no/eksport/allepartier"
  
  tmp <- read_html(url)
  
  tmp <- data.frame(response_date = tmp %>% html_elements("partier_liste > parti > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_elements("partier_liste > parti > versjon") %>% html_text(),
                    id = tmp %>% html_elements("partier_liste > parti > id") %>% html_text(),
                    name = tmp %>% html_elements("partier_liste > parti > navn") %>% html_text(),
                    represented_party = tmp %>% html_elements("partier_liste > parti > representert_parti") %>% html_text(),
                    sessionid = tmp %>% html_elements("sesjon_id") %>% html_text(),
                    period_id = tmp %>% html_elements("stortingsperiode_id") %>% html_text())
  
  return(tmp)
  
}


