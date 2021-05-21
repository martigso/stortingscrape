#' Get list of MPs in a given parliamentary period
#' 
#' A function for retrieving Norwegian MPs for a given parliamentary period from the parliament API
#' 
#' @usage get_parlperiod_mps(periodid = NA)
#' 
#' @param periodid Character string indicating the id of the parliamentary period to retrieve.
#' 
#' @return A data.frame with respnse date, version, date of death/birth, first and last name, id, and gender of the requested MP.
#' 
#' @family get_mp_data
#' 
#' 
#' @examples 
#' # Request one MP by id
#' get_parlperiod_mps("2005-2009")
#' 
#' # Request several MPs by id
#' ids <- c("1961-65", "1997-01", "MAAA")
#' 
#' mps <- lapply(ids, get_parlperiod_mps)
#' 
#' mps <- do.call(rbind, mps)
#' 
#' 
#' @export
#' 



get_parlperiod_mps <- function(periodid){
  
  require(rvest)
  
  tmp <- read_html(paste0("https://data.stortinget.no/eksport/representanter?stortingsperiode=", periodid))
  
  tmp <- data.frame(response_date = tmp %>% html_nodes("representanter_liste > representant > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_nodes("representanter_liste > representant > versjon") %>% html_text(),
                    death = tmp %>% html_nodes("representanter_liste > representant > doedsdato") %>% html_text(),
                    lastname = tmp %>% html_nodes("representanter_liste > representant > etternavn") %>% html_text(),
                    birth = tmp %>% html_nodes("representanter_liste > representant > foedselsdato") %>% html_text(),
                    firstname = tmp %>% html_nodes("representanter_liste > representant > fornavn") %>% html_text(),
                    id = tmp %>% html_nodes("representanter_liste > representant > id") %>% html_text(),
                    gender = tmp %>% html_nodes("representanter_liste > representant > kjoenn") %>% html_text(),
                    period_id = tmp %>% html_nodes("stortingsperiode_id") %>% html_text())
  
  return(tmp)
  
  
}