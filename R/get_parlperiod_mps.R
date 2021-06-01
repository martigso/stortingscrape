#' Get list of MPs in a given parliamentary period
#' 
#' A function for retrieving Norwegian MPs for a given parliamentary period from the parliament API
#' 
#' @usage get_parlperiod_mps(periodid = NA, substitute = FALSE, good_manners = 0)
#' 
#' @param periodid Character string indicating the id of the parliamentary period to retrieve.
#' @param substitute Logical. Whether or not to include substitute MPs.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with response date, version, date of death/birth, first and last name, id, and gender... of the requested MP.
#' 
#' @family get_mp_data
#' 
#' 
#' @examples 
#' # Request one MP by id
#' get_parlperiod_mps("2005-2009")
#' 
#' # Request MPs from several periods by id
#' ids <- c("1961-65", "1997-01", "2009-2013")
#' mps <- lapply(ids, get_parlperiod_mps, good_manners = 2)
#' mps <- do.call(rbind, mps)
#' 
#' @import rvest
#' @export
#' 



get_parlperiod_mps <- function(periodid = NA, substitute = FALSE, good_manners = 0){
  
  if(substitute == FALSE){
    
    tmp <- read_html(paste0("https://data.stortinget.no/eksport/representanter?stortingsperiode=", periodid))
    
  } else if(substitute == TRUE){
    tmp <- read_html(paste0("https://data.stortinget.no/eksport/representanter?stortingsperiode=", periodid, "&vararepresentanter=true"))
    
  }
  
  tmp <- data.frame(response_date = tmp %>% html_elements("representanter_liste > representant > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_elements("representanter_liste > representant > versjon") %>% html_text(),
                    death = tmp %>% html_elements("representanter_liste > representant > doedsdato") %>% html_text(),
                    lastname = tmp %>% html_elements("representanter_liste > representant > etternavn") %>% html_text(),
                    birth = tmp %>% html_elements("representanter_liste > representant > foedselsdato") %>% html_text(),
                    firstname = tmp %>% html_elements("representanter_liste > representant > fornavn") %>% html_text(),
                    id = tmp %>% html_elements("representanter_liste > representant > id") %>% html_text(),
                    gender = tmp %>% html_elements("representanter_liste > representant > kjoenn") %>% html_text(),
                    county_id = tmp %>% html_elements("representanter_liste > representant > fylke > id") %>% html_text(),
                    party_id = tmp %>% html_elements("representanter_liste > representant > parti > id") %>% html_text(),
                    substitute_mp = tmp %>% html_elements("representanter_liste > representant > vara_representant") %>% html_text(),
                    period_id = tmp %>% html_elements("stortingsperiode_id") %>% html_text())
  
  message(paste(periodid, "done"))
  
  Sys.sleep(good_manners)
  
  return(tmp)
  
}

