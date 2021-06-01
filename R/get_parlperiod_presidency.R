#' Get list of presidency in a given parliamentary period
#' 
#' A function for retrieving the presidency for a given parliamentary period from the parliament API
#' 
#' @usage get_parlperiod_presidency(periodid = NA, good_manners = 0)
#' 
#' @param periodid Character string indicating the id of the parliamentary period to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with response date, version ...
#' 
#' @family get_mp_data
#' 
#' 
#' @examples 
#' # Request one MP by id
#' get_parlperiod_presidency("2005-2009")
#' 
#' @import rvest
#' @export
#' 



get_parlperiod_presidency <- function(periodid = NA, good_manners = 0){
  
    
  tmp <- read_html(paste0("https://data.stortinget.no/eksport/presidentskapet?stortingsperiodeid=", periodid))
  
  tmp <- data.frame(response_date = tmp %>% html_nodes("presidentskapet_oversikt > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_nodes("presidentskapet_oversikt > versjon") %>% html_text(),
                    last_name = tmp %>% html_nodes("medlem > etternavn") %>% html_text(),
                    first_name = tmp %>% html_nodes("medlem > fornavn") %>% html_text(),
                    from_date = tmp %>% html_nodes("medlem > fra_dato") %>% html_text(),
                    party_id = tmp %>% html_nodes("medlem > parti_id") %>% html_text(),
                    person_id = tmp %>% html_nodes("medlem > person_id") %>% html_text(),
                    to_date = tmp %>% html_nodes("medlem > til_dato") %>% html_text(),
                    position = tmp %>% html_nodes("medlem > verv") %>% html_text())
  
  Sys.sleep(good_manners)
  
  return(tmp)
  
}

