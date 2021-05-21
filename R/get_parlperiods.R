#' Get Parliamentary Periods
#' 
#' A function for retrieving dates of the parliamentary periods after WWII
#' 
#' @usage get_parlperiods() 
#' 
#' @return A data.frame with from/to date and id for all parliamentary periods after WWII
#' 
#' @family get_mp_data
#' 
#' 
#' @examples 
#' 
#' get_parlperiods()
#' 
#' 
#' @export
#' 



get_parlperiods <- function(periodid){
  
  require(rvest)
  
  tmp <- read_html("https://data.stortinget.no/eksport/stortingsperioder")
  
  tmp <- data.frame(response_date = tmp %>% html_nodes("stortingsperioder_liste > stortingsperiode > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_nodes("stortingsperioder_liste > stortingsperiode > versjon") %>% html_text(),
                    from = tmp %>% html_nodes("stortingsperioder_liste > stortingsperiode > fra") %>% html_text(),
                    id = tmp %>% html_nodes("stortingsperioder_liste > stortingsperiode > id") %>% html_text(),
                    to = tmp %>% html_nodes("stortingsperioder_liste > stortingsperiode > til") %>% html_text())
  
  return(tmp)
  
  
}