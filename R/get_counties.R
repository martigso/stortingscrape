#' Get list of MPs in a given parliamentary period
#' 
#' A function for retrieving Norwegian MPs for a given parliamentary period from the parliament API
#' 
#' @usage get_counties(historical = FALSE)
#' 
#' @param historical Logical. Whether or not to include historical counties.
#' 
#' @return A data.frame with response date, version, county id, county name, and indication on whether county is historical.
#' 
#' @family get_mp_data
#' 
#' 
#' @examples 
#' # Request one MP by id
#' get_counties()
#' 
#' # With historical counties
#' get_counties(historical = TRUE)
#' 
#' @import rvest
#' 
#' @export
#' 



get_counties <- function(historical = FALSE){
  
  
  if(historical == FALSE){
    
    tmp <- read_html("https://data.stortinget.no/eksport/fylker")
    
  } 
  
  if(historical == TRUE){
    
    tmp <- read_html("https://data.stortinget.no/eksport/fylker/?historiskefylker=true")
    
  }
  
  
  tmp <- data.frame(response_date = tmp %>% html_nodes("fylker_liste > fylke > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_nodes("fylker_liste > fylke > versjon") %>% html_text(),
                    historical_county = tmp %>% html_nodes("fylker_liste > fylke > historisk_fylke") %>% html_text(),
                    id = tmp %>% html_nodes("fylker_liste > fylke > id") %>% html_text(),
                    name = tmp %>% html_nodes("fylker_liste > fylke > navn") %>% html_text())
  
  return(tmp)
  
  
}