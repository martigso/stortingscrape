#' Get list of MPs in a given parliamentary period
#' 
#' @description A function for retrieving Norwegian MPs for a given parliamentary 
#' period from the parliament API.
#' 
#' @usage get_counties(historical = FALSE)
#' 
#' @param historical Logical. Whether or not to include historical counties.
#' 
#' @return A data frame with the following variables:
#' 
#' 
#'    |                       |                                                     |
#'    |:----------------------|:----------------------------------------------------|
#'    | **response_date**     | Date of data retrieval                              |
#'    | **version**           | Data version from the API                           |
#'    | **historical_county** | Whether the county is historical (no longer exists) |
#'    | **id**                | Id of the county                                    |
#'    | **name**              | Name of the county                                  |
#' 
#' @md
#' 
#' 
#' 
#' @examples
#' \dontrun{ 
#' # Request one MP by id
#' get_counties()
#' 
#' # With historical counties
#' get_counties(historical = TRUE)
#' }
#' 
#' @import rvest httr
#' 
#' @export
#' 
get_counties <- function(historical = FALSE){
  
  
  if(historical == FALSE){
    
    url <- "https://data.stortinget.no/eksport/fylker"
    
    
  } 
  
  if(historical == TRUE){
    
    url <- "https://data.stortinget.no/eksport/fylker/?historiskefylker=true"
    
  }
  
  base <- GET(url)
  
  resp <- http_type(base)
  if(resp != "text/xml") stop(paste0("Response of ", url, " is not text/xml."), call. = FALSE)
  
  status <- http_status(base)
  if(status$category != "Success") stop(paste0("Response of ", url, " returned as '", status$message, "'"), call. = FALSE)
  
  tmp <- read_html(base)
  
  tmp <- data.frame(response_date = tmp %>% html_elements("fylker_liste > fylke > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_elements("fylker_liste > fylke > versjon") %>% html_text(),
                    historical_county = tmp %>% html_elements("fylker_liste > fylke > historisk_fylke") %>% html_text(),
                    id = tmp %>% html_elements("fylker_liste > fylke > id") %>% html_text(),
                    name = tmp %>% html_elements("fylker_liste > fylke > navn") %>% html_text())
  
  return(tmp)
  
}