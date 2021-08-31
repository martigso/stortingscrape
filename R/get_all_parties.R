#' All parliamentary parties
#' 
#' @description A function for retrieving all Norwegian parliamentary parties in the 
#' data.stortinget.no API.
#' 
#' @usage get_all_parties()
#'  
#' @return A data.frame of all parties, with the variables:
#' 
#' |                       |                                                          |
#' |:----------------------|:---------------------------------------------------------|
#' | **response_date**     | Date of data retrieval                                   |
#' | **version**           | Data version from the API                                |
#' | **id**                | Id of the party                                          |
#' | **name**              | Name of the party                                        |
#' | **represented_party** | Whether the party is represented at the time of download |
#' | **sessionid**         | Id of session (empty)                                    |
#' | **period_id**         | Id of parliamentary period (empty)                       |
#' 
#' @md
#' 
#' @seealso [get_session_parties]
#' 
#' 
#' @examples 
#' \dontrun{
#' parties <- get_all_parties()
#' head(parties)
#' }
#' 
#' @import httr rvest
#' @export
get_all_parties <- function(){
  
  url <- "https://data.stortinget.no/eksport/allepartier"

  base <- GET(url)
  
  resp <- http_type(base)
  if(resp != "text/xml") stop(paste0("Response of ", url, " is not text/xml."), call. = FALSE)
  
  status <- http_status(base)
  if(status$category != "Success") stop(paste0("Response of ", url, " returned as '", status$message, "'"), call. = FALSE)
  
  tmp <- read_html(base)
  
  
  tmp <- data.frame(response_date = tmp %>% html_elements("partier_liste > parti > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_elements("partier_liste > parti > versjon") %>% html_text(),
                    id = tmp %>% html_elements("partier_liste > parti > id") %>% html_text(),
                    name = tmp %>% html_elements("partier_liste > parti > navn") %>% html_text(),
                    represented_party = tmp %>% html_elements("partier_liste > parti > representert_parti") %>% html_text(),
                    sessionid = tmp %>% html_elements("sesjon_id") %>% html_text(),
                    period_id = tmp %>% html_elements("stortingsperiode_id") %>% html_text())
  
  return(tmp)
  
}


