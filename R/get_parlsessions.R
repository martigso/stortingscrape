#' Get Parliamentary Sessions
#' 
#' A function for retrieving dates of the parliamentary sessions after WWII
#' 
#' @usage get_parlsessions()
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                   |                                               |
#'    |:------------------|:----------------------------------------------|
#'    | **response_date** | Date of data retrieval                        |
#'    | **version**       | Data version from the API                     |
#'    | **from**          | Date session started                          |
#'    | **id**            | Id of for session (used for other functions)  |
#'    | **to**            | Date session ended                            |
#'    | **years**         | From year to year in full format              |
#'    
#' @md
#'    
#' @seealso [get_parlperiods]
#' 
#' 
#' @examples 
#' \dontrun{
#' 
#' parlses <- get_parlsessions()
#' parlses
#' 
#' }
#' 
#' @import rvest httr
#' 
#' @export
#' 
get_parlsessions <- function(){
  
  url <- "https://data.stortinget.no/eksport/sesjoner"
  
  base <- GET(url)
  
  resp <- http_type(base)
  if(resp != "text/xml") stop(paste0("Response of ", url, " is not text/xml."), call. = FALSE)
  
  status <- http_status(base)
  if(status$category != "Success") stop(paste0("Response of ", url, " returned as '", status$message, "'"), call. = FALSE)
  
  tmp <- read_html(base)
  
  tmp <- data.frame(response_date = tmp %>% html_elements("sesjoner_liste > sesjon > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_elements("sesjoner_liste > sesjon > versjon") %>% html_text(),
                    from = tmp %>% html_elements("sesjoner_liste > sesjon > fra") %>% html_text(),
                    id = tmp %>% html_elements("sesjoner_liste > sesjon > id") %>% html_text(),
                    to = tmp %>% html_elements("sesjoner_liste > sesjon > til") %>% html_text())
  tmp$years <- paste(format(as.Date(tmp$from), "%Y"), format(as.Date(tmp$to), "%Y"), sep = "-")
  
  return(tmp)
  
  
}