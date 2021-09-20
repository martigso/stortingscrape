#' Parliamentary committees in specified session
#' 
#' A function for retrieving Norwegian parliamentary committees for a specified parliamentary session
#' 
#' @usage get_session_committees(sessionid = NA, good_manners = 0)
#' 
#' @param sessionid Character string indicating the id of the parliamentary session to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                   |                                               |
#'    |:------------------|:----------------------------------------------|
#'    | **response_date** | Date of data retrieval                        |
#'    | **version**       | Data version from the API                     |
#'    | **id**            | Committee id                                  |
#'    | **name**          | Committee name                                |
#'    | **session_id**    | Session id                                    |
#' 
#' @seealso [get_all_committees] [get_mp_bio] [get_mp]
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' coms <- get_session_committees("2001-2002")
#' coms
#' }
#' 
#' @import rvest httr
#' @export
#' 
get_session_committees <- function(sessionid = NA, good_manners = 0){
  
  url <- paste0("http://data.stortinget.no/eksport/komiteer?sesjonid=", sessionid)
  
  base <- GET(url)
  
  resp <- http_type(base)
  if(resp != "text/xml") stop(paste0("Response of ", url, " is not text/xml."), call. = FALSE)
  
  status <- http_status(base)
  if(status$category != "Success") stop(paste0("Response of ", url, " returned as '", status$message, "'"), call. = FALSE)
  
  tmp <- read_html(base)
  
  tmp <- data.frame(response_date = tmp %>% html_elements("komiteer_liste > komite > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_elements("komiteer_liste > komite > versjon") %>% html_text(),
                    id = tmp %>% html_elements("komiteer_liste > komite > id") %>% html_text(),
                    name = tmp %>% html_elements("komiteer_liste > komite > navn") %>% html_text(),
                    session_id = tmp %>% html_elements("sesjon_id") %>% html_text())
  
  Sys.sleep(good_manners)
  
  return(tmp)
  
}


