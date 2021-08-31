#' Get list of presidency in a given parliamentary period
#' 
#' @description A function for retrieving the presidency for a given parliamentary period from the parliament API.
#' 
#' @usage get_parlperiod_presidency(periodid = NA, good_manners = 0)
#' 
#' @param periodid Character string indicating the id of the parliamentary period to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                   |                                        |
#'    |:------------------|:---------------------------------------|
#'    | **response_date** | Date of data retrieval                 |
#'    | **version**       | Data version from the API              |
#'    | **last_name**     | Last name of presidency member         |
#'    | **first_name**    | First name of presidency member        |
#'    | **from_date**     | Presidency member from date            |
#'    | **party_id**      | Party affiliation of presidency member |
#'    | **person_id**     | Id of the presidency member            |
#'    | **to_date**       | Presidency member to date              |
#'    | **position**      | Presidency position                    |
#' 
#' @seealso [get_mp] [get_mp_bio]
#' 
#' 
#' @examples
#' \dontrun{
#'  
#' # Request one MP by id
#' get_parlperiod_presidency("2005-2009")
#' 
#' }
#' 
#' @import rvest
#' @export
#' 
get_parlperiod_presidency <- function(periodid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/presidentskapet?stortingsperiodeid=", periodid)
  
  base <- GET(url)
  
  resp <- http_type(base)
  if(resp != "text/xml") stop(paste0("Response of ", url, " is not text/xml."), call. = FALSE)
  
  status <- http_status(base)
  if(status$category != "Success") stop(paste0("Response of ", url, " returned as '", status$message, "'"), call. = FALSE)
  
  tmp <- read_html(base)
  
  tmp <- data.frame(response_date = tmp %>% html_elements("presidentskapet_oversikt > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_elements("presidentskapet_oversikt > versjon") %>% html_text(),
                    last_name = tmp %>% html_elements("medlem > etternavn") %>% html_text(),
                    first_name = tmp %>% html_elements("medlem > fornavn") %>% html_text(),
                    from_date = tmp %>% html_elements("medlem > fra_dato") %>% html_text(),
                    party_id = tmp %>% html_elements("medlem > parti_id") %>% html_text(),
                    person_id = tmp %>% html_elements("medlem > person_id") %>% html_text(),
                    to_date = tmp %>% html_elements("medlem > til_dato") %>% html_text(),
                    position = tmp %>% html_elements("medlem > verv") %>% html_text())
  
  Sys.sleep(good_manners)
  
  return(tmp)
  
}

