#' Parliamentary committees in specified session
#' 
#' A function for retrieving Norwegian parliamentary committees for a specified parliamentary session
#' 
#' @usage get_session_committees(sessionid = NA, good_manners = 0)
#' 
#' @param sessionid Character string, or a vector of strings, indicating the id of the parliamentary session to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function. Note that the Stortinget API is limited to 100 calls per minute (see \url{https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/}).
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
#' @import rvest httr2
#' @export
#' 
get_session_committees <- function(sessionid = NA, good_manners = 0){

  if(length(sessionid) > 1)
    return(fetch_multi(sessionid, get_session_committees, good_manners))
  
  url <- paste0("http://data.stortinget.no/eksport/komiteer?sesjonid=", sessionid)
  
  tmp <- api_get(url)
  
  tmp <- data.frame(response_date = tmp |> html_elements("komiteer_liste > komite > respons_dato_tid") |> html_text(),
                    version = tmp |> html_elements("komiteer_liste > komite > versjon") |> html_text(),
                    id = tmp |> html_elements("komiteer_liste > komite > id") |> html_text(),
                    name = tmp |> html_elements("komiteer_liste > komite > navn") |> html_text(),
                    session_id = tmp |> html_elements("sesjon_id") |> html_text())
  
  Sys.sleep(good_manners)
  
  return(tmp)
  
}


