#' Parliamentary parties in specified session
#' 
#' A function for retrieving Norwegian parliamentary parties for a specified parliamentary session
#' 
#' @usage get_session_parties(sessionid = NA, good_manners = 0)
#' 
#' @param sessionid Character string, or a vector of strings, indicating the id of the parliamentary session to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function. Note that the Stortinget API is limited to 100 calls per minute (see \url{https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/}).
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                       |                                                     |
#'    |:----------------------|:----------------------------------------------------|
#'    | **response_date**     | Date of data retrieval                              |
#'    | **version**           | Data version from the API                           |
#'    | **id**                | Party id                                            |
#'    | **name**              | Party name                                          |
#'    | **represented_party** | Logical indication of whether party was represented |
#'    | **session_id**        | Session id                                          |
#'    | **period_id**         | Parliamentary period id                             |
#' 
#' @md
#' 
#' @seealso [get_all_parties]
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' parties <- get_session_parties("2003-2004")
#' parties
#' 
#' }
#'  
#' @import rvest httr2
#' @export
#' 
get_session_parties <- function(sessionid = NA, good_manners = 0){

  if(length(sessionid) > 1)
    return(fetch_multi(sessionid, get_session_parties, good_manners))
  
  url <- paste0("https://data.stortinget.no/eksport/partier?sesjonid=", sessionid)
  
  tmp <- api_get(url)
  
  tmp <- data.frame(response_date = tmp |> html_elements("partier_liste > parti > respons_dato_tid") |> html_text(),
                    version = tmp |> html_elements("partier_liste > parti > versjon") |> html_text(),
                    id = tmp |> html_elements("partier_liste > parti > id") |> html_text(),
                    name = tmp |> html_elements("partier_liste > parti > navn") |> html_text(),
                    represented_party = tmp |> html_elements("partier_liste > parti > representert_parti") |> html_text(),
                    session_id = tmp |> html_elements("sesjon_id") |> html_text(),
                    period_id = tmp |> html_elements("stortingsperiode_id") |> html_text())
  
  Sys.sleep(good_manners)
  
  return(tmp)
  
}


