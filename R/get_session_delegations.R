#' Parliamentary delegations in specified session
#' 
#' A function for retrieving delegations for a specified parliamentary session.
#' 
#' @usage get_session_delegations(sessionid = NA, good_manners = 0)
#' 
#' @param sessionid Character string, or a vector of strings, indicating the id of the parliamentary session to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function. Note that the Stortinget API is limited to 100 calls per minute (see \url{https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/}).
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                   |                           |
#'    |:------------------|:--------------------------|
#'    | **response_date** | Date of data retrieval    |
#'    | **version**       | Data version from the API |
#'    | **id**            | Delegation id             |
#'    | **name**          | Name of delegation        |
#'    | **session_id**    | Session id                |
#' 
#' @seealso [get_session_committees] [get_all_committees]
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' deleg <- get_session_delegations("2015-2016")
#' deleg
#' 
#' }
#' 
#' @import rvest httr2
#' @export
#' 



get_session_delegations <- function(sessionid = NA, good_manners = 0){

  if(length(sessionid) > 1)
    return(fetch_multi(sessionid, get_session_delegations, good_manners))
  
  url <- paste0("https://data.stortinget.no/eksport/delegasjoner?sesjonid=", sessionid)
  
  tmp <- api_get(url)
  
  tmp <- data.frame(response_date = tmp |> html_elements("delegasjoner_liste > delegasjon > respons_dato_tid") |> html_text(),
                    version = tmp |> html_elements("delegasjoner_liste > delegasjon > versjon") |> html_text(),
                    id = tmp |> html_elements("delegasjoner_liste > delegasjon > id") |> html_text(),
                    name = tmp |> html_elements("delegasjoner_liste > delegasjon > navn") |> html_text(),
                    session_id = tmp |> html_elements("sesjon_id") |> html_text())
  
  Sys.sleep(good_manners)
  
  return(tmp)
  
}


