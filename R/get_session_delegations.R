#' Parliamentary delegations in specified session
#' 
#' A function for retrieving delegations for a specified parliamentary session.
#' 
#' @usage get_session_delegations(sessionid = NA, good_manners = 0)
#' 
#' @param sessionid Character string indicating the id of the parliamentary session to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
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
  
  url <- paste0("https://data.stortinget.no/eksport/delegasjoner?sesjonid=", sessionid)
  
  base <- request(url)
  
  resp <- base |> 
    req_error(is_error = function(resp) FALSE) |> 
    req_perform()
  
  if(resp$status_code != 200) {
    stop(
      paste0(
        "Response of ", 
        url, 
        " is '", 
        resp |> resp_status_desc(),
        "' (",
        resp$status_code,
        ")."
      ), 
      call. = FALSE)
  }
  
  if(resp_content_type(resp) != "text/xml") {
    stop(
      paste0(
        "Response of ", 
        url, 
        " returned as '", 
        resp_content_type(resp), 
        "'.",
        " Should be 'text/xml'."), 
      call. = FALSE) 
  }
  
  tmp <- resp |> 
    resp_body_html(check_type = FALSE, encoding = "utf-8") 
  
  tmp <- data.frame(response_date = tmp |> html_elements("delegasjoner_liste > delegasjon > respons_dato_tid") |> html_text(),
                    version = tmp |> html_elements("delegasjoner_liste > delegasjon > versjon") |> html_text(),
                    id = tmp |> html_elements("delegasjoner_liste > delegasjon > id") |> html_text(),
                    name = tmp |> html_elements("delegasjoner_liste > delegasjon > navn") |> html_text(),
                    session_id = tmp |> html_elements("sesjon_id") |> html_text())
  
  Sys.sleep(good_manners)
  
  return(tmp)
  
}


