#' Retrieve all decisions for a specified session
#' 
#' A function for retrieving all decisions from a specific parliamentary session.
#' 
#' @usage get_session_decisions(sessionid = NA, good_manners = 0)
#' 
#' @param sessionid Character string indicating the id of the session to request all votes from
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                        |                                        |
#'    |:-----------------------|:---------------------------------------|
#'    | **response_date**      | Date of data retrieval                 |
#'    | **version**            | Data version from the API              |
#'    | **session_id**         | Session id                             |
#'    | **decision_id**        | Decision id                            |
#'    | **case_id**            | Case id                                |
#'    | **case_link_url**      | URL for case to front end web page     |
#'    | **decision_date**      | Decision date                          |
#'    | **decision_link_url**  | URL for decision to front end web page |
#'    | **decision_number**    | Decision number within session         |
#'    | **decision_text**      | Decision text                          |
#'    | **decision_title**     | Decision title                         |
#'    | **decision_type_id**   | Decision type id                       |
#'    | **decision_type_name** | Decision type name                     |
#' 
#' 
#' 
#' @seealso [get_decision_votes]
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' desci <- get_session_decisions("2004-2005")
#' head(desci)
#' }
#'  
#' @import rvest httr2
#' 
#' @export
#' 
get_session_decisions <- function(sessionid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/stortingsvedtak?sesjonid=", sessionid)
  
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
  
  tmp2 <- data.frame(response_date = tmp |> html_elements("stortingsvedtak_oversikt > respons_dato_tid") |> html_text(),
                     version = tmp |> html_elements("stortingsvedtak_oversikt > versjon") |> html_text(),
                     session_id = tmp |> html_elements("stortingsvedtak_oversikt > sesjon_id") |> html_text(),
                     decision_id = tmp |> html_elements("stortingsvedtak > id") |> html_text(),
                     case_id = tmp |> html_elements("stortingsvedtak > sak_id") |> html_text(),
                     case_link_url = tmp |> html_elements("stortingsvedtak > sak_lenke_url") |> html_text(),
                     decision_date = tmp |> html_elements("stortingsvedtak > stortingsvedtak_dato_tid") |> html_text(),
                     decision_link_url = tmp |> html_elements("stortingsvedtak > stortingsvedtak_lenke_url") |> html_text(),
                     decision_number = tmp |> html_elements("stortingsvedtak > stortingsvedtak_nummer") |> html_text(),
                     decision_title = tmp |> html_elements("stortingsvedtak > stortingsvedtak_tittel") |> html_text())
  
  decision_type_id <- lapply(tmp |> html_elements("stortingsvedtak > stortingsvedtak_type"), function(x){
    
    tmp_id <- x |> html_elements("id") |> html_text()
    
    if(identical(tmp_id, character())){
      tmp_id <- NA
    } 
    
    return(tmp_id)
    
  })

  decision_type_name <- lapply(tmp |> html_elements("stortingsvedtak > stortingsvedtak_type"), function(x){
    
    tmp_id <- x |> html_elements("navn") |> html_text()
    
    if(identical(tmp_id, character())){
      tmp_id <- NA
    } 
    
    return(tmp_id)
    
  })
  
  decision_text <- lapply(tmp |> html_elements("stortingsvedtak > stortingsvedtak_tekst"), function(x){
    
    if(x |> html_text() == "") {
      return("")
    }
    
    x |> html_text() |> read_html() |> html_text() |> trimws()
    
  }) |> unlist()
  
  tmp2$decision_text <- decision_text
  tmp2$decision_type_id <- do.call(c, decision_type_id)
  tmp2$decision_type_name <- do.call(c, decision_type_name)
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

