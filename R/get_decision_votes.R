#' Retreive vote decision for a specified vote
#' 
#' @description A function for retrieving vote decisions from a specific vote. 
#' Vote data are only available from the 2011-2012 session
#' 
#' @usage get_decision_votes(voteid = NA, good_manners = 0)
#' 
#' @param voteid Character string indicating the id of the vote to request all votes from
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                        |                            |
#'    |:-----------------------|:---------------------------|
#'    | **response_date**      | Date of data retrieval     |
#'    | **version**            | Data version from the API  |
#'    | **vote_id**            | Id of the vote             |
#'    | **decision_code**      | General code for decision  |
#'    | **decision_comment**   | Comments for the decision  |
#'    | **decision_number**    | Decision number            |
#'    | **decision_reference** | Reference for the decision |
#'    | **decision_text**      | Full text of the decision  |
#'    
#' @md
#' 
#' @seealso [get_session_decisions] [get_proposal_votes] [get_vote] [get_result_vote]
#' 
#' @examples 
#' \dontrun{
#' decision <- get_decision_votes(123)
#' decision
#' }
#'  
#' @import httr2 rvest
#' 
#' @export
#' 
get_decision_votes <- function(voteid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/voteringsvedtak?voteringid=", voteid)
  
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
  
  if(identical(html_elements(tmp, "voteringsvedtak_liste > voteringsvedtak") |> html_text(), character()) == TRUE){
    tmp2 <- data.frame(response_date = tmp |> html_elements("voteringsvedtak_oversikt > respons_dato_tid") |> html_text(),
                       version = tmp |> html_elements("voteringsvedtak_oversikt > versjon") |> html_text(),
                       vote_id = tmp |> html_elements("voteringsvedtak_oversikt > votering_id") |> html_text(),
                       decision_code = NA,
                       decision_comment = NA,
                       decision_number = NA,
                       decision_reference = NA,
                       decision_text = NA)
  } else {
    tmp2 <- data.frame(response_date = tmp |> html_elements("voteringsvedtak_oversikt > respons_dato_tid") |> html_text(),
                       version = tmp |> html_elements("voteringsvedtak_oversikt > versjon") |> html_text(),
                       vote_id = tmp |> html_elements("voteringsvedtak_oversikt > votering_id") |> html_text(),
                       decision_code = tmp |> html_elements("voteringsvedtak_liste > voteringsvedtak > vedtak_kode") |> html_text(),
                       decision_comment = tmp |> html_elements("voteringsvedtak > vedtak_kommentar") |> html_text(),
                       decision_number = tmp |> html_elements("voteringsvedtak > vedtak_nummer") |> html_text(),
                       decision_reference = tmp |> html_elements("voteringsvedtak > vedtak_referanse") |> html_text(),
                       decision_text = tmp |> html_elements("voteringsvedtak > vedtak_tekst") |> html_text())
    
  }

  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

