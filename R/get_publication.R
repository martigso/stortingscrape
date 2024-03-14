#' Retrieve a specific publication
#' 
#' A function for retrieving a specific publication. Because these are formated very differently in the API,
#' the returning object is in a raw html_document format, best manipulated with html packages such as \link{rvest}.
#' 
#' @usage get_publication(publicationid = NA, good_manners = 0)
#' 
#' @param publicationid Character string indicating the id of the publication to request all votes from
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A raw html_document
#'
#' @md
#' 
#' @seealso [get_question] [get_question_hour] [get_session_publications]
#' 
#' @examples 
#' 
#' \dontrun{
#' pub <- get_publication("refs-201819-03-06")
#' (pub |> html_elements("replikk"))[1] |> html_text()
#' }
#'  
#' @import rvest httr2
#' 
#' @export
#' 
get_publication <- function(publicationid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/publikasjon?publikasjonid=", publicationid)
  
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

  Sys.sleep(good_manners)
  
  return(tmp)
  
}

