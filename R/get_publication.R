#' Retrieve a specific publication
#' 
#' A function for retrieving a specific publication. Because these are formated very differently in the API,
#' the returning object is in a raw html_document format, best manipulated with html packages such as [rvest].
#' 
#' @usage get_question_hour(publicationid = NA, good_manners = 0)
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
#' (pub %>% html_elements("replikk"))[1] %>% html_text()
#' }
#' 
#'  
#' @import rvest httr
#' 
#' @export
#' 
get_publication <- function(publicationid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/publikasjon?publikasjonid=", publicationid)
  
  base <- GET(url)
  
  resp <- http_type(base)
  if(resp != "text/xml") stop(paste0("Response of ", url, " is not text/xml."), call. = FALSE)
  
  status <- http_status(base)
  if(status$category != "Success") stop(paste0("Response of ", url, " returned as '", status$message, "'"), call. = FALSE)
  
  tmp <- read_html(base)

  return(tmp)
  
}

