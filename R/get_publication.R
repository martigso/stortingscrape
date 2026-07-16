#' Retrieve a specific publication
#' 
#' A function for retrieving a specific publication. Because these are formatted very differently in the API,
#' the returning object is in a raw html_document format, best manipulated with html node extraction functions 
#' such as \code{\link[rvest:html_elements]{rvest::html_elements()}} .
#' 
#' @usage get_publication(publicationid = NA, good_manners = 0)
#' 
#' @param publicationid Character string, or a vector of strings, indicating the id of the publication to retrieve
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function. Note that the Stortinget API is limited to 100 calls per minute (see \url{https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/}).
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

  if(length(publicationid) > 1)
    return(fetch_multi(publicationid, get_publication, good_manners, .combine = NULL))
  
  url <- paste0("https://data.stortinget.no/eksport/publikasjon?publikasjonid=", publicationid)
  
  tmp <- api_get(url)

  Sys.sleep(good_manners)
  
  return(tmp)
  
}

