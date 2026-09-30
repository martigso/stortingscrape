#' Retrieve a specific publication
#' 
#' A function for retrieving a specific publication. Because these are formatted very differently in the API,
#' the returning object is the raw XML document, best manipulated with node extraction functions
#' such as \code{\link[rvest:html_elements]{rvest::html_elements()}} or \code{\link[xml2:xml_find_all]{xml2::xml_find_all()}}.
#'
#' Note that element and attribute names are case sensitive. Publications from before the 2016-2017 session
#' use lowercase names (e.g. `innlegg`, `sakid`), while later publications use capitalized names
#' (e.g. `Replikk`, `personID`). For debate transcripts, [get_speeches] returns the speeches as a data frame.
#'
#' @usage get_publication(publicationid = NA, good_manners = 0)
#'
#' @param publicationid Character string, or a vector of strings, indicating the id of the publication to retrieve
#' @param good_manners Numeric. Seconds delay between calls when making multiple calls to the same function. Note that the Stortinget API is limited to 100 calls per minute (see \url{https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/}).
#'
#' @return A raw xml_document
#'
#' @md
#'
#' @seealso [get_speeches] [get_question] [get_question_hour] [get_session_publications]
#'
#' @examples
#'
#' \dontrun{
#' pub <- get_publication("refs-201819-03-06")
#' (pub |> html_elements("Replikk"))[1] |> html_text()
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
  
  tmp <- api_get(url, as = "xml")

  Sys.sleep(good_manners)
  
  return(tmp)
  
}

