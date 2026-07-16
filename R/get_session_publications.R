#' Retrieve publications of a type in a parliamentary session
#' 
#' A function for retrieving one of several publication types within a parliamentary session.
#' 
#' @usage get_session_publications(sessionid = NA, type = "referat", good_manners = 0)
#' 
#' @param sessionid Character string, or a vector of strings, indicating the id of the session to retrieve publications from.
#' @param type Character specifying type of publication to download. Available types are "referat" (minutes), 
#' "innstilling" (proposition), "innberetning" (report), "lovvedtak" (law decision), "lovanmerkning" (law note),
#' "dok8" (MP proposal) "dok12" (Constitutional proposal), and "dokumentserie" (document series). 
#' Defaults to "referat".
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function. Note that the Stortinget API is limited to 100 calls per minute (see \url{https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/}).
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                                |                                         |
#'    |:-------------------------------|:----------------------------------------|
#'    | **response_date**              | Date of data retrieval                  |
#'    | **version**                    | Data version from the API               |
#'    | **session_id**                 | Session id                              |
#'    | **publication_date**           | Date of publication                     |
#'    | **publication_id**             | Id of publication                       |
#'    | **publication_format**         | Publication format (XML)                |
#'    | **publication_available_date** | When the publication was made available |
#'    | **publication_title**          | Publication title                       |
#'    | **publication_type**           | Publication type                        |
#'    
#' @md
#' 
#' @seealso [get_publication]
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' pub <- get_session_publications("1998-99")
#' head(pub)
#' 
#' }
#' 
#' 
#' @import rvest httr2
#' @export
#' 
get_session_publications <- function(sessionid = NA, type = "referat", good_manners = 0){

  if(length(sessionid) > 1)
    return(fetch_multi(sessionid, get_session_publications, good_manners, type = type))
  
  url <- paste0("https://data.stortinget.no/eksport/publikasjoner?publikasjontype=", type, "&sesjonid=", sessionid)
  
  tmp <- api_get(url)
  
  if(identical(tmp |> html_elements("publikasjon > id") |> html_text(), character())){
    message(paste0("No '", type, "' in ", sessionid, ". Returning NA data frame"))
    tmp2 <- data.frame(response_date = tmp |> html_elements("publikasjoner_oversikt > respons_dato_tid") |> html_text(),
                       version = tmp |> html_elements("publikasjoner_oversikt > versjon") |> html_text(),
                       session_id = tmp |> html_elements("publikasjoner_oversikt > sesjon_id") |> html_text(),
                       publication_date = NA,
                       publication_id = NA,
                       publication_format = NA,
                       publication_available_date = NA,
                       publication_title = NA,
                       publication_type = NA)
    
  } else {
    tmp2 <- data.frame(response_date = tmp |> html_elements("publikasjoner_oversikt > respons_dato_tid") |> html_text(),
                       version = tmp |> html_elements("publikasjoner_oversikt > versjon") |> html_text(),
                       session_id = tmp |> html_elements("publikasjoner_oversikt > sesjon_id") |> html_text(),
                       publication_date = tmp |> html_elements("publikasjon > dato") |> html_text(),
                       publication_id = tmp |> html_elements("publikasjon > id") |> html_text(),
                       publication_format = sapply(tmp |> html_elements("publikasjon > publikasjonformat_liste"), function(x){
                         paste0(x |> html_elements("string") |> html_text(), collapse = "//")
                       }),
                       publication_available_date = tmp |> html_elements("publikasjon > tilgjengelig_dato") |> html_text(),
                       publication_title = tmp |> html_elements("publikasjon > tittel") |> html_text(),
                       publication_type = tmp |> html_elements("publikasjon > type") |> html_text())
    
  }
  
  
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}



