#' Retrieve publications of a type in a parliamentary session
#' 
#' A function for retrieving one of several publication types within a parliamentary session.
#' 
#' @usage get_session_publications(sessionid = NA, type = "referat", good_manners = 0)
#' 
#' @param sessionid Character string indicating the id of the hearing to retrieve.
#' @param type Character specifying type of publication to download. Available types are "referat" (minutes), 
#' "innstilling" (proposition), "innberetning" (report), "lovvedtak" (law decision), "lovanmerkning" (law note),
#' "dok8" (MP proposal) "dok12" (Constitutional proposal), and "dokumentserie" (document series). 
#' Defaults to "referat".
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
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
  
  url <- paste0("https://data.stortinget.no/eksport/publikasjoner?publikasjontype=", type, "&sesjonid=", sessionid)
  
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



