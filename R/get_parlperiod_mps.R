#' Get list of MPs in a given parliamentary period
#' 
#' @description A function for retrieving Norwegian MPs for a given parliamentary period from the parliament API
#' 
#' @usage get_parlperiod_mps(periodid = NA, substitute = FALSE, good_manners = 0)
#' 
#' @param periodid Character string indicating the id of the parliamentary period to retrieve.
#' @param substitute Logical. Whether or not to include substitute MPs.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with the following variables:
#'    |                   |                                        |
#'    |:------------------|:---------------------------------------|
#'    | **response_date** | Date of data retrieval                 |
#'    | **version**       | Data version from the API              |
#'    | **death**         | Date of death                          |
#'    | **lastname**      | MP last name                           |
#'    | **birth**         | Date of birth                          |
#'    | **firstname**     | MP first name                          |
#'    | **mp_id**         | MP id                                  |
#'    | **gender**        | MP gender                              |
#'    | **county_id**     | Id of county MP represented            |
#'    | **party_id**      | Id of party MP represented             |
#'    | **substitute_mp** | Logical for whether MP is a substitute |
#'    | **period_id**     | Id of period represented in            |
#'    
#' @md
#' 
#' @seealso [get_mp_bio] [get_mp] [get_mp_pic] [get_session_mp_speech_activity]
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # Request one MP by id
#' get_parlperiod_mps("2005-2009")
#' 
#' # Request MPs from several periods by id
#' ids <- c("1961-65", "1997-01", "2009-2013")
#' mps <- lapply(ids, get_parlperiod_mps, good_manners = 2)
#' mps <- do.call(rbind, mps)
#' 
#' }
#' 
#' @import httr2 rvest
#' @export
#' 
get_parlperiod_mps <- function(periodid = NA, substitute = FALSE, good_manners = 0){
  
  
  if(substitute == FALSE){
    url <- paste0(
      "https://data.stortinget.no/eksport/representanter?stortingsperiodeid=", 
      periodid
      )
    
    
  } else if(substitute == TRUE){
    url <- paste0(
      "https://data.stortinget.no/eksport/representanter?stortingsperiodeid=", 
      periodid, 
      "&vararepresentanter=true"
      )

  }
  
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
  
  tmp <- data.frame(response_date = tmp %>% html_elements("representanter_liste > representant > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_elements("representanter_liste > representant > versjon") %>% html_text(),
                    death = tmp %>% html_elements("representanter_liste > representant > doedsdato") %>% html_text(),
                    lastname = tmp %>% html_elements("representanter_liste > representant > etternavn") %>% html_text(),
                    birth = tmp %>% html_elements("representanter_liste > representant > foedselsdato") %>% html_text(),
                    firstname = tmp %>% html_elements("representanter_liste > representant > fornavn") %>% html_text(),
                    mp_id = tmp %>% html_elements("representanter_liste > representant > id") %>% html_text(),
                    gender = tmp %>% html_elements("representanter_liste > representant > kjoenn") %>% html_text(),
                    county_id = tmp %>% html_elements("representanter_liste > representant > fylke > id") %>% html_text(),
                    party_id = tmp %>% html_elements("representanter_liste > representant > parti > id") %>% html_text(),
                    substitute_mp = tmp %>% html_elements("representanter_liste > representant > vara_representant") %>% html_text(),
                    period_id = tmp %>% html_elements("stortingsperiode_id") %>% html_text())
  
  message(periodid, " done")
  
  Sys.sleep(good_manners)
  
  return(tmp)
  
}

