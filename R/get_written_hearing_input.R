#' Retrieve written input for a specified hearing
#' 
#' A function for retrieving written input for a specified hearing
#' 
#' @usage get_written_hearing_input(hearingid = NA, good_manners = 0)
#' 
#' @param hearingid Character string indicating the id of the hearing to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame the following variables:
#' 
#'    |                                |                                             |
#'    |:-------------------------------|:--------------------------------------------|
#'    | **response_date**              | Date of data retrieval                      |
#'    | **version**                    | Data version from the API                   |
#'    | **hearing_id**                 | Id of the hearing                           |
#'    | **hearing_type**               | Type of hearing                             |
#'    | **committee_id**               | Id of committee responsible for the hearing |
#'    | **hearing_input_date**         | Date of receiving input                     |
#'    | **hearing_input_id**           | Hearing input id                            |
#'    | **hearing_input_organization** | Organization giving input                   |
#'    | **hearing_input_text**         | Full text of the hearing input              |
#'    | **hearing_input_title**        | Title of the hearing input                  |
#' 
#' @md
#' 
#' @seealso [get_hearing_input] [get_hearing_program] [get_session_hearings]
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' get_written_hearing_input(hearingid = 10004243)
#' 
#' }
#' 
#' @import rvest httr
#' @export
#' 
get_written_hearing_input <- function(hearingid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/skriftligInnspill?horingid=", hearingid)
  
  base <- GET(url)
  
  resp <- http_type(base)
  if(resp != "text/xml"){
    message(paste0("Response of ", url, " is not text/xml. Returning NA on all variables"))
    tmp2 <- data.frame(response_date = NA,
                       version = NA,
                       hearing_id = hearingid,
                       hearing_type = NA,
                       committee_id = NA,
                       hearing_input_date = NA,
                       hearing_input_id = NA,
                       hearing_input_organization = NA,
                       hearing_input_text = NA,
                       hearing_input_title = NA)
    
    Sys.sleep(good_manners)
    
    return(tmp2)
    
  } 
  
  status <- http_status(base)
  if(status$category != "Success") stop(paste0("Response of ", url, " returned as '", status$message, "'"), call. = FALSE)
  
  tmp <- read_html(base)
  
  if(identical(tmp %>% html_elements("skriftlig_innspill > tekst") %>% html_text(), character())){
    
    message(paste0("Response of hearing ", hearingid, " has no text. Returning NA on missing variables"))
    
    tmp2 <- data.frame(response_date = tmp %>% html_elements("skriftlige_innspill_oversikt > respons_dato_tid") %>% html_text(),
                       version = tmp %>% html_elements("skriftlige_innspill_oversikt > versjon") %>% html_text(),
                       hearing_id = tmp %>% html_elements("skriftlige_innspill_oversikt > horing_id") %>% html_text(),
                       hearing_type = tmp %>% html_elements("skriftlige_innspill_oversikt > horing_type") %>% html_text(),
                       committee_id = tmp %>% html_elements("skriftlige_innspill_oversikt > komite > id") %>% html_text(),
                       hearing_input_date = NA,
                       hearing_input_id = NA,
                       hearing_input_organization = NA,
                       hearing_input_text = NA,
                       hearing_input_title = NA)
    
    Sys.sleep(good_manners)
    
    return(tmp2)
  }
  tmp2 <- data.frame(response_date = tmp %>% html_elements("skriftlige_innspill_oversikt > respons_dato_tid") %>% html_text(),
                     version = tmp %>% html_elements("skriftlige_innspill_oversikt > versjon") %>% html_text(),
                     hearing_id = tmp %>% html_elements("skriftlige_innspill_oversikt > horing_id") %>% html_text(),
                     hearing_type = tmp %>% html_elements("skriftlige_innspill_oversikt > horing_type") %>% html_text(),
                     committee_id = tmp %>% html_elements("skriftlige_innspill_oversikt > komite > id") %>% html_text(),
                     hearing_input_date = tmp %>% html_elements("skriftlig_innspill > dato") %>% html_text(),
                     hearing_input_id = tmp %>% html_elements("skriftlig_innspill > id") %>% html_text(),
                     hearing_input_organization = tmp %>% html_elements("skriftlig_innspill > organisasjon") %>% html_text(),
                     hearing_input_text = tmp %>% html_elements("skriftlig_innspill > tekst") %>% html_text(),
                     hearing_input_title = tmp %>% html_elements("skriftlig_innspill > tittel") %>% html_text())
    
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}


