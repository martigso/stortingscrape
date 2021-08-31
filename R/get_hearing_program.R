#' Retrieve the hearing program for a specified hearing
#' 
#' @description A function for retrieving the hearing program for a specified hearing. 
#' The earlier periods (around 2005 and back) are less rich with data on some variables
#' 
#' @usage get_hearing_program(hearingid = NA, good_manners = 0)
#' 
#' @param hearingid Character string indicating the id of the hearing to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                              |                                              |
#'    |:-----------------------------|:---------------------------------------------|
#'    | **response_date**            | Date of data retrieval                       |
#'    | **version**                  | Data version from the API                    |
#'    | **hearing_id**               | Id of the hearing                            |
#'    | **hearing_type**             | Type of hearing                              |
#'    | **committee_id**             | Id of committee responsible for the hearing  |
#'    | **hearing_program_date**     | Date hearing program                         |
#'    | **hearing_program_footnote** | Footnote for hearing program                 |
#'    | **order_number**             | Order number for the hearing program element |
#'    | **text**                     | Description of the hearing participant       |
#'    | **time_indication**          | Time stamp for participant hearing input     |
#'    | **date**                     | Date of participant input in hearing         |
#' 
#' @md
#' 
#' @seealso [get_session_hearings] [get_hearing_input] [get_written_hearing_input]
#' 
#' 
#' @examples 
#' \dontrun{
#' s0910 <- get_session_hearings("2009-2010")
#' hearing <- get_hearing_program(s0910$hearing$hearing_id[1])
#' head(hearing)
#' }
#' 
#' 
#' @import rvest httr
#' @export
#' 



get_hearing_program <- function(hearingid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/horingsprogram?horingid=", hearingid)
  
  base <- GET(url)
  
  resp <- http_type(base)
  if(resp != "text/xml") stop(paste0("Response of ", url, " is not text/xml."), call. = FALSE)
  
  status <- http_status(base)
  if(status$category != "Success") stop(paste0("Response of ", url, " returned as '", status$message, "'"), call. = FALSE)
  
  tmp <- read_html(base)
  
  response_date <- tmp %>% html_elements("horingsprogram_oversikt > respons_dato_tid") %>% html_text()
  version <- tmp %>% html_elements("horingsprogram_oversikt > versjon") %>% html_text()
  hearing_id <- tmp %>% html_elements("horingsprogram_oversikt > horing_id") %>% html_text()
  hearing_type <- tmp %>% html_elements("horingsprogram_oversikt > horing_type") %>% html_text()
  committee_id <- tmp %>% html_elements("komite > id") %>% html_text()
  hearing_program_date <- tmp %>% html_elements("horingsprogram > dato") %>% html_text()
  hearing_program_footnote <- tmp %>% html_elements("horingsprogram > fotnote") %>% html_text()
    
  
  hearing_program_participants <- lapply(tmp %>% html_elements("horingsprogram_element_liste"), function(x){
    
    order_number <- x %>% html_elements("horingsprogram_element > rekkefolge_nummer") %>% html_text()
    text <- x %>% html_elements("horingsprogram_element > tekst") %>% html_text()
    time_indication <- x %>% html_elements("horingsprogram_element > tidsangivelse") %>% html_text()
    
    data.frame(order_number = order_number[2:length(order_number)],
               text = text[2:length(text)],
               time_indication = time_indication[2:length(time_indication)],
               date = text[1])
    })

  
  if(identical(hearing_program_participants, list())){
    tmp2 <- data.frame(response_date,
                       version,
                       hearing_id,
                       hearing_type,
                       committee_id,
                       hearing_program_date = ifelse(identical(hearing_program_date, character()), NA, hearing_program_date),
                       hearing_program_footnote = ifelse(identical(hearing_program_footnote, character()), NA, hearing_program_footnote),
                       order_number = NA,
                       text = NA,
                       time_indication = NA,
                       date = NA)
  } else {
    
    for(i in 1:length(hearing_program_participants)){
      hearing_program_participants[[i]]$hearing_program_date <- hearing_program_date[i]
      hearing_program_participants[[i]]$hearing_program_footnote <- hearing_program_footnote[i]
    }
    
    
    tmp2 <- do.call(rbind, hearing_program_participants)
    tmp2 <- data.frame(response_date,
                       version,
                       hearing_id,
                       hearing_type,
                       committee_id,
                       hearing_program_date = tmp2$hearing_program_date,
                       hearing_program_footnote = tmp2$hearing_program_footnote,
                       order_number = tmp2$order_number,
                       text = tmp2$text,
                       time_indication = tmp2$time_indication,
                       date = tmp2$date)
  }
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}


