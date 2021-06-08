#' Retrieve written input for a specified hearing
#' 
#' A function for retrieving written input for a specified hearing
#' 
#' @usage get_written_hearing_input(hearingid = NA, good_manners = 0)
#' 
#' @param hearingid Character string indicating the id of the hearing to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with response date, version ...
#' 
#' @family get_mp_data
#' 
#' 
#' @examples 
#' 
#' 
#' 
#' @import rvest parallel
#' @export
#' 



get_written_hearing_input <- function(hearingid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/skriftligInnspill?horingid=", hearingid)
  
  tmp <- try(read_html(url), silent = TRUE)
  
  if(class(tmp)[1] == "try-error"){
    
    tmp2 <- data.frame(response_date = NA,
                       version = NA,
                       hearing_id = hearingid,
                       hearing_type = NA,
                       committee_id = NA,
                       date = NA,
                       id = NA,
                       organization = NA,
                       text = NA,
                       title = NA)
    
  } else {
    
    tmp2 <- data.frame(response_date = tmp %>% html_elements("skriftlige_innspill_oversikt > respons_dato_tid") %>% html_text(),
                       version = tmp %>% html_elements("skriftlige_innspill_oversikt > versjon") %>% html_text(),
                       hearing_id = tmp %>% html_elements("skriftlige_innspill_oversikt > horing_id") %>% html_text(),
                       hearing_type = tmp %>% html_elements("skriftlige_innspill_oversikt > horing_type") %>% html_text(),
                       committee_id = tmp %>% html_elements("skriftlige_innspill_oversikt > komite > id") %>% html_text(),
                       date = ifelse(identical(tmp %>% html_elements("skriftlig_innspill > dato") %>% html_text(), character()), NA,
                                     tmp %>% html_elements("skriftlig_innspill > dato") %>% html_text()),
                       id = ifelse(identical(tmp %>% html_elements("skriftlig_innspill > id") %>% html_text(), character()), NA,
                                   tmp %>% html_elements("skriftlig_innspill > id") %>% html_text()),
                       organization = ifelse(identical(tmp %>% html_elements("skriftlig_innspill > organisasjon") %>% html_text(), character()), NA,
                                             tmp %>% html_elements("skriftlig_innspill > organisasjon") %>% html_text()),
                       text = ifelse(identical(tmp %>% html_elements("skriftlig_innspill > tekst") %>% html_text(), character()), NA,
                                     tmp %>% html_elements("skriftlig_innspill > tekst") %>% html_text()),
                       title = ifelse(identical(tmp %>% html_elements("skriftlig_innspill > tittel") %>% html_text(), character()), NA,
                                      tmp %>% html_elements("skriftlig_innspill > tittel") %>% html_text()))
    
  }
  
  
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}


