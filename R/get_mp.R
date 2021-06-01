#' Extract information on specific MPs
#' 
#' A function for retrieving information on Norwegian MPs from the parliament API
#' 
#' @usage get_mp(id = NA, good_manners = 0)
#' 
#' @param id Character string indicating the id of the MP to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#'
#' @return A data.frame with response date, version, date of death/birth, first and last name, id, and gender of the requested MP.
#' 
#' @family get_mp_data
#' 
#' 
#' @examples 
#' # Request one MP by id
#' get_mp("AAMH")
#' 
#' # Request several MPs by id
#' ids <- c("AAMH", "AMSK", "MAAA")
#' 
#' mps <- lapply(ids, get_mp, good_manners = 2)
#' 
#' mps <- do.call(rbind, mps)
#' 
#' @import rvest
#' @export
#' 



get_mp <- function(id = NA, good_manners = 0){
  
  tmp <- read_html(paste0("https://data.stortinget.no/eksport/person?personid=", id))
  
  tmp <- data.frame(response_date = tmp %>% html_elements("respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_elements("versjon") %>% html_text(),
                    death = tmp %>% html_elements("doedsdato") %>% html_text(),
                    last_name = tmp %>% html_elements("etternavn") %>% html_text(),
                    birth = tmp %>% html_elements("foedselsdato") %>% html_text(),
                    first_name = tmp %>% html_elements("fornavn") %>% html_text(),
                    id = tmp %>% html_elements("id") %>% html_text(),
                    gender = tmp %>% html_elements("kjoenn") %>% html_text())
  
  message(paste0(id, " (", tmp$first_name, " ", tmp$last_name, ") done."))
  
  Sys.sleep(good_manners)
  
  return(tmp)
  
}