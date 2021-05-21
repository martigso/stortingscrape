#' Extract information on specific MPs
#' 
#' A function for retrieving information on Norwegian MPs from the parliament API
#' 
#' @usage get_mp(id = NA)
#' 
#' @param id Character string indicating the id of the MP to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#'
#' @return A data.frame with respnse date, version, date of death/birth, first and last name, id, and gender of the requested MP.
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
#' 
#' @export
#' 



get_mp <- function(id, good_manners = 0){
  
  require(rvest)
  
  tmp <- read_html(paste0("https://data.stortinget.no/eksport/person?personid=", id))
    
  tmp <- data.frame(response_date = tmp %>% html_nodes("respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_nodes("versjon") %>% html_text(),
                    death = tmp %>% html_nodes("doedsdato") %>% html_text(),
                    lastname = tmp %>% html_nodes("etternavn") %>% html_text(),
                    birth = tmp %>% html_nodes("foedselsdato") %>% html_text(),
                    firstname = tmp %>% html_nodes("fornavn") %>% html_text(),
                    id = tmp %>% html_nodes("id") %>% html_text(),
                    gender = tmp %>% html_nodes("kjoenn") %>% html_text())
  
  message(paste0(id, " (", tmp$firstname, " ", tmp$lastname, ") done."))
  
  Sys.sleep(good_manners)
  
  return(tmp)
  
}