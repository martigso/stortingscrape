#' All parliamentary proceedings
#' 
#' A function for retrieving all proceedings in Stortinget, both current and historical.
#' 
#' @usage get_proceedings()
#'  
#' @return A data.frame with response date, version ...
#' 
#' @family get_mp_data
#' 
#' 
#' @examples 
#' 
#' 
#' get_proceedings()
#' 
#' 
#' @import rvest
#' @export
#' 



get_proceedings <- function(){
  
  url <- "https://data.stortinget.no/eksport/saksganger"
  
  tmp <- read_html(url)
  
  tmp2 <- list(root = data.frame(response_date = tmp %>% html_elements("saksgang_oversikt > respons_dato_tid") %>% html_text(),
                                 version = tmp %>% html_elements("saksgang_oversikt > versjon") %>% html_text()),
               proceedings = data.frame(id = tmp %>% html_elements("saksgang > id") %>% html_text(),
                                        name = tmp %>% html_elements("saksgang > navn") %>% html_text()),
               poceedings_steps = data.frame(id = tmp %>% html_elements("saksgang_steg > id") %>% html_text(),
                                        name = tmp %>% html_elements("saksgang_steg > navn") %>% html_text(),
                                        step_number = tmp %>% html_elements("saksgang_steg > steg_nummer") %>% html_text(),
                                        outdated = tmp %>% html_elements("saksgang_steg > uaktuell") %>% html_text()))
  
  # The following will break if the API is updated
  h <- rep(NA, 171)
  
  l <- c(5, 3, 3, 5, 3, 2, 3, 2, 2, 3,
         1, 1, 1, 1, 2, 1, 1, 3, 2, 2,
         4, 4, 5, 4, 1, 6, 6, 5, 5, 6,
         5, 5, 6, 3, 3, 3, 5, 6, 1, 1,
         5, 6, 4, 5, 4, 5, 4, 1, 5, 2) 
  
  for(i in 1:nrow(tmp2$proceedings)){
    # I will not understand this tomorrow:
    h[which(is.na(h))][1:l[i]] <- tmp2$proceedings$id[i]
    # grab only NA from h, copy id to 1:l for each i
  }

  tmp2$poceedings_steps$main_id <- h
  
  return(tmp2)
  
}



