#' All parliamentary proceedings
#' 
#' A function for retrieving all proceedings in Stortinget, both current and historical.
#' 
#' @usage get_proceedings()
#'  
#' @return A list with three dataframes:
#' 
#' 1. **$root** (only download meta data)
#' 
#'    |                   |                                               |
#'    |:------------------|:----------------------------------------------|
#'    | **response_date** | Date of data retrieval                        |
#'    | **version**       | Data version from the API                     |
#'  
#' 2. $proceedings (description of main proceeding categories)
#' 
#'    |          |                    |
#'    |:---------|:-------------------|
#'    | **id**   | Id of proceeding   |
#'    | **name** | Name of proceeding |
#'    
#' 2. $poceedings_steps (description of proceeding steps within each main category)
#' 
#'    |                 |                                            |
#'    |:----------------|:-------------------------------------------|
#'    | **id**          | Id of proceeding step                      |
#'    | **name**        | Name of proceeding step                    |
#'    | **step_number** | Order of proceeding steps                  |
#'    | **outdated**    | Whether the step is outdated               |
#'    | **main_id**     | Id for proceeding type the step belongs to |
#'    
#' @md
#' 
#' @examples 
#' \dontrun{
#' 
#' get_proceedings()
#' 
#' }
#' 
#' @import rvest
#' @export
#' 



get_proceedings <- function(){
  
  url <- "https://data.stortinget.no/eksport/saksganger"
  
  base <- GET(url)
  
  resp <- http_type(base)
  if(resp != "text/xml") stop(paste0("Response of ", url, " is not text/xml."), call. = FALSE)
  
  status <- http_status(base)
  if(status$category != "Success") stop(paste0("Response of ", url, " returned as '", status$message, "'"), call. = FALSE)
  
  tmp <- read_html(base)
  
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



