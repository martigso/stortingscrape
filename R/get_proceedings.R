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
#' @import rvest httr2
#' @export
#' 



get_proceedings <- function(){
  
  url <- "https://data.stortinget.no/eksport/saksganger"

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
  
  tmp2 <- list(root = data.frame(response_date = tmp %>% html_elements("saksgang_oversikt > respons_dato_tid") %>% html_text(),
                                 version = tmp %>% html_elements("saksgang_oversikt > versjon") %>% html_text()),
               proceedings = data.frame(id = tmp %>% html_elements("saksgang > id") %>% html_text(),
                                        name = tmp %>% html_elements("saksgang > navn") %>% html_text()),
               poceedings_steps = data.frame(id = tmp %>% html_elements("saksgang_steg > id") %>% html_text(),
                                        name = tmp %>% html_elements("saksgang_steg > navn") %>% html_text(),
                                        step_number = tmp %>% html_elements("saksgang_steg > steg_nummer") %>% html_text(),
                                        outdated = tmp %>% html_elements("saksgang_steg > uaktuell") %>% html_text()))
  
  # Because the only indicator for matching main id with
  # sub-ids is the list order, we need to match it by when
  # step_number is reset to 1. This is a more scalable solution
  # than in previous versions of the package (< 0.2.0).
  
  tmp2$poceedings_steps$main_id <- 1
  
  for(i in 2:nrow(tmp2$poceedings_steps)) {
    
    # if i - 1 is bigger than i ...
    if(tmp2$poceedings_steps$step_number[i] > tmp2$poceedings_steps$step_number[i - 1]) {
      
      # give i and i - 1 the same id
      tmp2$poceedings_steps$main_id[i] <- tmp2$poceedings_steps$main_id[i - 1]
      
    } else {
      
      # else, increase id by 1
      tmp2$poceedings_steps$main_id[i] <- tmp2$poceedings_steps$main_id[i - 1] + 1
      
    }
  
  }
  
  main_id_holder <- data.frame(tmp_id = unique(tmp2$proceedings$id),
                               main_id = unique(tmp2$poceedings_steps$main_id))
  
  tmp2$poceedings_steps <- merge(x = tmp2$poceedings_steps, 
                                 y = main_id_holder, 
                                 by = c("main_id"), 
                                 all.x = TRUE)
  
  
  tmp2$poceedings_steps$main_id <- tmp2$poceedings_steps$tmp_id

  keep_vars <- c("id", "name", "step_number", "outdated", "main_id")
  tmp2$poceedings_steps <- tmp2$poceedings_steps[, keep_vars]
  
  return(tmp2)
  
}



