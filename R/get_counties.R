#' Get list of MPs in a given parliamentary period
#' 
#' @description A function for retrieving Norwegian MPs for a given parliamentary 
#' period from the parliament API.
#' 
#' @usage get_counties(historical = FALSE)
#' 
#' @param historical Logical. Whether or not to include historical counties.
#' 
#' @return A data frame with the following variables:
#' 
#' 
#'    |                       |                                                     |
#'    |:----------------------|:----------------------------------------------------|
#'    | **response_date**     | Date of data retrieval                              |
#'    | **version**           | Data version from the API                           |
#'    | **historical_county** | Whether the county is historical (no longer exists) |
#'    | **id**                | Id of the county                                    |
#'    | **name**              | Name of the county                                  |
#' 
#' @md
#' 
#' 
#' 
#' @examples
#' \dontrun{ 
#' # Request one MP by id
#' get_counties()
#' 
#' # With historical counties
#' get_counties(historical = TRUE)
#' }
#' 
#' @import httr2 rvest
#' 
#' @export
#' 
get_counties <- function(historical = FALSE){
  
  
  if(historical == FALSE){
    
    url <- "https://data.stortinget.no/eksport/fylker"
    
    
  } 
  
  if(historical == TRUE){
    
    url <- "https://data.stortinget.no/eksport/fylker/?historiskefylker=true"
    
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
  
  tmp <- data.frame(response_date = tmp %>% html_elements("fylker_liste > fylke > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_elements("fylker_liste > fylke > versjon") %>% html_text(),
                    historical_county = tmp %>% html_elements("fylker_liste > fylke > historisk_fylke") %>% html_text(),
                    id = tmp %>% html_elements("fylker_liste > fylke > id") %>% html_text(),
                    name = tmp %>% html_elements("fylker_liste > fylke > navn") %>% html_text())
  
  return(tmp)
  
}