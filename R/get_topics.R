#' Get list of topics and sub-topics for the Norwegian parliament
#' 
#' A function for retrieving topic keys used to label various data from the Norwegian parliament.
#' 
#' @usage get_topics(keep_sub_topics = TRUE)
#' 
#' @param keep_sub_topics Logical. Whether to keep sub-topics (default) for all main topics or not.
#' 
#' 1. **$topics** (All topics)
#' 
#'    |                   |                                                         |
#'    |:------------------|:--------------------------------------------------------|
#'    | **response_date** | Date of data retrieval                                  |
#'    | **version**       | Data version from the API                               |
#'    | **is_main_topic** | Logical indicator for whether the topic is a main topic |
#'    | **main_topic_id** | Id of main topic                                        |
#'    | **id**            | Id of topic                                             |
#'    | **name**          | Name of topic                                           |
#'    
#' 2. **$main_topics** (exclusively main topics, if keep_sub_topics = TRUE)
#' 
#'    |                   |                                                         |
#'    |:------------------|:--------------------------------------------------------|
#'    | **response_date** | Date of data retrieval                                  |
#'    | **version**       | Data version from the API                               |
#'    | **is_main_topic** | Logical indicator for whether the topic is a main topic |
#'    | **main_topic_id** | Id of main topic                                        |
#'    | **id**            | Id of topic                                             |
#'    | **name**          | Name of topic                                           |
#' 
#' 
#' @examples 
#' 
#' # Request the data
#' tops <- get_topics()
#' 
#' # Look at the first main topic
#' tops$main_topics[1, ]
#' 
#' # Extract all sub-topics for the first main topic
#' tops$topics[which(tops$topics$main_topic_id == 5), ]
#' 
#' @import rvest httr
#' 
#' @export
#' 



get_topics <- function(keep_sub_topics = TRUE){
  
  url <- "https://data.stortinget.no/eksport/emner"
  
  base <- GET(url)
  
  resp <- http_type(base)
  if(resp != "text/xml") stop(paste0("Response of ", url, " is not text/xml."), call. = FALSE)
  
  status <- http_status(base)
  if(status$category != "Success") stop(paste0("Response of ", url, " returned as '", status$message, "'"), call. = FALSE)
  
  tmp <- read_html(base)
  
  if(keep_sub_topics == TRUE){
    
    tmp <- list(
      topics = data.frame(
        response_date = tmp %>% html_elements("underemne_liste > emne > respons_dato_tid") %>% html_text(),
        version = tmp %>% html_elements("underemne_liste > emne > versjon") %>% html_text(),
        is_main_topic = tmp %>% html_elements("underemne_liste > emne > er_hovedemne") %>% html_text(),
        main_topic_id = tmp %>% html_elements("underemne_liste > emne > hovedemne_id") %>% html_text(),
        id = tmp %>% html_elements("underemne_liste > emne > id") %>% html_text(),
        name = tmp %>% html_elements("underemne_liste > emne > navn") %>% html_text()),
      
      main_topics = data.frame(
        response_date = tmp %>% html_elements("emne_liste > emne > respons_dato_tid") %>% html_text(),
        version = tmp %>% html_elements("emne_liste > emne > versjon") %>% html_text(),
        is_main_topic = tmp %>% html_elements("emne_liste > emne > er_hovedemne") %>% html_text(),
        main_topic_id = tmp %>% html_elements("emne_liste > emne > hovedemne_id") %>% html_text(),
        id = tmp %>% html_elements("emne_liste > emne > id") %>% html_text(),
        name = tmp %>% html_elements("emne_liste > emne > navn") %>% html_text())
    )
  } 
  
  if(keep_sub_topics == FALSE){
    
    tmp <- data.frame(
      response_date = tmp %>% html_elements("emne_liste > emne > respons_dato_tid") %>% html_text(),
      version = tmp %>% html_elements("emne_liste > emne > versjon") %>% html_text(),
      is_main_topic = tmp %>% html_elements("emne_liste > emne > er_hovedemne") %>% html_text(),
      main_topic_id = tmp %>% html_elements("emne_liste > emne > hovedemne_id") %>% html_text(),
      id = tmp %>% html_elements("emne_liste > emne > id") %>% html_text(),
      name = tmp %>% html_elements("emne_liste > emne > navn") %>% html_text())
    
  }
  
  return(tmp)
  
}