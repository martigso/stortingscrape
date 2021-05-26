#' Get list of topics and sub-topics for the Norwegian parliament
#' 
#' A function for retrieving topic keys used to label various data from the Norwegian parliament.
#' 
#' @usage get_topics(keep_sub_topics = TRUE)
#' 
#' @param keep_sub_topics Logical. Whether to keep sub-topics (default) for all main topics or not.
#' 
#' @return A data.frame with response date, version, topic id, main topic id, and name.
#' 
#' @family get_mp_data
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
#' @import rvest
#' 
#' @export
#' 



get_topics <- function(keep_sub_topics = TRUE){
  
  url <- "https://data.stortinget.no/eksport/emner"
  
  tmp <- read_html(url)
  
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