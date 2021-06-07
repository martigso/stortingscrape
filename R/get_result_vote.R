#' Retreive vote results on MP level for a specified vote
#' 
#' A function for retrieving vote results from a specific vote on MP level. Vote data are only available from the 2011-2012 session
#' 
#' @usage get_result_vote(voteid = NA, good_manners = 0)
#' 
#' @param voteid Character string indicating the id of the vote to request all votes from
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with response date, version ...
#' 
#' @family get_mp_data
#' 
#' @examples 
#' 
#'  
#' @import rvest
#' 
#' @export
#' 



get_result_vote <- function(voteid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/voteringsresultat?voteringid=", voteid)
  
  tmp <- read_html(url)

  if(identical(tmp %>% html_elements("representant_voteringsresultat") %>% html_text(), character()) == FALSE){
    
    tmp2 <- data.frame(response_date = tmp %>% html_elements("voteringsresultat_oversikt > respons_dato_tid") %>% html_text(),
                       version = tmp %>% html_elements("voteringsresultat_oversikt > versjon") %>% html_text(),
                       vote_id = tmp %>% html_elements("voteringsresultat_oversikt > votering_id") %>% html_text(),
                       mp_id = tmp %>% html_elements("representant_voteringsresultat > representant > id") %>% html_text(),
                       vote = tmp %>% html_elements("representant_voteringsresultat > votering") %>% html_text())
    
    
    sub_holder <- lapply(tmp %>% html_elements("representant_voteringsresultat"), function(x){
      x %>% html_elements("fast_vara_for > id") %>% html_text()
    })
    
    tmp2$permanent_sub_for <- unlist(sapply(sub_holder, function(x) ifelse(identical(x, character()), NA, x)))
    
    sub_holder <- lapply(tmp %>% html_elements("representant_voteringsresultat"), function(x){
      x %>% html_elements("vara_for > id") %>% html_text()
    })
    
    tmp2$sub_for <- unlist(sapply(sub_holder, function(x) ifelse(identical(x, character()), NA, x)))
    
  } else {
    tmp2 <- data.frame(response_date = tmp %>% html_elements("voteringsresultat_oversikt > respons_dato_tid") %>% html_text(),
                       version = tmp %>% html_elements("voteringsresultat_oversikt > versjon") %>% html_text(),
                       vote_id = tmp %>% html_elements("voteringsresultat_oversikt > votering_id") %>% html_text(),
                       mp_id = NA,
                       vote = NA,
                       permanent_sub_for = NA,
                       sub_for = NA)
  }
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

