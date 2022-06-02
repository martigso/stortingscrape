#' Retrieve vote results on MP level for a specified vote
#' 
#' A function for retrieving vote results from a specific vote on MP level. Vote data are only available from the 2011-2012 session.
#' Needs some preprocessing for use with rollcall packages, such as \link[pscl]{ideal}.
#' 
#' @usage get_result_vote(voteid = NA, good_manners = 0)
#' 
#' @param voteid Character string indicating the id of the vote to request all votes from
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                       |                                                                          |
#'    |:----------------------|:-------------------------------------------------------------------------|
#'    | **response_date**     | Date of data retrieval                                                   |
#'    | **version**           | Data version from the API                                                |
#'    | **vote_id**           | Id of vote                                                               |
#'    | **mp_id**             | MP id                                                                    |
#'    | **party_id**          | Party id                                                                 |
#'    | **vote**              | Vote: for, mot (against), ikke_tilstede (absent)                         |
#'    | **permanent_sub_for** | Id of the MP originally holding the seat, if the substitute is permanent |
#'    | **sub_for**           | Id of the MP originally holding the seat                                 |
#'    
#' @md
#' 
#' @seealso [get_decision_votes] [get_proposal_votes] [get_vote] [get_mp_bio]
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' v <- get_result_vote(12345)
#' table(v$vote)
#' 
#' p <- get_proposal_votes(12345)
#' 
#' stringr::str_replace_all(p$proposal_vote$proposal_text, 
#'                          "\\<(.*)\\>|\\r\\n", "")  %>% 
#'   stringr::str_trim()
#' }
#'  
#' @import rvest httr
#' 
#' @export
#' 
get_result_vote <- function(voteid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/voteringsresultat?voteringid=", voteid)
  
  base <- GET(url)
  
  resp <- http_type(base)
  if(resp != "text/xml") stop(paste0("Response of ", url, " is not text/xml."), call. = FALSE)
  
  status <- http_status(base)
  if(status$category != "Success") stop(paste0("Response of ", url, " returned as '", status$message, "'"), call. = FALSE)
  
  tmp <- read_html(base)
  
  if(identical(tmp %>% html_elements("representant_voteringsresultat") %>% html_text(), character()) == FALSE){
    
    tmp2 <- data.frame(response_date = tmp %>% html_elements("voteringsresultat_oversikt > respons_dato_tid") %>% html_text(),
                       version = tmp %>% html_elements("voteringsresultat_oversikt > versjon") %>% html_text(),
                       vote_id = tmp %>% html_elements("voteringsresultat_oversikt > votering_id") %>% html_text(),
                       mp_id = tmp %>% html_elements("representant_voteringsresultat > representant > id") %>% html_text(),
                       party_id = tmp %>% html_elements("representant_voteringsresultat > representant > parti > id") %>% html_text(),
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
                       party_id = NA,
                       vote = NA,
                       permanent_sub_for = NA,
                       sub_for = NA)
  }
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

