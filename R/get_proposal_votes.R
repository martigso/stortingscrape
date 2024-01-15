#' Retreive all votes for a specified vote proposal
#' 
#' A function for retrieving all votes from a specific vote proposal. Vote data are only available from the 2011-2012 session
#' 
#' @usage get_proposal_votes(voteid = NA, good_manners = 0)
#' 
#' @param voteid Character string indicating the id of the vote to request all votes from
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A list with two elements:
#' 
#' 1. **$proposal_vote** (main data on the vote proposal)
#' 
#'    |                      |                           |
#'    |:---------------------|:--------------------------|
#'    | **response_date**    | Date of data retrieval    |
#'    | **version**          | Data version from the API |
#'    | **vote_id**          | Id of the vote            |
#'    
#' 2. **$proposal_by_parties${proposal_id}** (what parties (id) stood behind proposal(s))
#'    
#' @md
#' 
#' @seealso [get_vote] [get_decision_votes] [get_result_vote]
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' prop <- get_proposal_votes(7523)
#' prop
#' 
#' for(i in 1:length(prop$proposal_by_parties)){
#'     prop$proposal_vote$parties[i] <- paste0(prop$proposal_by_parties[[i]], 
#'                                             collapse = ", ")
#'
#' }
#' 
#' }
#' 
#' 
#' @import rvest httr2
#' 
#' @export
#' 
get_proposal_votes <- function(voteid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/voteringsforslag?voteringid=", voteid)
  
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
  
  if(identical(tmp |> html_elements("voteringsforslag > forslag_id") |> html_text(), character())){
    tmp2 <- list(
      proposal_vote = data.frame(response_date = tmp |> html_elements("voteringsforslag_oversikt > respons_dato_tid") |> html_text(),
                                 version = tmp |> html_elements("voteringsforslag_oversikt > versjon") |> html_text(),
                                 vote_id = tmp |> html_elements("voteringsforslag_oversikt > votering_id") |> html_text(),
                                 proposal_designation = NA,
                                 proposal_designation_short = NA,
                                 proposal_id = NA,
                                 proposal_delivered_by_mp = NA,
                                 proposal_on_behalf_of_text = NA,
                                 proposal_sortingnumber = NA,
                                 proposal_text = NA,
                                 proposal_type = NA),
      proposal_by_parties = NA)
    
    
    names(tmp2$proposal_by_parties) <- tmp2$proposal_vote$proposal_id
    
  } else {
    tmp2 <- list(
      proposal_vote = data.frame(response_date = tmp |> html_elements("voteringsforslag_oversikt > respons_dato_tid") |> html_text(),
                                 version = tmp |> html_elements("voteringsforslag_oversikt > versjon") |> html_text(),
                                 vote_id = tmp |> html_elements("voteringsforslag_oversikt > votering_id") |> html_text(),
                                 proposal_designation = tmp |> html_elements("voteringsforslag > forslag_betegnelse") |> html_text(),
                                 proposal_designation_short = tmp |> html_elements("voteringsforslag > forslag_betegnelse_kort") |> html_text(),
                                 proposal_id = tmp |> html_elements("voteringsforslag > forslag_id") |> html_text(),
                                 proposal_delivered_by_mp = tmp |> html_elements("voteringsforslag > forslag_levert_av_representant") |> html_text(),
                                 proposal_on_behalf_of_text = tmp |> html_elements("voteringsforslag > forslag_paa_vegne_av_tekst") |> html_text(),
                                 proposal_sortingnumber = tmp |> html_elements("voteringsforslag > forslag_sorteringsnummer") |> html_text(),
                                 proposal_text = tmp |> html_elements("voteringsforslag > forslag_tekst") |> html_text(),
                                 proposal_type = tmp |> html_elements("voteringsforslag > forslag_type") |> html_text()),
      proposal_by_parties = tmp |> html_elements("forslag_levert_av_parti_liste"))
    
    tmp2$proposal_by_parties <- lapply(tmp2$proposal_by_parties, function(x){
      x |> html_elements("parti > id") |> html_text()
    })
    
    names(tmp2$proposal_by_parties) <- tmp2$proposal_vote$proposal_id
    
  }
  
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

