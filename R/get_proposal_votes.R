#' Retrieve all proposals for a specified vote
#' 
#' A function for retrieving all votes from a specific vote proposal. Vote data are only available from the 2011-2012 session
#' 
#' @usage get_proposal_votes(voteid = NA, good_manners = 0)
#' 
#' @param voteid Character string, or a vector of strings, indicating the id of the vote to retrieve proposals from
#' @param good_manners Numeric. Seconds delay between calls when making multiple calls to the same function. Note that the Stortinget API is limited to 100 calls per minute (see \url{https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/}).
#' 
#' @return A list with two elements:
#' 
#' 1. **$proposal_vote** (main data on the proposals, one row per proposal)
#' 
#'    |                                |                                         |
#'    |:-------------------------------|:----------------------------------------|
#'    | **response_date**              | Date of data retrieval                  |
#'    | **version**                    | Data version from the API               |
#'    | **vote_id**                    | Id of the vote                          |
#'    | **proposal_designation**       | Designation of the proposal             |
#'    | **proposal_designation_short** | Short designation of the proposal       |
#'    | **proposal_id**                | Id of the proposal                      |
#'    | **proposal_delivered_by_mp**   | Id of the MP delivering the proposal    |
#'    | **proposal_on_behalf_of_text** | Text on whose behalf the proposal is    |
#'    | **proposal_sortingnumber**     | Sorting number of the proposal          |
#'    | **proposal_text**              | Text of the proposal                    |
#'    | **proposal_type**              | Type of proposal                        |
#'    
#' 2. **$proposal_by_parties** (a list named by `proposal_id`, each element a character vector of the ids
#'    of the parties behind the proposal)
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

  if(length(voteid) > 1)
    return(fetch_multi(voteid, get_proposal_votes, good_manners, .combine = NULL))
  
  url <- paste0("https://data.stortinget.no/eksport/voteringsforslag?voteringid=", voteid)
  
  tmp <- api_get(url)
  
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

    # Fields are read per proposal, so a field missing from one proposal (e.g. no
    # delivering MP) gives NA rather than shifting the other proposals' values
    proposals <- tmp |> html_elements("voteringsforslag")

    field <- function(xpath) proposals |> html_element(xpath = xpath) |> html_text()

    tmp2 <- list(
      proposal_vote = data.frame(response_date = tmp |> html_elements("voteringsforslag_oversikt > respons_dato_tid") |> html_text(),
                                 version = tmp |> html_elements("voteringsforslag_oversikt > versjon") |> html_text(),
                                 vote_id = tmp |> html_elements("voteringsforslag_oversikt > votering_id") |> html_text(),
                                 proposal_designation = field("./forslag_betegnelse"),
                                 proposal_designation_short = field("./forslag_betegnelse_kort"),
                                 proposal_id = field("./forslag_id"),
                                 proposal_delivered_by_mp = field("./forslag_levert_av_representant/id"),
                                 proposal_on_behalf_of_text = field("./forslag_paa_vegne_av_tekst"),
                                 proposal_sortingnumber = field("./forslag_sorteringsnummer"),
                                 proposal_text = field("./forslag_tekst"),
                                 proposal_type = field("./forslag_type")),
      proposal_by_parties = lapply(proposals, function(x){
        x |> html_elements(xpath = "./forslag_levert_av_parti_liste/parti/id") |> html_text()
      }))

    names(tmp2$proposal_by_parties) <- tmp2$proposal_vote$proposal_id
    
  }
  
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

