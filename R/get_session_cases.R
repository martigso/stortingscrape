#' Cases in specified session
#' 
#' A function for retrieving all cases treated in a specified parliamentary session.
#' 
#' @usage get_session_cases(sessionid = NA, good_manners = 0, cores = 1)
#' 
#' @param sessionid Character string indicating the id of the parliamentary session to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' @param cores Integer. Number of cores (1 by default) to use in structuring the data. 
#' More than 1 will not work on windows
#' 
#' @return A data.frame with the following variables:
#' 
#' 1. **$root** (main data on the MP)
#' 
#'    |                        |                                    |
#'    |:-----------------------|:-----------------------------------|
#'    | **response_date**      | Date of data retrieval             |
#'    | **version**            | Data version from the API          |
#'    | **treated_session_id** | Session the case was treated in    |
#'    | **document_group**     | Document group the case belongs to |
#'    | **reference**          | Document reference                 |
#'    | **id**                 | Case id                            |
#'    | **com_req_id**         | Committee recommendation id        |
#'    | **com_req_code**       | Committee recommendation code      |
#'    | **title_short**        | Short title of case                |
#'    | **case_filed_id**      | Id of filed case                   |
#'    | **last_update_date**   | Date of last update on case        |
#'    | **status**             | Status of the case                 |
#'    | **title**              | Full title of the case             |
#'    | **type**               | Type of case                       |
#'    | **session_id**         | Session id of the case             |
#'    | **committee_id**       | Responsible committee id           |
#'    
#' 2. **$topics** (named list by case id)
#' 
#'    |                   |                                                        |
#'    |:------------------|:-------------------------------------------------------|
#'    | **is_main_topic** | Logical indication whether the topic is the main topic |
#'    | **main_topic_id** | Id of the main topic for the case                      |
#'    | **id**            | Topic id                                               |
#'    | **name**          | Topic name                                             |
#'    
#' 3. **$proposers** (named list by case id)
#' 
#'    |               |                                                  |
#'    |:--------------|:-------------------------------------------------|
#'    | **rep_id**    | Proposing MP id                                  |
#'    | **county_id** | County id of proposing MP                        |
#'    | **party_id**  | Party id of proposing MP                         |
#'    | **rep_sub**   | Logical indicator for whether MP is a substitute |
#'    
#' 4. **$spokespersons** (data frame by case id)
#' 
#'    |               |                                                  |
#'    |:--------------|:-------------------------------------------------|
#'    | **case_id**   | Case id                                          |
#'    | **rep_id**    | Spokesperson(s) MP id for the case               |
#'    | **county_id** | County id of spokesperson MP                     |
#'    | **party_id**  | Party id of spokesperson MP                      |
#'    | **rep_sub**   | Logical indicator for whether MP is a substitute |
#' 
#' @md
#' 
#' @seealso [get_case] [get_vote]
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' s0506 <- get_session_cases("2005-2006")
#' head(s0506)
#' }
#' 
#' @import rvest parallel httr2
#' @export
#' 
get_session_cases <- function(sessionid = NA, good_manners = 0, cores = 1){
  
  url <- paste0("https://data.stortinget.no/eksport/saker?sesjonid=", sessionid)
  
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
  
  tmp2 <- list(root = data.frame(response_date      = tmp %>% html_elements("saker_oversikt > saker_liste > sak > respons_dato_tid") %>% html_text(),
                                 version            = tmp %>% html_elements("saker_oversikt > saker_liste > sak > versjon") %>% html_text(),
                                 treated_session_id = tmp %>% html_elements("saker_oversikt > saker_liste > sak > behandlet_sesjon_id") %>% html_text(),
                                 document_group     = tmp %>% html_elements("saker_oversikt > saker_liste > sak > dokumentgruppe") %>% html_text(),
                                 reference          = tmp %>% html_elements("saker_oversikt > saker_liste > sak > henvisning") %>% html_text(),
                                 id                 = tmp %>% html_elements("saker_oversikt > saker_liste > sak > id") %>% html_text(),
                                 com_req_id         = tmp %>% html_elements("saker_oversikt > saker_liste > sak > innstilling_id") %>% html_text(), 
                                 com_req_code       = tmp %>% html_elements("saker_oversikt > saker_liste > sak > innstilling_kode") %>% html_text(),
                                 title_short        = tmp %>% html_elements("saker_oversikt > saker_liste > sak > korttittel") %>% html_text(),
                                 case_filed_id      = tmp %>% html_elements("saker_oversikt > saker_liste > sak > sak_fremmet_id") %>% html_text(),
                                 last_update_date   = tmp %>% html_elements("saker_oversikt > saker_liste > sak > sist_oppdatert_dato") %>% html_text(),
                                 status             = tmp %>% html_elements("saker_oversikt > saker_liste > sak > status") %>% html_text(),
                                 title              = tmp %>% html_elements("saker_oversikt > saker_liste > sak > tittel") %>% html_text(),
                                 type               = tmp %>% html_elements("saker_oversikt > saker_liste > sak > type") %>% html_text(),
                                 session_id         = tmp %>% html_elements("saker_oversikt > sesjon_id") %>% html_text()))
  
  
  # Case topics
  tmp2$topics <- mclapply((tmp %>% html_elements("saker_oversikt > saker_liste > sak > emne_liste")), function(x){
    data.frame(is_main_topic = x %>% html_elements("emne > er_hovedemne") %>% html_text(),
               main_topic_id = x %>% html_elements("emne > hovedemne_id") %>% html_text(),
               id = x %>% html_elements("emne > id") %>% html_text(),
               name = x %>% html_elements("emne > navn") %>% html_text())
  }, mc.cores = cores)
  
  names(tmp2$topics) <- tmp2$root$id
  
  # Case proposer
  tmp2$proposers <- mclapply((tmp %>% html_elements("saker_oversikt > saker_liste > sak > forslagstiller_liste")), function(x){
    
    if(identical(x %>% html_elements("representant > id") %>% html_text(), character()) == TRUE){
      data.frame(rep_id = NA,
                 county_id = NA,
                 party_id = NA,
                 rep_sub = NA)
      
    } else {
      
      if(identical((x %>% html_elements("representant > fylke > id") %>% html_text()), character())) {
        county_id = NA
      } else {
        county_id <- x %>% html_elements("representant > fylke > id") %>% html_text()
      }
      
      if(identical(x %>% html_elements("representant > parti > id") %>% html_text(), character())) {
        party_id <- NA
      } else {
        party_id <- x %>% html_elements("representant > parti > id") %>% html_text()
      }
      
      data.frame(rep_id = x %>% html_elements("representant > id") %>% html_text(),
                 county_id = county_id,
                 party_id = party_id,
                 rep_sub = x %>% html_elements("representant > vara_representant") %>% html_text())
    }
  }, mc.cores = cores)
  
  names(tmp2$proposers) <- tmp2$root$id
  
  # Case committee
  committee <- mclapply((tmp %>% html_elements("saker_oversikt > saker_liste > sak")), function(x){
    
    tmp3 <- x %>% html_elements("komite > id") %>% html_text()
    
    if(identical(tmp3, character())) {
      tmp3 <- NA
    }
    
    return(tmp3)
    
  }, mc.cores = cores)
  
  tmp2$root$committee_id <- unlist(committee)
  
  # Case spokesperson
  tmp2$spokespersons <- mclapply((tmp %>% html_elements("saker_oversikt > saker_liste > sak > saksordfoerer_liste")), function(x){
    
    
    if(identical((x %>% html_elements("representant > id") %>% html_text()), character())) {
      rep_id <- NA
    } else {
      rep_id <- x %>% html_elements("representant > id") %>% html_text()
    }
    
    if(identical((x %>% html_elements("representant > fylke > id") %>% html_text()), character())) {
      county_id <- NA
    } else {
      county_id <- x %>% html_elements("representant > fylke > id") %>% html_text()
    }
    
    if(identical((x %>% html_elements("representant > parti > id") %>% html_text()), character())) {
      party_id <- NA
    } else {
      party_id <- x %>% html_elements("representant > parti > id") %>% html_text()
    }
    
    if(identical((x %>% html_elements("representant > vara_representant") %>% html_text()), character())) {
      rep_sub <- NA
    } else {
      rep_sub <- x %>% html_elements("representant > vara_representant") %>% html_text()
    }
    
    
    
    data.frame(rep_id,  
               county_id,
               party_id,
               rep_sub)
    
  }, mc.cores = cores)
  
  names(tmp2$spokespersons) <- tmp2$root$id
  
  tmp2$spokespersons <- do.call(rbind, tmp2$spokespersons)
  tmp2$spokespersons$case_id <- sub("\\.[0-9]+$", "", rownames(tmp2$spokespersons))
  rownames(tmp2$spokespersons) <- 1:nrow(tmp2$spokespersons)
  
  tmp2$spokespersons <-  tmp2$spokespersons[, c("case_id", "rep_id", "county_id", "party_id", "rep_sub")]
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}


