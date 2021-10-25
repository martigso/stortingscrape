#' Retreive a parliamentary case
#' 
#' @description A function for retrieving single parliamentary case by id.
#' 
#' @usage get_case(caseid = NA, good_manners = 0)
#' 
#' @param caseid Character string indicating the id of the case to request
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A list with seven data frame elements:
#' 
#' 1. **$root** (main data on the case)
#' 
#'    |                      |                                   |
#'    |:---------------------|:----------------------------------|
#'    | **response_date**    | Date of data retrieval            |
#'    | **version**          | Data version from the API         |
#'    | **document_group**   | Case document group type          |
#'    | **finalized**        | Whether the case finalized        |
#'    | **reference**        | Relevant publication references   |
#'    | **id**               | Case id                           |
#'    | **req_text**         | Recommendation (proposal) text    |
#'    | **committee_id**     | Id of committee handling the case |
#'    | **title_short**      | Case short title                  |
#'    | **decision_short**   | Case decision_short               |
#'    | **parenthesis_text** | Case parenthesis text             |
#'    | **case_number**      | Case number                       |
#'    | **session_id**       | Session id                        |
#'    | **proceedings_id**   | Type of proceeding id             |
#'    | **proceedings_name** | Type of proceeding name           |
#'    | **status**           | Status for case                   |
#'    | **title**            | Case title (long)                 |
#'    | **type**             | Case type                         |
#'    | **decision_text**    | Decision text                     |
#'    
#' 2. **$topic** (the topics related to the case)
#' 
#'    |                   |                               |
#'    |:------------------|:------------------------------|
#'    | **is_main_topic** | Is this (row) the main topic? |
#'    | **main_topic_id** | Id for main topic             |
#'    | **id**            | Topic id                      |
#'    | **navn**          | Topic name                    |
#' 
#' 3. **$publication_references** (references for publications on the case)
#' 
#'    |                   |                                                     |
#'    |:------------------|:----------------------------------------------------|
#'    | **export_id** | Id for export of publication (used in ?get_publication) |
#'    | **link_text** | Publication title                                       |
#'    | **link_url**  | URL to publication                                      |
#'    | **type**      | Publication type                                        |
#'    | **subtype**   | Publication subtype (chamber)                           |
#'    
#' 4. **$proposers** (MPs behind case proposal, when relevant)
#' 
#'    |              |                             |
#'    |:-------------|:----------------------------|
#'    | **mp_id**    | MPs id                      |
#'    | **party_id** | Party id of MPs             |
#'    | **sub_mp**   | Whether MPs are substitutes |
#'    
#' 5. **$proceeding_steps** (case proceeding steps)
#' 
#'    |                 |                                   |
#'    |:----------------|:----------------------------------|
#'    | **step_name**   | Name of steps                     |
#'    | **step_number** | Step order for case               |
#'    | **outdated**    | Whether the step type is outdated |
#' 
#' 6. **$spokespersons** (all MPs that are spokespersons for the case)
#' 
#'    |              |                             |
#'    |:-------------|:----------------------------|
#'    | **mp_id**    | MPs id                      |
#'    | **party_id** | Party id of MPs             |
#'    | **sub_mp**   | Whether MPs are substitutes |
#'     
#' 7. **$keywords** (all keywords associated with the case)
#' 
#'    |            |                       |
#'    |:-----------|:----------------------|
#'    | **keyword**| Keywords for the case |
#'    
#' @md
#' 
#' @seealso [get_session_cases]
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # Get one case
#' case <- get_case("30233")
#' case
#' 
#' # Get multiple cases
#' cases <- lapply(c("30233", "30362", "30234", "30236"), get_case, good_manners = 2)
#' cases_root <- lapply(cases, function(x) x$root)
#' cases_root <- do.call(rbind, cases_root)
#' cases_root
#' 
#' cases_keywords <- lapply(1:nrow(cases_root), function(x){
#'   tmp <- cases[[x]]$keywords
#'   tmp$case_id <- cases_root$id[x]
#'   return(tmp)
#' })
#' cases_keywords <- do.call(rbind, cases_keywords)
#' cases_keywords
#' 
#' }
#' @import httr rvest
#' 
#' @export
get_case <- function(caseid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/sak?sakid=", caseid)
  
  base <- GET(url)
  
  resp <- http_type(base)
  if(resp != "text/xml") stop(paste0("Response of ", url, " is not text/xml."), call. = FALSE)
  
  status <- http_status(base)
  if(status$category != "Success") stop(paste0("Response of ", url, " returned as '", status$message, "'"), call. = FALSE)
  
  tmp <- read_html(base)
  
  tmp2 <- list(root = data.frame(response_date = tmp %>% html_elements("detaljert_sak > respons_dato_tid") %>% html_text(),
                                 version = tmp %>% html_elements("detaljert_sak > versjon") %>% html_text(),
                                 document_group = tmp %>% html_elements("detaljert_sak > dokumentgruppe") %>% html_text(),
                                 finalized = tmp %>% html_elements("ferdigbehandlet") %>% html_text(),
                                 reference = tmp %>% html_elements("henvisning") %>% html_text(), 
                                 id = tmp %>% html_elements("detaljert_sak > id") %>% html_text(),
                                 req_text = tmp %>% html_elements("innstillingstekst") %>% html_text(),
                                 committee_id = ifelse(identical(tmp %>% html_elements("komite > id") %>% html_text(), character()), "",
                                                       tmp %>% html_elements("komite > id") %>% html_text()),
                                 title_short = tmp %>% html_elements("korttittel") %>% html_text(),
                                 decision_short = tmp %>% html_elements("kortvedtak") %>% html_text(),
                                 parenthesis_text = tmp %>% html_elements("parentestekst") %>% html_text(),
                                 case_number = tmp %>% html_elements("sak_nummer") %>% html_text(),
                                 session_id = tmp %>% html_elements("sak_sesjon") %>% html_text(),
                                 proceedings_id = tmp %>% html_elements("saksgang > id") %>% html_text(),
                                 proceedings_name = tmp %>% html_elements("saksgang > navn") %>% html_text(),
                                 status = tmp %>% html_elements("detaljert_sak > status") %>% html_text(),
                                 title = tmp %>% html_elements("detaljert_sak > tittel") %>% html_text(),
                                 type = tmp %>% html_elements("detaljert_sak > type") %>% html_text(),
                                 decision_text = tmp %>% html_elements("detaljert_sak > vedtakstekst") %>% html_text()),
               topic = data.frame(is_main_topic = tmp %>% html_elements("emne > er_hovedemne") %>% html_text(),
                                  main_topic_id = tmp %>% html_elements("emne > hovedemne_id") %>% html_text(),
                                  id = tmp %>% html_elements("emne > id") %>% html_text(),
                                  navn = tmp %>% html_elements("emne > navn") %>% html_text()),
               publication_references = data.frame(export_id = tmp %>% html_elements("publikasjon_referanse > eksport_id") %>% html_text(),
                                                   link_text = tmp %>% html_elements("publikasjon_referanse > lenke_tekst") %>% html_text(),
                                                   link_url = tmp %>% html_elements("publikasjon_referanse > lenke_url") %>% html_text(),
                                                   type = tmp %>% html_elements("publikasjon_referanse > type") %>% html_text(),
                                                   subtype = tmp %>% html_elements("publikasjon_referanse > undertype") %>% html_text()),
               proposers = data.frame(mp_id = tmp %>% html_elements("forslagstiller_liste > representant > id") %>% html_text(),
                                      party_id = ifelse(identical(tmp %>% html_elements("forslagstiller_liste > representant > parti > id") %>% html_text(), character()),
                                                        NA, tmp %>% html_elements("forslagstiller_liste > representant > parti > id") %>% html_text()),
                                      sub_mp = tmp %>% html_elements("forslagstiller_liste > representant > vara_representant") %>% html_text()),
               proceeding_steps = data.frame(step_name = tmp %>% html_elements("saksgang_steg > navn") %>% html_text(),
                                             step_number = tmp %>% html_elements("saksgang_steg > steg_nummer") %>% html_text(),
                                             outdated = tmp %>% html_elements("saksgang_steg > uaktuell") %>% html_text()),
               spokespersons = data.frame(mp_id = tmp %>% html_elements("saksordfoerer_liste > representant > id") %>% html_text(),
                                          party_id = tmp %>% html_elements("saksordfoerer_liste > representant > parti > id") %>% html_text(),
                                          sub_mp = tmp %>% html_elements("saksordfoerer_liste > representant > vara_representant") %>% html_text()),
               keywords = data.frame(keyword = tmp %>% html_elements("stikkord_liste > string") %>% html_text()))
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}

