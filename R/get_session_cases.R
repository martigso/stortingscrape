#' Cases in specified session
#' 
#' A function for retrieving all cases treated in a specified parliamentary session
#' 
#' @usage get_session_cases(sessionid = NA, good_manners = 0, cores = 1)
#' 
#' @param sessionid Character string indicating the id of the parliamentary session to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' @param cores Integer. Number of cores (1 by default) to use in structuring the data. 
#' More than 1 will not work on windows
#' @return A data.frame with response date, version ...
#' 
#' @family get_mp_data
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' s0506 <- get_session_cases("2005-2006")
#' head(s0506)
#' }
#' 
#' 
#' 
#' @import rvest parallel
#' @export
#' 



get_session_cases <- function(sessionid = NA, good_manners = 0, cores = 1){
  
  url <- paste0("https://data.stortinget.no/eksport/saker?sesjonid=", sessionid)
  
  tmp <- read_html(url)
  
  tmp2 <- list(root = data.frame(response_date = tmp %>% html_elements("saker_oversikt > saker_liste > sak > respons_dato_tid") %>% html_text(),
                                 version = tmp %>% html_elements("saker_oversikt > saker_liste > sak > versjon") %>% html_text(),
                                 treated_session_id = tmp %>% html_elements("saker_oversikt > saker_liste > sak > behandlet_sesjon_id") %>% html_text(),
                                 document_group = tmp %>% html_elements("saker_oversikt > saker_liste > sak > dokumentgruppe") %>% html_text(),
                                 reference = tmp %>% html_elements("saker_oversikt > saker_liste > sak > henvisning") %>% html_text(),
                                 id = tmp %>% html_elements("saker_oversikt > saker_liste > sak > id") %>% html_text(),
                                 com_req_id = tmp %>% html_elements("saker_oversikt > saker_liste > sak > innstilling_id") %>% html_text(), 
                                 com_req_code = tmp %>% html_elements("saker_oversikt > saker_liste > sak > innstilling_kode") %>% html_text(),
                                 title_short = tmp %>% html_elements("saker_oversikt > saker_liste > sak > korttittel") %>% html_text(),
                                 case_filed_id = tmp %>% html_elements("saker_oversikt > saker_liste > sak > sak_fremmet_id") %>% html_text(),
                                 last_update_date = tmp %>% html_elements("saker_oversikt > saker_liste > sak > sist_oppdatert_dato") %>% html_text(),
                                 status = tmp %>% html_elements("saker_oversikt > saker_liste > sak > status") %>% html_text(),
                                 title = tmp %>% html_elements("saker_oversikt > saker_liste > sak > tittel") %>% html_text(),
                                 type = tmp %>% html_elements("saker_oversikt > saker_liste > sak > type") %>% html_text(),
                                 session_id = tmp %>% html_elements("saker_oversikt > sesjon_id") %>% html_text()))
  
  
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
      
      data.frame(rep_id = x %>% html_elements("representant > id") %>% html_text(),
                 county_id = ifelse(identical(x %>% html_elements("representant > fylke > id") %>% html_text(), character()), NA,
                                    x %>% html_elements("representant > fylke > id") %>% html_text()),
                 party_id = ifelse(identical(x %>% html_elements("representant > parti > id") %>% html_text(), character()), NA,
                                   x %>% html_elements("representant > parti > id") %>% html_text()),
                 rep_sub = x %>% html_elements("representant > vara_representant") %>% html_text())
    }
  }, mc.cores = cores)
  
  names(tmp2$proposers) <- tmp2$root$id
  
  # Case committee
  committee <- mclapply((tmp %>% html_elements("saker_oversikt > saker_liste > sak")), function(x){
    
    tmp3 <- x %>% html_elements("komite > id") %>% html_text()
    
    tmp3 <- ifelse(identical(tmp3, character()), NA, tmp3)
    
    return(tmp3)
    
  }, mc.cores = cores)
  
  tmp2$root$committee_id <- unlist(committee)
  
  # Case spokesperson
  tmp2$spokespersons <- mclapply((tmp %>% html_elements("saker_oversikt > saker_liste > sak > saksordfoerer_liste")), function(x){
    
    data.frame(rep_id = x %>% html_elements("representant > id") %>% html_text(),
               county_id = x %>% html_elements("representant > fylke > id") %>% html_text(),
               party_id = x %>% html_elements("representant > parti > id") %>% html_text(),
               rep_sub = x %>% html_elements("representant > vara_representant") %>% html_text())
    
  }, mc.cores = cores)
  
  names(tmp2$proposers) <- tmp2$root$id
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}


