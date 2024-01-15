#' Retrieve hearings in specified session
#' 
#' A function for retrieving all hearings in a specified parliamentary session.
#' 
#' @usage get_session_hearings(sessionid = NA, good_manners = 0, cores = 1)
#' 
#' @param sessionid Character string indicating the id of the parliamentary session to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' @param cores Integer...
#' 
#' @return A list with four elements:
#' 
#' 1. **$root** (hearing meta data)
#' 
#'    |                      |                           |
#'    |:---------------------|:--------------------------|
#'    | **response_date**    | Date of data retrieval    |
#'    | **version**          | Data version from the API |
#'    | **session_id**       | Session id                |
#'    
#' 2. **$hearing** (main data on the hearing)
#' 
#'    |                          |                                                     |
#'    |:-------------------------|:----------------------------------------------------|
#'    | **deadline_date**        | Deadline date for hearing                           |
#'    | **status**               | Data version from the API                           |
#'    | **hearing_id**           | Hearing id                                          |
#'    | **input_deadline**       | Deadline date for input                             |
#'    | **written**              | Logical indication of whether the input was written |
#'    | **application_deadline** | Deadline date for application to hearing            |
#'    | **start_date**           | Start date for hearing                              |
#'    | **status_pub**           | Publication status for hearing                      |
#'    | **status_info_text**     | Status information text                             |
#'    | **type**                 | Type of hearing                                     |
#'    | **committee_id**         | Committee id for committee responsible for hearing  |
#'    
#' 3. **$hearing_case_info** (named list by hearing id with information on the case(s) belonging to the hearing)
#' 
#'    |                      |                                        |
#'    |:---------------------|:---------------------------------------|
#'    | **hearing_id**       | Hearing id                             |
#'    | **case_reference**   | Text reference for case                |
#'    | **case_id**          | Case id                                |
#'    | **case_short_title** | Short title for case                   |
#'    | **case_publication** | URL for front end web-page publication |
#'    | **case_title**       | Full title for case                    |
#'    
#' 4. **$hearing_date** (named list by hearing id with date(s) the hearing was held)
#' 
#'    |                      |                            |
#'    |:---------------------|:---------------------------|
#'    | **hearing_id**       | Hearing id                 |
#'    | **date**             | Date of hearing            |
#'    | **place**            | Where the hearing was held |
#' 
#' @md
#' 
#' @seealso [get_hearing_input] [get_hearing_program] [get_written_hearing_input]
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' hear <- get_session_hearings("2010-2011")
#' head(hear$hearing)
#' 
#' }
#' 
#' 
#' @import rvest parallel httr2
#' @export
#' 



get_session_hearings <- function(sessionid = NA, good_manners = 0, cores = 1){
  
  url <- paste0("https://data.stortinget.no/eksport/horinger?sesjonid=", sessionid)
  
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
  
  
  tmp2 <- list(
    root = data.frame(
      response_date = tmp |> html_elements("horinger_oversikt > respons_dato_tid") |> html_text(),
      version = tmp |> html_elements("horinger_oversikt > versjon") |> html_text(),
      session_id = tmp |> html_elements("horinger_oversikt > sesjon_id") |> html_text()
    ),
    hearing = data.frame(
      deadline_date = tmp |> html_elements("horing > anmodningsfrist_dato_tid") |> html_text(),
      status = tmp |> html_elements("horing > horing_status") |> html_text(),
      hearing_id = tmp |> html_elements("horing > id") |> html_text(),
      input_deadline = tmp |> html_elements("horing > innspillsfrist") |> html_text(),
      written = tmp |> html_elements("horing > skriftlig") |> html_text(),
      application_deadline = tmp |> html_elements("horing > soknadfrist_dato") |> html_text(),
      start_date = tmp |> html_elements("horing > start_dato") |> html_text(),
      status_pub = tmp |> html_elements("horing > status") |> html_text(),
      status_info_text = tmp |> html_elements("horing > status_info_tekst") |> html_text(),
      type = tmp |> html_elements("horing > type") |> html_text(),
      committee_id = tmp |> html_elements("horing > komite > id") |> html_text()
    ))
  
  tmp2$hearing_case_info <- mclapply(tmp |> html_elements("horing_sak_info_liste"), function(x){
    data.frame(case_reference = x |> html_elements("horing_sak_info > sak_henvisning") |> html_text(),
               case_id = x |> html_elements("horing_sak_info > sak_id") |> html_text(),
               case_short_title = x |> html_elements("horing_sak_info > sak_korttittel") |> html_text(),
               case_publication = x |> html_elements("horing_sak_info > sak_publikasjon") |> html_text(),
               case_title = x |> html_elements("horing_sak_info > sak_tittel") |> html_text())
  }, mc.cores = cores)
  
  names(tmp2$hearing_case_info) <- tmp2$hearing$hearing_id
  
  tmp2$hearing_case_info <- do.call(rbind, tmp2$hearing_case_info)
  
  tmp2$hearing_case_info$hearing_id <- sub("\\.[0-9]+$", "", rownames(tmp2$hearing_case_info))
  rownames(tmp2$hearing_case_info) <- 1:nrow(tmp2$hearing_case_info)
  tmp2$hearing_case_info <- tmp2$hearing_case_info[, c("hearing_id", "case_reference", "case_id", "case_short_title", 
                                                       "case_publication", "case_title")]
  
  tmp2$hearing_date <- mclapply(tmp |> html_elements("horingstidspunkt_liste"), function(x){
    data.frame(place = x |> html_elements("horingstidspunkt > sted") |> html_text(),
               date = x |> html_elements("horingstidspunkt > tidspunkt") |> html_text())
  })
  
  names(tmp2$hearing_date) <- tmp2$hearing$hearing_id
  
  tmp2$hearing_date <- do.call(rbind, tmp2$hearing_date)
  
  tmp2$hearing_date$hearing_id <- sub("\\.[0-9]+$", "", rownames(tmp2$hearing_date))
  rownames(tmp2$hearing_date) <- 1:nrow(tmp2$hearing_date)
  tmp2$hearing_date <- tmp2$hearing_date[, c("hearing_id", "date", "place")]
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}


