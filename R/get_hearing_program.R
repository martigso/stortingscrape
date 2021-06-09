#' Retrieve the hearing program for a specified hearing
#' 
#' A function for retrieving the hearing program for a specified hearing
#' 
#' @usage get_hearing_program(hearingid = NA, good_manners = 0)
#' 
#' @param hearingid Character string indicating the id of the hearing to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#' @return A data.frame with response date, version ...
#' 
#' @family get_mp_data
#' 
#' 
#' @examples 
#' 
#' 
#' 
#' @import rvest parallel
#' @export
#' 



get_hearing_program <- function(hearingid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/horingsprogram?horingid=", hearingid)
  
  # tmp <- try(read_html(url), silent = TRUE)
  tmp <- read_html(url)
  # if(class(tmp)[1] == "try-error"){
  #   
  #   tmp2 <- data.frame(response_date = NA,
  #                      version = NA,
  #                      hearing_id = hearingid,
  #                      hearing_type = NA,
  #                      committee_id = NA,
  #                      date = NA,
  #                      id = NA,
  #                      organization = NA,
  #                      text = NA,
  #                      title = NA)
  #   
  # } else {
  #   
  tmp2 <- list(
    root = data.frame(response_date = tmp %>% html_elements("horingsprogram_oversikt > respons_dato_tid") %>% html_text(),
                      version = tmp %>% html_elements("horingsprogram_oversikt > versjon") %>% html_text(),
                      hearing_id = tmp %>% html_elements("horingsprogram_oversikt > horing_id") %>% html_text(),
                      hearing_type = tmp %>% html_elements("horingsprogram_oversikt > horing_type") %>% html_text(),
                      committee_id = tmp %>% html_elements("komite > id") %>% html_text(),
                      hearing_program_date = ifelse(identical(tmp %>% html_elements("horingsprogram > dato") %>% html_text(), character()),
                                                    NA, tmp %>% html_elements("horingsprogram > dato") %>% html_text()),
                      hearing_program_footnote = ifelse(identical(tmp %>% html_elements("horingsprogram > fotnote") %>% html_text(), character()),
                                                        NA, tmp %>% html_elements("horingsprogram > fotnote") %>% html_text())),
    hearing_program_participants = lapply(tmp %>% html_elements("horingsprogram_element_liste"), function(x){
      data.frame(order_number = x %>% html_elements("horingsprogram_element > rekkefolge_nummer") %>% html_text(),
                 text = x %>% html_elements("horingsprogram_element > tekst") %>% html_text(),
                 time_indication = x %>% html_elements("horingsprogram_element > tidsangivelse") %>% html_text()
      )
    })
  )
  
  if(identical(tmp2$hearing_program_participants, list())){
    tmp2$hearing_program_participants <- list(data.frame(order_number = NA,
                                                         text = NA,
                                                         time_indication = NA))
    names(tmp2$hearing_program_participants) <- "missing_date"
  } else {
    names(tmp2$hearing_program_participants) <- tmp2$root$hearing_program_date  
  }
  
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}


