#' Parliamentary committees over all sessions
#'
#' @description Imports data on all committee names and ids over all sessions in the data.stortinget.no API.
#'
#' @usage get_all_committees()
#'
#'
#' @return A data frame of committees, with the variables:
#' 
#' |                   |                           |
#' |:------------------|:--------------------------|
#' | **response_date** | Date of data retrieval    |
#' | **version**       | Data version from the API |
#' | **id**            | Id of the committee       |
#' | **name**          | Name of the committee     |
#' | **sessionid**     | Id of session (empty)     |
#' 
#' @md
#' 
#' @seealso [get_session_committees()]
#'
#'
#' @examples
#' \dontrun{
#' coms <- get_all_committees()
#' head(coms)
#' }
#'
#' @import rvest
#' @export
get_all_committees <- function(){

  url <- "https://data.stortinget.no/eksport/allekomiteer"

  tmp <- read_html(url)

  tmp <- data.frame(response_date = tmp %>% html_elements("komiteer_liste > komite > respons_dato_tid") %>% html_text(),
                    version = tmp %>% html_elements("komiteer_liste > komite > versjon") %>% html_text(),
                    id = tmp %>% html_elements("komiteer_liste > komite > id") %>% html_text(),
                    name = tmp %>% html_elements("komiteer_liste > komite > navn") %>% html_text(),
                    sessionid = tmp %>% html_elements("sesjon_id") %>% html_text())

  return(tmp)

}
