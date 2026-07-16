#' Get Parliamentary Sessions
#' 
#' A function for retrieving dates of the parliamentary sessions after WWII
#' 
#' @usage get_parlsessions()
#' 
#' @return A data.frame with the following variables:
#' 
#'    |                   |                                               |
#'    |:------------------|:----------------------------------------------|
#'    | **response_date** | Date of data retrieval                        |
#'    | **version**       | Data version from the API                     |
#'    | **from**          | Date session started                          |
#'    | **id**            | Id of for session (used for other functions)  |
#'    | **to**            | Date session ended                            |
#'    | **years**         | From year to year in full format              |
#'    
#' @md
#'    
#' @seealso [get_parlperiods]
#' 
#' 
#' @examples 
#' \dontrun{
#' 
#' parlses <- get_parlsessions()
#' parlses
#' 
#' }
#' 
#' @import rvest httr2
#' 
#' @export
#' 
get_parlsessions <- function(){
  
  url <- "https://data.stortinget.no/eksport/sesjoner"
  
  tmp <- api_get(url)
  
  tmp <- data.frame(response_date = tmp |> html_elements("sesjoner_liste > sesjon > respons_dato_tid") |> html_text(),
                    version = tmp |> html_elements("sesjoner_liste > sesjon > versjon") |> html_text(),
                    from = tmp |> html_elements("sesjoner_liste > sesjon > fra") |> html_text(),
                    id = tmp |> html_elements("sesjoner_liste > sesjon > id") |> html_text(),
                    to = tmp |> html_elements("sesjoner_liste > sesjon > til") |> html_text())

  tmp$years <- paste(format(as.Date(tmp$from), "%Y"), format(as.Date(tmp$to), "%Y"), sep = "-")
  
  return(tmp)
  
}