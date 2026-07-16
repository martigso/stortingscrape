#' Get Parliamentary Periods
#' 
#' A function for retrieving dates of the parliamentary periods after WWII
#' 
#' @usage get_parlperiods() 
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
#' @seealso [get_parlsessions]
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' parlper <- get_parlperiods()
#' parlper
#' 
#' }
#' 
#' @import rvest httr2
#' @export
#' 



get_parlperiods <- function(){

  url <- "https://data.stortinget.no/eksport/stortingsperioder"
  
  tmp <- api_get(url)
  
  tmp <- data.frame(response_date = tmp |> html_elements("stortingsperioder_liste > stortingsperiode > respons_dato_tid") |> html_text(),
                    version = tmp |> html_elements("stortingsperioder_liste > stortingsperiode > versjon") |> html_text(),
                    from = tmp |> html_elements("stortingsperioder_liste > stortingsperiode > fra") |> html_text(),
                    id = tmp |> html_elements("stortingsperioder_liste > stortingsperiode > id") |> html_text(),
                    to = tmp |> html_elements("stortingsperioder_liste > stortingsperiode > til") |> html_text())
  tmp$years <- paste(format(as.Date(tmp$from), "%Y"), format(as.Date(tmp$to), "%Y"), sep = "-")
  
  return(tmp)
  
  
}