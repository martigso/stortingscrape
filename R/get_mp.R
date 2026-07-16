#' Extract information on specific MPs
#' 
#' @description A function for retrieving information on Norwegian MPs from the parliament API
#' 
#' @usage get_mp(mpid = NA, good_manners = 0)
#' 
#' @param mpid Character string, or a vector of strings, indicating the id of the MP to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function. Note that the Stortinget API is limited to 100 calls per minute (see \url{https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/}).
#' 
#'
#' @return A data.frame with the following variables:
#' 
#'    |                   |                                               |
#'    |:------------------|:----------------------------------------------|
#'    | **response_date** | Date of data retrieval                        |
#'    | **version**       | Data version from the API                     |
#'    | **death**         | MP date of death, if applicable               |
#'    | **last_name**     | MP last name                                  |
#'    | **birth**         | MP date of birth                              |
#'    | **first_name**    | MP first name                                 |
#'    | **id**            | MP id                                         |
#'    | **gender**        | MP gender                                     |
#'    
#' @md
#' 
#' @seealso [get_mp_bio] [get_parlperiod_mps] [get_mp_pic] [get_session_mp_speech_activity]
#' 
#' 
#' @examples 
#' \dontrun{
#' # Request one MP by id
#' get_mp("AAMH")
#' 
#' # Request several MPs by id
#' ids <- c("AAMH", "AMSK", "MAAA")
#' 
#' mps <- lapply(ids, get_mp, good_manners = 2)
#' 
#' mps <- do.call(rbind, mps)
#' }
#' 
#' @import rvest httr2
#' @export
#' 
get_mp <- function(mpid = NA, good_manners = 0){

  if(length(mpid) > 1)
    return(fetch_multi(mpid, get_mp, good_manners))

  url <- paste0("https://data.stortinget.no/eksport/person?personid=", mpid)

  tmp <- api_get(url)


  tmp <- data.frame(response_date = tmp |> html_elements("respons_dato_tid") |> html_text(),
                    version = tmp |> html_elements("versjon") |> html_text(),
                    death = tmp |> html_elements("doedsdato") |> html_text(),
                    last_name = tmp |> html_elements("etternavn") |> html_text(),
                    birth = tmp |> html_elements("foedselsdato") |> html_text(),
                    first_name = tmp |> html_elements("fornavn") |> html_text(),
                    id = tmp |> html_elements("id") |> html_text(),
                    gender = tmp |> html_elements("kjoenn") |> html_text())
  
  message(paste0(mpid, " (", tmp$first_name, " ", tmp$last_name, ") done."))
  
  Sys.sleep(good_manners)
  
  return(tmp)
  
}
