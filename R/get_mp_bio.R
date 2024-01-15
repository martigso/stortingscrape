#' Extract biography of specific MPs
#' 
#' @description A function for retrieving biography of Norwegian MPs from the parliament API
#' 
#' @usage get_mp_bio(mpid = NA, good_manners = 0)
#' 
#' @param mpid Character string indicating the id of the MP to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#'
#' @return A list with ten data frames:
#' 
#' 1. **$root** (main data on the MP)
#' 
#'    |                      |                           |
#'    |:---------------------|:--------------------------|
#'    | **response_date**    | Date of data retrieval    |
#'    | **version**          | Data version from the API |
#'    | **id**               | Id of the MP              |
#'    
#' 2. **$literature** (all literature the MP contributed to)
#' 
#'    |                   |                                       |
#'    |:------------------|:--------------------------------------|
#'    | **year**          | Year of entry publication             |
#'    | **description**   | Description of the publication        |
#'    | **last_name**     | MP's last name                        |
#'    | **more_years**    | __Not described in the API__          |
#'    | **publisher**     | Publisher                             |
#'    | **first_name**    | First name of the MP                  |
#'    | **place**         | Place of publication                  |
#'    | **title**         | Title of the publication              |
#'    | **type**          | MP's role in publication (author etc) |
#'    
#' 3. **$leave_of_absence** (times the MP was on leave)
#'  
#'    |                    |                                           |
#'    |:-------------------|:------------------------------------------|
#'    | **from_date**      | Start date of leave                       |
#'    | **reason**         | Reason for leave                          |
#'    | **to_date**        | End of leave                              |
#'    | **type**           | Type of leave                             |
#'    | **sub_last_name**  | Substitute MP last name (id not recorded) |
#'    | **sub_first_name** | Substitute MP first name                  |
#'    
#' 4. **$personalia** (the MP's personalia)
#'  
#'    |                           |                                           |
#'    |:--------------------------|:------------------------------------------|
#'    | **seniority_aar**         | Number of years in parliament             |
#'    | **seniority_dager**       | Number of extra days (addition to years)  |
#'    | **county_of_birth**       | Birth county of the MP                    |
#'    | **municipality_of_birth** | Birth municipality of the MP              |
#'    | **eulogy_date**           | Eulogy date of the MP, when applicable    |
#'  
#' 5. **$father** (the MP's father personalia)
#' 
#'    |                |                        |
#'    |:---------------|:-----------------------|
#'    | **death_year** | Father's year of death |
#'    | **last_name**  | Father's last name     |
#'    | **birth_year** | Father's year of birth |
#'    | **first_name** | Father's first name    |
#'    | **profession** | Father's profession    |
#'    
#' 6. **$mother** (the MP's mother personalia)
#' 
#'    |                |                        |
#'    |:---------------|:-----------------------|
#'    | **death_year** | Mother's year of death |
#'    | **last_name**  | Mother's last name     |
#'    | **birth_year** | Mother's year of birth |
#'    | **first_name** | Mother's first name    |
#'    | **profession** | Mother's profession    |
#' 
#' 7. **$parl_periods** (parliamentary periods the MP has held a seat)
#' 
#'    |                    |                                                    |
#'    |:-------------------|:---------------------------------------------------|
#'    | **from_date**      | Date MP held seat from                             |
#'    | **county**         | County the MP represented                          |
#'    | **party_id**       | Party id for the MP's party                        |
#'    | **rep_number**     | Representative number (within the whol parliament) |
#'    | **parl_period_id** | Id of the parliamentary period                     |
#'    | **to_date**        | Date MP held a seat to                             |
#'    | **type**           | Type of representation                             |
#'    
#' 8. **$parl_positions** (parliamentary positions held by the MP)
#' 
#'    |                    |                                                                   |
#'    |:-------------------|:------------------------------------------------------------------|
#'    | **from_year**      | Year MP held position from                                        |
#'    | **from_date**      | Date MP held position from                                        |
#'    | **committee_id**   | Id of the position (in committee, cabinet, delegation, etc)       |
#'    | **committee_name** | Position name                                                     |
#'    | **committee_type** | Position type                                                     |
#'    | **sorting**        | __Not described in the API__                                      |
#'    | **parl_period_id** | Parliamentary period the position was held (cabinet data missing) |
#'    | **to_year**        | Year MP held position to                                          |
#'    | **to_date**        | Date MP held position to                                          |
#'    
#' 9. **$vocation** (vocation and education of the MP outside of parliament)
#' 
#'    |                          |                                                               |
#'    |:-------------------------|:--------------------------------------------------------------|
#'    | **several_periods_text** | Text description if the vocation was held for several periods |
#'    | **from_year**            | Year MP held vocation from                                    |
#'    | **from_year_unknown**    | Logical indication for whether the start year is unknown      |
#'    | **note**                 | Note for vocation                                             |
#'    | **name**                 | Name of vocation                                              |
#'    | **to_year**              | Year MP held vocation to                                      |
#'    | **to_year_unknown**      | Logical indication for whether the end year is unknown        |
#'    | **type**                 | Vocation type (10 = education, 20 = work)                     |
#'    
#' 1. **$other_positions** (other positions held outside parliament)
#' 
#'    |                          |                                                                                  |
#'    |:-------------------------|----------------------------------------------------------------------------------|
#'    | **several_periods_text** | Text description if the vocation was held for several periods (removed from API) |
#'    | **from_year**            | Year MP held vocation from                                                       |
#'    | **from_year_sorting**    | __Not described in API__ (removed from API)                                      |
#'    | **from_year_unknown**    | Logical indication for whether the start year is unknown                         |
#'    | **max_to_year**          | The last possible time the MP held the position (removed from API)               |
#'    | **note**                 | Note for position                                                                |
#'    | **min_to_year**          | The earliest possible time the MP held the position (removed from API)           |
#'    | **level**                | __Not described in API__                                                         |
#'    | **organization**         | Organization holding the position                                                |
#'    | **place**                | Place of the position                                                            |
#'    | **to_year**              | Year MP held position to                                                         |
#'    | **to_year_unknown**      | Logical indication for whether the end year is unknown                           |
#'    | **type**                 | Position type                                                                    |
#'    | **position**             | Position name/description                                                        |
#' 
#' @md
#' 
#' @seealso [get_mp] [get_parlperiod_mps] [get_mp_pic] [get_session_mp_speech_activity]
#' 
#' 
#' @examples 
#' \dontrun{
#' 
#' # Request one MP by id
#' get_mp_bio("AAMH")
#' 
#' }
#' 
#' @import rvest httr2
#' @export
#' 
get_mp_bio <- function(mpid = NA, good_manners = 0){
  
  url <- paste0("https://data.stortinget.no/eksport/kodetbiografi?personid=", mpid)
  
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
  
  tmp2 <- list(root = data.frame(response_date = tmp |> html_elements("respons_dato_tid") |> html_text(),
                                 version = tmp |> html_elements("versjon") |> html_text(),
                                 id = tmp |> html_elements("person_id") |> html_text()),
               literature = data.frame(year = tmp |> html_elements("person_biografi_litteratur_kodet > aar") |> html_text(),
                                       description = tmp |> html_elements("person_biografi_litteratur_kodet > beskrivelse") |> html_text(),
                                       last_name = tmp |> html_elements("person_biografi_litteratur_kodet > etternavn") |> html_text(),
                                       more_years = tmp |> html_elements("person_biografi_litteratur_kodet > flere_aar") |> html_text(),
                                       publisher = tmp |> html_elements("person_biografi_litteratur_kodet > forlag") |> html_text(),
                                       first_name = tmp |> html_elements("person_biografi_litteratur_kodet > fornavn") |> html_text(),
                                       place = tmp |> html_elements("person_biografi_litteratur_kodet > sted") |> html_text(),
                                       title = tmp |> html_elements("person_biografi_litteratur_kodet > tittel") |> html_text(),
                                       type = tmp |> html_elements("person_biografi_litteratur_kodet > type") |> html_text()),
               leave_of_absence = data.frame(from_date = tmp |> html_elements("person_biografi_permisjon_kodet > fra_dato") |> html_text(),
                                             reason = tmp |> html_elements("person_biografi_permisjon_kodet > fravaer_grunn") |> html_text(),
                                             to_date = tmp |> html_elements("person_biografi_permisjon_kodet > til_dato") |> html_text(),
                                             type = tmp |> html_elements("person_biografi_permisjon_kodet > type") |> html_text(),
                                             sub_last_name = tmp |> html_elements("person_biografi_permisjon_kodet > vara_etternavn") |> html_text(),
                                             sub_first_name = tmp |> html_elements("person_biografi_permisjon_kodet > vara_fornavn") |> html_text()),
               personalia =  data.frame(seniority_years = tmp |> html_elements("personalia_kodet > ansiennitet_aar") |> html_text(),
                                        seniority_days = tmp |> html_elements("personalia_kodet > ansiennitet_dager") |> html_text(),
                                        county_of_birth = tmp |> html_elements("personalia_kodet > foede_fylke") |> html_text(),
                                        municipality_of_birth = tmp |> html_elements("personalia_kodet > foede_kommune") |> html_text(),
                                        eulogy_date = tmp |> html_elements("personalia_kodet > minnetale_dato") |> html_text()),
               father = data.frame(death_year = tmp |> html_elements("person_biografi_foreldre_far_kodet > doedsaar") |> html_text(),
                                   last_name = tmp |> html_elements("person_biografi_foreldre_far_kodet > etternavn") |> html_text(),
                                   birth_year = tmp |> html_elements("person_biografi_foreldre_far_kodet > foedselsaar") |> html_text(),
                                   first_name = tmp |> html_elements("person_biografi_foreldre_far_kodet > fornavn") |> html_text(),
                                   profession = tmp |> html_elements("person_biografi_foreldre_far_kodet > stilling") |> html_text()),
               mother = data.frame(death_year = tmp |> html_elements("person_biografi_foreldre_mor_kodet > doedsaar") |> html_text(),
                                   last_name = tmp |> html_elements("person_biografi_foreldre_mor_kodet > etternavn") |> html_text(),
                                   birth_year = tmp |> html_elements("person_biografi_foreldre_mor_kodet > foedselsaar") |> html_text(),
                                   first_name = tmp |> html_elements("person_biografi_foreldre_mor_kodet > fornavn") |> html_text(),
                                   profession = tmp |> html_elements("person_biografi_foreldre_mor_kodet > stilling") |> html_text()),
               parl_periods = data.frame(from_date = tmp |> html_elements("person_biografi_stortingsperiode_kodet > fra_dato") |> html_text(),
                                         county = tmp |> html_elements("person_biografi_stortingsperiode_kodet > fylke") |> html_text(),
                                         party_id = tmp |> html_elements("person_biografi_stortingsperiode_kodet > parti_id") |> html_text(),
                                         rep_number = tmp |> html_elements("person_biografi_stortingsperiode_kodet > representantnummer") |> html_text(),
                                         parl_period_id = tmp |> html_elements("person_biografi_stortingsperiode_kodet > stortingsperiode_id") |> html_text(),
                                         to_date = tmp |> html_elements("person_biografi_stortingsperiode_kodet > til_dato") |> html_text(),
                                         type = tmp |> html_elements("person_biografi_stortingsperiode_kodet > verv") |> html_text()),
               parl_positions = data.frame(from_year = tmp |> html_elements("person_biografi_stortingsverv_kodet > fra_aar") |> html_text(),
                                           from_date = tmp |> html_elements("person_biografi_stortingsverv_kodet > fra_dato") |> html_text(),
                                           committee_id = tmp |> html_elements("person_biografi_stortingsverv_kodet > komite_kode") |> html_text(),
                                           committee_name = tmp |> html_elements("person_biografi_stortingsverv_kodet > komite_navn") |> html_text(),
                                           committee_type = tmp |> html_elements("person_biografi_stortingsverv_kodet > komite_type") |> html_text(),
                                           sorting = tmp |> html_elements("person_biografi_stortingsverv_kodet > sortering") |> html_text(),
                                           parl_period_id = tmp |> html_elements("person_biografi_stortingsverv_kodet > stortingsperiode_id") |> html_text(),
                                           to_year = tmp |> html_elements("person_biografi_stortingsverv_kodet > til_aar") |> html_text(),
                                           to_date = tmp |> html_elements("person_biografi_stortingsverv_kodet > til_dato") |> html_text()),
               vocation = data.frame(several_periods_text = tmp |> html_elements("person_biografi_utdanning_yrke_kodet > flere_perioder_tekst") |> html_text(),
                                     from_year = tmp |> html_elements("person_biografi_utdanning_yrke_kodet > fra_aar") |> html_text(),
                                     from_year_unknown = tmp |> html_elements("person_biografi_utdanning_yrke_kodet > fra_aar_ukjent") |> html_text(),
                                     note = tmp |> html_elements("person_biografi_utdanning_yrke_kodet > merknad") |> html_text(),
                                     name = tmp |> html_elements("person_biografi_utdanning_yrke_kodet > navn") |> html_text(),
                                     to_year = tmp |> html_elements("person_biografi_utdanning_yrke_kodet > til_aar") |> html_text(),
                                     to_year_unknown = tmp |> html_elements("person_biografi_utdanning_yrke_kodet > til_aar_ukjent") |> html_text(),
                                     type = tmp |> html_elements("person_biografi_utdanning_yrke_kodet > type") |> html_text()),
               other_positions = data.frame(from_year = tmp |> html_elements("person_biografi_verv_kodet > fra_aar") |> html_text(),
                                            # Some variables seems to have been removed from the API
                                            # several_periods_text = tmp |> html_elements("person_biografi_verv_kodet > flere_perioder_tekst") |> html_text(),
                                            # from_year_sorting = tmp |> html_elements("person_biografi_verv_kodet > fra_aar_sortering") |> html_text(),
                                            from_year_unknown = tmp |> html_elements("person_biografi_verv_kodet > fra_aar_ukjent") |> html_text(),
                                            # max_to_year = tmp |> html_elements("person_biografi_verv_kodet > maksimum_til_aar") |> html_text(),
                                            note = tmp |> html_elements("person_biografi_verv_kodet > merknad") |> html_text(),
                                            # min_start_year = tmp |> html_elements("person_biografi_verv_kodet > minimum_start_aar") |> html_text(),
                                            level = tmp |> html_elements("person_biografi_verv_kodet > nivaa") |> html_text(),
                                            organization = tmp |> html_elements("person_biografi_verv_kodet > organisasjon") |> html_text(),
                                            place = tmp |> html_elements("person_biografi_verv_kodet > sted") |> html_text(),
                                            to_year = tmp |> html_elements("person_biografi_verv_kodet > til_aar") |> html_text(),
                                            to_year_unknown = tmp |> html_elements("person_biografi_verv_kodet > til_aar_ukjent") |> html_text(),
                                            type = tmp |> html_elements("person_biografi_verv_kodet > type") |> html_text(),
                                            position = tmp |> html_elements("person_biografi_verv_kodet > verv") |> html_text()))
  
  
  Sys.sleep(good_manners)
  
  return(tmp2)
  
}