#' Extract biography of specific MPs
#' 
#' A function for retrieving biography of Norwegian MPs from the parliament API
#' 
#' @usage get_mp_bio(id = NA, good_manners = 0)
#' 
#' @param id Character string indicating the id of the MP to retrieve.
#' @param good_manners Integer. Seconds delay between calls when making multiple calls to the same function
#' 
#'
#' @return A list with response date, version ...
#' 
#' @family get_mp_data
#' 
#' 
#' @examples 
#' # Request one MP by id
#' get_mp_bio("AAMH")
#' 
#' @import rvest
#' @export
#' 



get_mp_bio <- function(id = NA, good_manners = 0){
  
  tmp <- read_html(paste0("https://data.stortinget.no/eksport/kodetbiografi?personid=", id))
  
  tmp2 <- list(root = data.frame(response_date = tmp %>% html_elements("respons_dato_tid") %>% html_text(),
                                 version = tmp %>% html_elements("versjon") %>% html_text(),
                                 id = tmp %>% html_elements("person_id") %>% html_text()),
               literature = data.frame(year = tmp %>% html_elements("person_biografi_litteratur_kodet > aar") %>% html_text(),
                                       description = tmp %>% html_elements("person_biografi_litteratur_kodet > beskrivelse") %>% html_text(),
                                       last_name = tmp %>% html_elements("person_biografi_litteratur_kodet > etternavn") %>% html_text(),
                                       more_years = tmp %>% html_elements("person_biografi_litteratur_kodet > flere_aar") %>% html_text(),
                                       publisher = tmp %>% html_elements("person_biografi_litteratur_kodet > forlag") %>% html_text(),
                                       first_name = tmp %>% html_elements("person_biografi_litteratur_kodet > fornavn") %>% html_text(),
                                       place = tmp %>% html_elements("person_biografi_litteratur_kodet > sted") %>% html_text(),
                                       title = tmp %>% html_elements("person_biografi_litteratur_kodet > tittel") %>% html_text(),
                                       type = tmp %>% html_elements("person_biografi_litteratur_kodet > type") %>% html_text()),
               leave_of_absence = data.frame(from_date = tmp %>% html_elements("person_biografi_permisjon_kodet > fra_dato") %>% html_text(),
                                             reason = tmp %>% html_elements("person_biografi_permisjon_kodet > fravaer_grunn") %>% html_text(),
                                             to_date = tmp %>% html_elements("person_biografi_permisjon_kodet > til_dato") %>% html_text(),
                                             type = tmp %>% html_elements("person_biografi_permisjon_kodet > type") %>% html_text(),
                                             sub_last_name = tmp %>% html_elements("person_biografi_permisjon_kodet > vara_etternavn") %>% html_text(),
                                             sub_first_name = tmp %>% html_elements("person_biografi_permisjon_kodet > vara_fornavn") %>% html_text()),
               personalia =  data.frame(seniority = tmp %>% html_elements("personalia_kodet > ansiennitet") %>% html_text(),
                                        county_of_birth = tmp %>% html_elements("personalia_kodet > foede_fylke") %>% html_text(),
                                        municipality_of_birth = tmp %>% html_elements("personalia_kodet > foede_kommune") %>% html_text(),
                                        eulogy_date = tmp %>% html_elements("personalia_kodet > minnetale_dato") %>% html_text()),
               father = data.frame(death_year = tmp %>% html_elements("person_biografi_foreldre_far_kodet > doedsaar") %>% html_text(),
                                   last_name = tmp %>% html_elements("person_biografi_foreldre_far_kodet > etternavn") %>% html_text(),
                                   birth_year = tmp %>% html_elements("person_biografi_foreldre_far_kodet > foedselsaar") %>% html_text(),
                                   first_name = tmp %>% html_elements("person_biografi_foreldre_far_kodet > fornavn") %>% html_text(),
                                   profession = tmp %>% html_elements("person_biografi_foreldre_far_kodet > stilling") %>% html_text()),
               mother = data.frame(death_year = tmp %>% html_elements("person_biografi_foreldre_mor_kodet > doedsaar") %>% html_text(),
                                   last_name = tmp %>% html_elements("person_biografi_foreldre_mor_kodet > etternavn") %>% html_text(),
                                   birth_year = tmp %>% html_elements("person_biografi_foreldre_mor_kodet > foedselsaar") %>% html_text(),
                                   first_name = tmp %>% html_elements("person_biografi_foreldre_mor_kodet > fornavn") %>% html_text(),
                                   profession = tmp %>% html_elements("person_biografi_foreldre_mor_kodet > stilling") %>% html_text()),
               parl_periods = data.frame(from_date = tmp %>% html_elements("person_biografi_stortingsperiode_kodet > fra_dato") %>% html_text(),
                                         county = tmp %>% html_elements("person_biografi_stortingsperiode_kodet > fylke") %>% html_text(),
                                         party_id = tmp %>% html_elements("person_biografi_stortingsperiode_kodet > parti_id") %>% html_text(),
                                         rep_number = tmp %>% html_elements("person_biografi_stortingsperiode_kodet > representantnummer") %>% html_text(),
                                         parl_period_id = tmp %>% html_elements("person_biografi_stortingsperiode_kodet > stortingsperiode_id") %>% html_text(),
                                         to_date = tmp %>% html_elements("person_biografi_stortingsperiode_kodet > til_dato") %>% html_text(),
                                         type = tmp %>% html_elements("person_biografi_stortingsperiode_kodet > verv") %>% html_text()),
               parl_positions = data.frame(from_year = tmp %>% html_elements("person_biografi_stortingsverv_kodet > fra_aar") %>% html_text(),
                                           from_date = tmp %>% html_elements("person_biografi_stortingsverv_kodet > fra_dato") %>% html_text(),
                                           committee_id = tmp %>% html_elements("person_biografi_stortingsverv_kodet > komite_kode") %>% html_text(),
                                           committee_name = tmp %>% html_elements("person_biografi_stortingsverv_kodet > komite_navn") %>% html_text(),
                                           committee_type = tmp %>% html_elements("person_biografi_stortingsverv_kodet > komite_type") %>% html_text(),
                                           sorting = tmp %>% html_elements("person_biografi_stortingsverv_kodet > sortering") %>% html_text(),
                                           parl_period_id = tmp %>% html_elements("person_biografi_stortingsverv_kodet > stortingsperiode_id") %>% html_text(),
                                           to_year = tmp %>% html_elements("person_biografi_stortingsverv_kodet > til_aar") %>% html_text(),
                                           to_date = tmp %>% html_elements("person_biografi_stortingsverv_kodet > til_dato") %>% html_text()),
               vocation = data.frame(several_periods_text = tmp %>% html_elements("person_biografi_utdanning_yrke_kodet > flere_perioder_tekst") %>% html_text(),
                                     from_year = tmp %>% html_elements("person_biografi_utdanning_yrke_kodet > fra_aar") %>% html_text(),
                                     from_year_unknown = tmp %>% html_elements("person_biografi_utdanning_yrke_kodet > fra_aar_ukjent") %>% html_text(),
                                     note = tmp %>% html_elements("person_biografi_utdanning_yrke_kodet > merknad") %>% html_text(),
                                     name = tmp %>% html_elements("person_biografi_utdanning_yrke_kodet > navn") %>% html_text(),
                                     to_year = tmp %>% html_elements("person_biografi_utdanning_yrke_kodet > til_aar") %>% html_text(),
                                     to_year_unknown = tmp %>% html_elements("person_biografi_utdanning_yrke_kodet > til_aar_ukjent") %>% html_text(),
                                     type = tmp %>% html_elements("person_biografi_utdanning_yrke_kodet > type") %>% html_text()),
               other_positions = data.frame(several_periods_text = tmp %>% html_elements("person_biografi_verv_kodet > flere_perioder_tekst") %>% html_text(),
                                            from_year = tmp %>% html_elements("person_biografi_verv_kodet > fra_aar") %>% html_text(),
                                            from_year_sorting = tmp %>% html_elements("person_biografi_verv_kodet > fra_aar_sortering") %>% html_text(),
                                            from_year_unknown = tmp %>% html_elements("person_biografi_verv_kodet > fra_aar_ukjent") %>% html_text(),
                                            max_to_year = tmp %>% html_elements("person_biografi_verv_kodet > maksimum_til_aar") %>% html_text(),
                                            note = tmp %>% html_elements("person_biografi_verv_kodet > merknad") %>% html_text(),
                                            min_start_year = tmp %>% html_elements("person_biografi_verv_kodet > minimum_start_aar") %>% html_text(),
                                            level = tmp %>% html_elements("person_biografi_verv_kodet > nivaa") %>% html_text(),
                                            organization = tmp %>% html_elements("person_biografi_verv_kodet > organisasjon") %>% html_text(),
                                            place = tmp %>% html_elements("person_biografi_verv_kodet > sted") %>% html_text(),
                                            to_year = tmp %>% html_elements("person_biografi_verv_kodet > til_aar") %>% html_text(),
                                            to_year_unknown = tmp %>% html_elements("person_biografi_verv_kodet > til_aar_ukjent") %>% html_text(),
                                            type = tmp %>% html_elements("person_biografi_verv_kodet > type") %>% html_text(),
                                            position = tmp %>% html_elements("person_biografi_verv_kodet > verv") %>% html_text()))
  

  Sys.sleep(good_manners)
  
  return(tmp2)
  
}