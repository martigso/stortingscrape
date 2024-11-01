#' Storting cases in the 2019-2020 session
#'
#' A dataset containing all cases of the 2019-2020 parliamentary
#' session in *Stortinget*
#'
#' @format A list with four elements 
#'  
#' \describe{
#'  \item{$root}{main data on the MP}
#'  \item{$topics}{named list by case id}
#'  \item{$proposers}{named list by case id}
#'  \item{$spokespersons}{named list by case id}
#'  \item{Further description:}{[get_session_cases]}
#' }
#'   
#' @source \url{https://data.stortinget.no/eksport/saker?sesjonid=2019-2020}
"cases"

#' Vote id 85196
#'
#' A dataset containing all vote information on case id 85196
#'
#' @format A data frame with 22 columns and 71 rows
#'  
#' \describe{
#'    \item{response_date}{Date of data retrieval}
#'    \item{version}{Data version from the API}
#'    \item{case_id}{Case id up for vote}
#'    \item{alternative_vote}{Whether vote is an alternative vote}
#'    \item{n_for}{Number of votes for}
#'    \item{n_absent}{Number of MPs absent}
#'    \item{n_against}{Number of votes against}
#'    \item{treatment_order}{Order of treated votes}
#'    \item{agenda_case_number}{Case number on the agenda of the meeting}
#'    \item{free_vote}{Logical indication of whether the vote is related to the case as a whole}
#'    \item{comment}{Vote comment}
#'    \item{meeting_map_number}{Number on the meeting map}
#'    \item{personal_vote}{Logical indication of whether vote was recorded as roll call or not}
#'    \item{president_id}{Id of president holding president chair at the time of voting}
#'    \item{president_party_id}{Party of the sitting president}
#'    \item{adopted}{Logical indication of whether the proposal voted on was adopted}
#'    \item{vote_id}{Id of vote}
#'    \item{vote_method}{Voting method}
#'    \item{vote_result_type}{Result type (enstemmig_vedtatt = unanimously adopted)}
#'    \item{vote_result_type_text}{See __vote_result_type__}
#'    \item{vote_topic}{Description of the proposal voted upon}
#'    \item{vote_datetime}{Date and time of vote}
#' }  
#' 
#' @source \url{https://data.stortinget.no/eksport/voteringer?sakid=85196}
"covid_relief"

#' Vote id 85196 results
#'
#' A dataset containing vote matrix on vote id 17689
#'
#' @format A data frame with 8 columns and 169 rows
#'  
#' \describe{
#'    \item{response_date}{Date of data retrieval}
#'    \item{version}{Data version from the API}
#'    \item{vote_id}{Id of vote}
#'    \item{mp_id}{MP id}
#'    \item{party_id}{Party id}
#'    \item{vote}{Vote: for, mot (against), ikke_tilstede (absent)}
#'    \item{permanent_sub_for}{Id of the MP originally holding the seat, if the substitute is }
#'    \item{sub_for}{Id of the MP originally holding the seat}
#' }  
#' 
#' @source \url{https://data.stortinget.no/eksport/voteringsresultat?voteringid=17689}
"covid_relief_result"

#' Interpellations from the 2002-2003 
#'
#' A dataset containing all interpellations in the 2002-2003 parliamentary session in
#' *Stortinget*
#'
#' @format A data frame with 26 columns and 22 rows
#'  
#' \describe{
#'    \item{response_date}{Date of data retrieval}
#'    \item{version}{Data version from the API}
#'    \item{answ_by_id}{Id of minister answering question}
#'    \item{answ_by_minister_id}{Department id of answering minister}
#'    \item{answ_by_minister_title}{Department title of answering minister}
#'    \item{answ_date}{Date answer was given}
#'    \item{answ_on_belhalf_of}{Answer given on behalf of}
#'    \item{answ_on_belhalf_of_minister_id}{Department id of minister given answer on behalf of}
#'    \item{answ_on_belhalf_of_minister_title}{Department title of minister given answer on behalf of}
#'    \item{topic_ids}{Id of relevant topics for question}
#'    \item{moved_to}{Question moved to}
#'    \item{asked_by_other_id}{MP id, if question was not asked by the questioning MP}
#'    \item{id}{Question id}
#'    \item{correct_person}{Not documented in API}
#'    \item{correct_person_minister_id}{Not documented in API}
#'    \item{correct_person_minister_title}{Not documented in API}
#'    \item{sendt_date}{Date the question was sent}
#'    \item{session_id}{Session id}
#'    \item{question_from_id}{Question from MP id}
#'    \item{qustion_number}{Question number within session}
#'    \item{qustion_to_id}{Question directed to minister id}
#'    \item{qustion_to_minister_id}{Question directed to minister department id}
#'    \item{qustion_to_minister_title}{Question directed to minister department title}
#'    \item{type}{Question type}
#'    \item{title}{Question title}
#'    \item{status}{Question status}
#' }  
#' 
#' @source \url{https://data.stortinget.no/eksport/interpellasjoner?sesjonid=2002-2003}
"interp0203"

#' Members of parliament from the 1945-1949
#'
#' A dataset containing all MPs during the 1945-1949 parliamentary period in
#' *Stortinget*
#'
#' @format A data frame with 12 columns and 150 rows
#'  
#' \describe{
#'    \item{response_date}{Date of data retrieval}
#'    \item{version}{Data version from the API}
#'    \item{death}{Date of death}
#'    \item{lastname}{MP last name}
#'    \item{birth}{Date of birth}
#'    \item{firstname}{MP first name}
#'    \item{mp_id}{MP id}
#'    \item{gender}{MP gender}
#'    \item{county_id}{Id of county MP represented}
#'    \item{party_id}{Id of party MP represented}
#'    \item{substitute_mp}{Logical for whether MP is a substitute}
#'    \item{period_id}{Id of period represented in}
#' }  
#' 
#' @source \url{https://data.stortinget.no/eksport/representanter?stortingsperiodeid=1945-49}
"mps4549"

#' Parliamentary periods
#'
#' A dataset containing all parliamentary periods in
#' *Stortinget*
#'
#' @format A data frame with 12 columns and 150 rows
#'  
#' \describe{
#'    \item{response_date}{Date of data retrieval}
#'    \item{version}{Data version from the API}
#'    \item{from}{Date session started}
#'    \item{id}{Id of for session (used for other functions)}
#'    \item{to}{Date session ended}
#'    \item{years}{From year to year in full format}
#' }  
#' 
#' @source \url{https://data.stortinget.no/eksport/stortingsperioder}
"parl_periods"

#' Parliamentary sessions
#'
#' A dataset containing all parliamentary sessions in
#' *Stortinget*
#'
#' @format A data frame with 6 columns and 36 rows
#'  
#' \describe{
#'    \item{response_date}{Date of data retrieval}
#'    \item{version}{Data version from the API}
#'    \item{from}{Date session started}
#'    \item{id}{Id of for session (used for other functions)}
#'    \item{to}{Date session ended}
#'    \item{years}{From year to year in full format}
#' }  
#' 
#' @source \url{https://data.stortinget.no/eksport/sesjoner}
"parl_sessions"

#' Roll call vote results for vote ids 15404, 15405, and 15406
#'
#' A dataset containing all personal votes for votes 
#' 15404, 15405, and 15406 in *Stortinget*
#'
#' @format A list with one vote per element
#'  
#' \describe{
#'    \item{response_date}{Date of data retrieval}
#'    \item{version}{Data version from the API}
#'    \item{vote_id}{Id of vote}
#'    \item{mp_id}{MP id}
#'    \item{party_id}{Party id}
#'    \item{vote}{Vote: for, mot (against), ikke_tilstede (absent)}
#'    \item{permanent_sub_for}{Id of the MP originally holding the seat, if the substitute is permanent}
#'    \item{sub_for}{Id of the MP originally holding the seat}
#' }  
#' 
#' @source 
#'   \url{https://data.stortinget.no/eksport/voteringsresultat?voteringid=15404},
#'   \url{https://data.stortinget.no/eksport/voteringsresultat?voteringid=15405},
#'   \url{https://data.stortinget.no/eksport/voteringsresultat?voteringid=15406}
"vote_result"

#' Meta data on votes of case id 78686
#'
#' A dataset containing vote infomation on case id
#' 78686 in *Stortinget*
#'
#' @format A list with three elements (votes)
#'  
#' \describe{
#'    \item{response_date}{Date of data retrieval}
#'    \item{version}{Data version from the API}
#'    \item{case_id}{Case id up for vote}
#'    \item{alternative_vote}{Whether vote is an alternative vote}
#'    \item{n_for}{Number of votes for}
#'    \item{n_absent}{Number of MPs absent}
#'    \item{n_against}{Number of votes against}
#'    \item{treatment_order}{Order of treated votes}
#'    \item{agenda_case_number}{Case number on the agenda of the meeting}
#'    \item{free_vote}{Logical indication of whether the vote is related to the case as a whole}
#'    \item{comment}{Vote comment}
#'    \item{meeting_map_number}{Number on the meeting map}
#'    \item{personal_vote}{Logical indication of whether vote was recorded as roll call or not}
#'    \item{president_id}{Id of president holding president chair at the time of voting}
#'    \item{president_party_id}{Party of the sitting president}
#'    \item{adopted}{Logical indication of whether the proposal voted on was adopted}
#'    \item{vote_id}{Id of vote}
#'    \item{vote_method}{Voting method}
#'    \item{vote_result_type}{Result type (enstemmig_vedtatt = unanimously adopted)}
#'    \item{vote_result_type_text}{See __vote_result_type__}
#'    \item{vote_topic}{Description of the proposal voted upon}
#'    \item{vote_datetime}{Date and time of vote}
#' }
#' 
#' @source 
#'   \url{https://data.stortinget.no/eksport/voteringsresultat?voteringid=15404}
"vote"

#' Color palette for parties in the Storting
#' 
#' A color palette for all (current) parties in the Storting
#' 
#' @format A vector of party abbreviations and official hex colors
#' 
#' 
#' \describe{
#'    \item{Arbeiderpartiet (Labour Party)}{\url{https://www.arbeiderpartiet.no/om/presse/profil/}}
#'    \item{Fremskrittspartiet (Progress Party)}{\url{https://www.frp.no/files/Grafiske-retningslinjer/FrP-Profilmanual-2023.pdf}}
#'    \item{Høyre (Conservative Party)}{\url{https://hoyre.no/design/farger/}}
#'    \item{Kristelig Folkeparti (Christian Democratic Party)}{\url{https://krf.no/ressursbank/logoarkiv/}}
#'    \item{Miljøpartiet De Grønne (Green Party)}{\url{https://mdg.no/partiet/organisasjon#logo}}
#'    \item{Pasientfokus (Patient Focus)}{\url{https://no.wikipedia.org/wiki/Mal:Farge/Pasientfokus}}
#'    \item{Rødt (Red Party)}{\url{https://roedt.no/grafisk-materiell}}
#'    \item{Senterpartiet (Centre Party)}{\url{https://profil.senterpartiet.no/point/no/senterpartietbc/component/default/24406}}
#'    \item{Sosialistisk Venstreparti (Socialist Left Party)}{\url{https://www.sv.no/ressursbanken/grafisk/grafisk-profil/}}
#'    \item{Venstre (Liberal Party)}{\url{https://www.venstre.no/organisasjon/visuell-identitet/}}
#' }
#' 
#' @source See list of links above; there are several color alternatives for most parties. 
#' 
#' @examples 
#' \dontrun{
#' 
#' barplot(table(get_parlperiod_mps(parl_periods$id[1])$party_id), col = st_party_colors)
#' 
#' }
"st_party_colors" 
