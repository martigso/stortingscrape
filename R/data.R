#' Storting cases in the 2019-2020 session
#'
#' A dataset containing all cases of the 2019-2020 parliamentary
#' session in *Stortinget*
#'
#' @format A list with four elements 
#'  
#' \describe{
#'  \item{$root}{main data on the cases}
#'  \item{$topics}{named list by case id}
#'  \item{$proposers}{named list by case id}
#'  \item{$spokespersons}{named list by case id}
#'  \item{Further description:}{[get_session_cases]}
#' }
#'
#' The dataset was retrieved with an earlier version of the package; [get_session_cases] now returns
#' `$spokespersons` as a data frame.
#'   
#' @source \url{https://data.stortinget.no/eksport/saker?sesjonid=2019-2020}
"cases"

#' Votes on case id 85196
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

#' Vote id 17689 results
#'
#' A dataset containing the vote matrix on vote id 17689
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
#'    \item{permanent_sub_for}{Id of the MP originally holding the seat, if the substitute is permanent}
#'    \item{sub_for}{Id of the MP originally holding the seat}
#' }  
#' 
#' @source \url{https://data.stortinget.no/eksport/voteringsresultat?voteringid=17689}
"covid_relief_result"

#' Interpellations from the 2002-2003 session
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
#'    \item{answ_on_behalf_of}{Answer given on behalf of}
#'    \item{answ_on_behalf_of_minister_id}{Department id of minister given answer on behalf of}
#'    \item{answ_on_behalf_of_minister_title}{Department title of minister given answer on behalf of}
#'    \item{topic_ids}{Id of relevant topics for question}
#'    \item{moved_to}{Question moved to}
#'    \item{asked_by_other_id}{MP id, if question was not asked by the questioning MP}
#'    \item{id}{Question id}
#'    \item{correct_person}{Not documented in API}
#'    \item{correct_person_minister_id}{Not documented in API}
#'    \item{correct_person_minister_title}{Not documented in API}
#'    \item{sent_date}{Date the question was sent}
#'    \item{session_id}{Session id}
#'    \item{question_from_id}{Question from MP id}
#'    \item{question_number}{Question number within session}
#'    \item{question_to_id}{Question directed to minister id}
#'    \item{question_to_minister_id}{Question directed to minister department id}
#'    \item{question_to_minister_title}{Question directed to minister department title}
#'    \item{status}{Question status}
#'    \item{title}{Question title}
#'    \item{type}{Question type}
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
#' @format A data frame with 6 columns and 21 rows
#'  
#' \describe{
#'    \item{response_date}{Date of data retrieval}
#'    \item{version}{Data version from the API}
#'    \item{from}{Date period started}
#'    \item{id}{Id of the period (used by other functions)}
#'    \item{to}{Date period ended}
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
#' @format A data frame with 6 columns and 43 rows (including sessions announced ahead of time)
#'  
#' \describe{
#'    \item{response_date}{Date of data retrieval}
#'    \item{version}{Data version from the API}
#'    \item{from}{Date session started}
#'    \item{id}{Id of the session (used by other functions)}
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
#' @format A list with three data frames, one per vote (15404, 15405, and 15406), each with 169 rows and
#' the following variables:
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
#' A dataset containing vote information on case id
#' 78686 in *Stortinget*
#'
#' @format A data frame with 22 columns and 3 rows (one per vote)
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
#' @source \url{https://data.stortinget.no/eksport/voteringer?sakid=78686}
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
#'    \item{Uavhengig (independent)}{Black; no official color}
#'    \item{Venstre (Liberal Party)}{\url{https://www.venstre.no/organisasjon/visuell-identitet/}}
#' }
#' 
#' @source See list of links above; there are several color alternatives for most parties. 
#' 
#' @examples 
#' \dontrun{
#' 
#' seats <- table(get_parlperiod_mps(parl_periods$id[1])$party_id)
#' barplot(seats, col = st_party_colors[names(seats)])
#' 
#' }
"st_party_colors" 

#' Person ids for speakers and chairs in debate transcripts
#'
#' A crosswalk from the names of speakers and chairs, as written in the debate
#' transcripts (see [get_speeches]), to person ids, by parliamentary session.
#' [get_speeches] adds these links to its output by default (`link = TRUE`). With
#' `link = FALSE`, the dataset can be joined to the output by `speaker_name` and
#' `session_id` (speakers) or by `chair_name` and `session_id` (chairs).
#'
#' Names are linked to MPs and substitutes of the session's parliamentary period
#' ([get_parlperiod_mps]) and to ministers in office during the session. A name
#' is linked only when exactly one person matches, first on the full name and
#' then on first and last name. Names with no such match can still be linked as
#' a changed name (e.g. a surname added or dropped at marriage): when the first
#' name is the same, every word of the shorter name is in the longer one, exactly
#' one MP in the period fits, and the name is written with that MP's party
#' throughout the session. A name that only matches a substitute registration,
#' and is never written with a party, a title, or as chair in the session (e.g. a
#' witness in a hearing), is not linked. There are no manual corrections: names
#' that cannot be resolved this way (e.g. witnesses in hearings, misspelled names,
#' and some changed names) have `linked_person_id` `NA`.
#'
#' The links are made from names only and do not use the `person_id` given in
#' the transcripts from 2016-2017 onward, which is sometimes wrong.
#'
#' Ministers and their periods in office come from a list of ministers from 1945
#' to January 2024, supplemented by the cabinet posts in [get_mp_bio] for
#' ministers in office after that. The minister data will be updated in a later
#' version.
#'
#' The dataset covers the transcripts available in the API when it was built
#' (sessions 1998-99 onward). The API's list of transcripts for 2008-2009 fails,
#' so these transcripts were found by their ids, constructed from
#' [get_session_meetings]; open hearings in that session are therefore missing.
#' Four listed transcripts (s031205, s050526k, s602081, o006051) could not be
#' retrieved from the API.
#'
#' @format A data frame with 4 columns and 8098 rows
#'
#' \describe{
#'    \item{speaker_name}{Name as written in the transcripts (`speaker_name` or `chair_name` in [get_speeches])}
#'    \item{session_id}{Id of the parliamentary session}
#'    \item{linked_person_id}{Id of the person (see [get_mp]), or `NA` when the name cannot be linked}
#'    \item{link_method}{How the name was linked: on full name, first and last name, or as a changed name, to an MP roster and/or a minister spell; "ambiguous" when several persons match; "substitute only, no party or title" when not linked for that reason}
#' }
#'
#' @source Built by `data-raw/speaker_links.R` from the transcripts
#' (\url{https://data.stortinget.no/dokumentasjon-og-hjelp/publikasjon/}), the MPs of each parliamentary
#' period (\url{https://data.stortinget.no/dokumentasjon-og-hjelp/representanter/}), and a list of ministers
#' and their periods in office from regjeringen.no, supplemented by the coded biographies
#' (\url{https://data.stortinget.no/dokumentasjon-og-hjelp/kodet-personbiografi/}).
#'
#' @examples
#' \dontrun{
#'
#' speeches <- get_speeches("s140213", link = FALSE)
#'
#' speeches <- merge(speeches, speaker_links, by = c("speaker_name", "session_id"), all.x = TRUE)
#'
#' }
"speaker_links"

#' Speeches in the Storting's meeting on 13 February 2014
#'
#' A dataset containing all speeches in the transcript of the Storting's meeting
#' on 13 February 2014 (publication id "s140213"), as returned by [get_speeches]
#'
#' @format A data frame with 27 columns and 48 rows (see [get_speeches] for details)
#'
#' \describe{
#'    \item{response_date}{Date and time of retrieval}
#'    \item{publication_id}{Id of the transcript}
#'    \item{session_id}{Id of the parliamentary session}
#'    \item{meeting_order}{Order of the meeting within the transcript}
#'    \item{meeting_id}{Meeting id, when given in the transcript}
#'    \item{meeting_title}{Raw meeting heading}
#'    \item{meeting_date}{Date of the meeting}
#'    \item{section}{Name of the XML element directly containing the speech}
#'    \item{case_id}{Id of the case the speech belongs to, when given in the transcript}
#'    \item{agenda_no}{Agenda item number (from 2016-2017 onward)}
#'    \item{agenda_merged}{Agenda item numbers debated together (from 2016-2017 onward)}
#'    \item{speech_order}{Order of the speech element within the transcript}
#'    \item{speech_part}{Order of the speaker within the speech element}
#'    \item{speech_id}{Speech element id (from 2016-2017 onward)}
#'    \item{speech_type}{Type of speech}
#'    \item{speaker_raw}{Speaker as written in the transcript}
#'    \item{speaker_title}{Title parsed from `speaker_raw`}
#'    \item{speaker_name}{Name parsed from `speaker_raw`}
#'    \item{speaker_party}{Party parsed from `speaker_raw`}
#'    \item{speech_time}{Time stamp parsed from `speaker_raw`}
#'    \item{person_id}{Id of the speaker, when given in the transcript}
#'    \item{linked_person_id}{Id of the speaker, linked from `speaker_name` (see [speaker_links])}
#'    \item{link_method}{How `linked_person_id` was linked}
#'    \item{chair_name}{Name of the sitting chair}
#'    \item{chair_id}{Id of the sitting chair, when given in the transcript}
#'    \item{chair_linked_id}{Id of the sitting chair, linked from `chair_name`}
#'    \item{text}{Speech text, one line per paragraph}
#' }
#'
#' @source \url{https://data.stortinget.no/eksport/publikasjon?publikasjonid=s140213}
"speeches140213"
