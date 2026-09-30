#' Retrieve the speeches in a debate transcript
#'
#' A function for retrieving all speeches in a debate transcript (publication type "referat"), one row
#' per speech. Handles both the transcript format used up to the 2015-2016 session and the format used
#' from the 2016-2017 session onward, returning the same variables for both. Plenary sittings, open
#' hearings, and meetings in the European Committee are all published as transcripts.
#'
#' Some speech elements in the transcripts hold more than one speaker (e.g. a question and an answer in
#' a hearing). These are split into one row per speaker; `speech_order` identifies the speech element
#' and `speech_part` the speaker within it.
#'
#' The transcripts before about 2005 mostly do not say whether a speech is a main speech
#' ("hovedinnlegg") or a reply ("replikk"); `speech_type` is then `NA`, except for the president's
#' remarks ("presinnlegg").
#'
#' The meeting date is given both in the meeting heading (weekday, day, month, and, from 2007 onward,
#' year) and in the publication id, and both contain occasional errors. When they agree, that date is
#' used; when they disagree, the one whose weekday matches the weekday in the heading is used, and `NA`
#' if that does not settle it.
#'
#' Speakers are only identified by the API (`person_id`) in transcripts from the 2016-2017 session onward,
#' and not for all speeches (e.g. not for the president or committee chair). Note that these ids are not
#' always correct: speeches are sometimes tagged with the id of another person than the one named in
#' `speaker_raw`. A numeric suffix that some of these ids carry (e.g. "ARK_775612110") is removed. The
#' raw speaker string is always kept in `speaker_raw`; `speaker_title`, `speaker_name`, `speaker_party`,
#' and `speech_time` are parsed from it.
#'
#' With `link = TRUE` (the default), person ids linked from the names of the speakers and chairs are
#' added from the [speaker_links] dataset (`linked_person_id`, `link_method`, and `chair_linked_id`),
#' also for transcripts before 2016-2017. These links are made by the package, not given by the API
#' (`person_id` is kept as the API gives it), and they only cover the sessions in [speaker_links]; for
#' later sessions, the linked ids are `NA`.
#'
#' The sitting chair (president or meeting leader) is tracked through the transcript: the chair named at
#' the start of each meeting, updated at the transcript's notes on changes of chair (e.g. "X hadde her
#' overtatt presidentplassen"). When a note about the chair cannot be read, the chair is set to `NA`
#' until the next recognized change. `chair_id` is only given for the chair named at the start of a
#' meeting, and only when the transcript includes it (a numeric suffix that some of these ids carry,
#' e.g. "OLET_62710109", is removed).
#'
#' @usage get_speeches(publicationid = NA, good_manners = 0, link = TRUE)
#'
#' @param publicationid Character string, or a vector of strings, indicating the id of the transcript to retrieve.
#' Ids can be found with [get_session_publications] (`type = "referat"`)
#' @param good_manners Numeric. Seconds delay between calls when making multiple calls to the same function. Note that the Stortinget API is limited to 100 calls per minute (see \url{https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/}).
#' @param link Logical. Whether to add person ids linked from the names of speakers and chairs
#' (see [speaker_links]). Defaults to `TRUE`.
#'
#' @return A data.frame with the following variables:
#'
#'    |                     |                                                                                       |
#'    |:--------------------|:--------------------------------------------------------------------------------------|
#'    | **response_date**   | Date and time of retrieval (the transcripts have no response date of their own)       |
#'    | **publication_id**  | Id of the transcript                                                                  |
#'    | **session_id**      | Id of the parliamentary session (see [get_parlsessions]), from `meeting_date`         |
#'    | **meeting_order**   | Order of the meeting within the transcript (some transcripts hold several meetings)   |
#'    | **meeting_id**      | Meeting id (see [get_session_meetings]), when given in the transcript                 |
#'    | **meeting_title**   | Raw meeting heading (e.g. "Møte torsdag den 13. februar 2014 kl. 10")                 |
#'    | **meeting_date**    | Date of the meeting, from `meeting_title` and the publication id (see details)        |
#'    | **section**         | Name of the XML element directly containing the speech (e.g. sak, spm, formalia)     |
#'    | **case_id**         | Id of the case the speech belongs to (see [get_case]), when given in the transcript   |
#'    | **agenda_no**       | Agenda item number (from 2016-2017 onward)                                            |
#'    | **agenda_merged**   | Agenda item numbers debated together, comma separated (from 2016-2017 onward)         |
#'    | **speech_order**    | Order of the speech element within the transcript                                     |
#'    | **speech_part**     | Order of the speaker within the speech element (usually 1)                            |
#'    | **speech_id**       | Speech element id (from 2016-2017 onward)                                             |
#'    | **speech_type**     | Type of speech ("hovedinnlegg", "replikk", or "presinnlegg"; see details)            |
#'    | **speaker_raw**     | Speaker as written in the transcript                                                  |
#'    | **speaker_title**   | Title parsed from `speaker_raw` (e.g. "Statsråd", "Presidenten")                      |
#'    | **speaker_name**    | Name parsed from `speaker_raw`                                                        |
#'    | **speaker_party**   | Party parsed from `speaker_raw`, as a party id of [get_all_parties] (else NA)         |
#'    | **speech_time**     | Time stamp parsed from `speaker_raw` (hh:mm:ss)                                       |
#'    | **person_id**       | Id of the speaker (see [get_mp]), when given in the transcript                        |
#'    | **linked_person_id**| Id of the speaker, linked from `speaker_name` (with `link = TRUE`)                    |
#'    | **link_method**     | How `linked_person_id` was linked (with `link = TRUE`; see [speaker_links])           |
#'    | **chair_name**      | Name of the sitting chair (president or meeting leader)                               |
#'    | **chair_id**        | Id of the sitting chair, when given in the transcript                                 |
#'    | **chair_linked_id** | Id of the sitting chair, linked from `chair_name` (with `link = TRUE`)                |
#'    | **text**            | Speech text, one line per paragraph                                                   |
#'
#' @md
#'
#' @seealso [speaker_links] [get_publication] [get_session_publications] [get_session_meetings] [get_case]
#'
#' @examples
#'
#' \dontrun{
#' speeches <- get_speeches("s140213")
#' head(speeches[, c("speech_type", "speaker_name", "person_id", "linked_person_id")])
#'
#' # Without the linked ids
#' speeches <- get_speeches("s140213", link = FALSE)
#' }
#'
#' @import rvest httr2 stringr
#' @importFrom xml2 xml_attr xml_find_all xml_find_first xml_name xml_parent xml_path xml_root xml_text
#'
#' @export
#'
get_speeches <- function(publicationid = NA, good_manners = 0, link = TRUE){

  if(length(publicationid) > 1)
    return(fetch_multi(publicationid, get_speeches, good_manners, link = link))

  url <- paste0("https://data.stortinget.no/eksport/publikasjon?publikasjonid=", publicationid)

  tmp <- api_get(url, as = "xml")

  tmp2 <- parse_speeches(tmp, publicationid)

  # The transcripts have no response date of their own, so record the time of retrieval
  retrieved <- sub("(\\d{2})(\\d{2})$", "\\1:\\2", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  tmp2 <- data.frame(response_date = rep(retrieved, nrow(tmp2)), tmp2)

  if(link) tmp2 <- link_speakers(tmp2)

  Sys.sleep(good_manners)

  return(tmp2)

}

#' Add person ids linked from speaker and chair names
#'
#' Looks up `speaker_name` and `chair_name` by session in the [speaker_links]
#' dataset, keeping the order of the rows. Places `linked_person_id` and
#' `link_method` after `person_id`, and `chair_linked_id` after `chair_id`.
#'
#' @param x A data.frame from \code{\link{parse_speeches}}.
#'
#' @keywords internal
#' @noRd
link_speakers <- function(x) {

  links <- stortingscrape::speaker_links

  key <- paste(links$speaker_name, links$session_id, sep = "\r")

  speaker <- match(paste(x$speaker_name, x$session_id, sep = "\r"), key)
  speaker[is.na(x$speaker_name)] <- NA

  chair <- match(paste(x$chair_name, x$session_id, sep = "\r"), key)
  chair[is.na(x$chair_name)] <- NA

  x$linked_person_id <- links$linked_person_id[speaker]
  x$link_method <- links$link_method[speaker]
  x$chair_linked_id <- links$linked_person_id[chair]

  cols <- names(x)[!names(x) %in% c("linked_person_id", "link_method", "chair_linked_id")]
  cols <- append(cols, c("linked_person_id", "link_method"), after = match("person_id", cols))
  cols <- append(cols, "chair_linked_id", after = match("chair_id", cols))

  x[, cols]

}

#' Parse the speeches of a debate transcript
#'
#' Internal parser behind \code{\link{get_speeches}}, separated from the API call
#' so that it can be run on transcripts stored locally (read with
#' \code{xml2::read_xml()}).
#'
#' @param doc An \pkg{xml2} document of a transcript, parsed as XML.
#' @param publicationid Character. Id of the transcript.
#'
#' @return A data.frame, see \code{\link{get_speeches}}.
#'
#' @keywords internal
#' @noRd
parse_speeches <- function(doc, publicationid) {

  root <- xml_name(xml_root(doc))

  if(!(root %in% c("forhandling", "Forhandlinger"))) {
    stop(paste0("Publication '", publicationid, "' is not a debate transcript (root element <", root, ">)."),
         call. = FALSE)
  }

  old <- root == "forhandling"

  # All XPath calls below pass `ns = character()`: the transcripts have no namespaces, and
  # the default (xml_ns()) walks the whole document on every call, which makes parsing
  # large transcripts very slow

  # Meetings
  meetings <- xml_find_all(doc, "/*/mote | /*/Mote", ns = character())

  meeting_id <- xml_attr(meetings, if(old) "sakid" else "moteID")
  meeting_id[meeting_id == ""] <- NA

  meeting_title <- vapply(meetings, function(x) {
    first_text(x, "./formalia/dato | ./Startseksjon/Motestart/Tittel | ./Startseksjon/Folkevalgte/Forhtit")
  }, character(1))

  meeting_date <- meeting_dates(meeting_title, publicationid)

  # Speeches (speech elements; one element can hold several speakers, see speech_segments())
  speeches <- xml_find_all(doc, if(old) "//innlegg | //presinnl" else "//Hovedinnlegg | //Replikk | //Presinnlegg", ns = character())

  meeting_order <- match(
    vapply(speeches, function(x) xml_path(xml_find_first(x, "ancestor::mote | ancestor::Mote", ns = character())), character(1)),
    xml_path(meetings)
  )

  case_node <- lapply(speeches, function(x) {
    xml_find_first(x, "ancestor::*[not(self::mote)][@sakid != '' or @sakID != ''][1]", ns = character())
  })

  agenda_node <- lapply(speeches, function(x) xml_find_first(x, "ancestor::*[@saksKartNr != ''][1]", ns = character()))

  speech_type <- vapply(speeches, function(x) {
    switch(xml_name(x),
           innlegg = xml_attr(x, "type"),
           presinnl = "presinnlegg",
           tolower(xml_name(x)))
  }, character(1))

  elements <- data.frame(
    publication_id = rep(publicationid, length(speeches)),
    session_id = session_from_date(meeting_date[meeting_order]),
    meeting_order = meeting_order,
    meeting_id = meeting_id[meeting_order],
    meeting_title = meeting_title[meeting_order],
    meeting_date = meeting_date[meeting_order],
    section = tolower(vapply(speeches, function(x) xml_name(xml_parent(x)), character(1))),
    case_id = vapply(case_node, function(x) {
      id <- xml_attr(x, "sakid")
      if(is.na(id)) xml_attr(x, "sakID") else id
    }, character(1)),
    agenda_no = vapply(agenda_node, xml_attr, character(1), attr = "saksKartNr"),
    agenda_merged = vapply(agenda_node, xml_attr, character(1), attr = "sammenslatteSaker"),
    speech_order = seq_along(speeches),
    speech_id = xml_attr(speeches, "Id"),
    speech_type = speech_type,
    track_chair(doc, speeches)
  )

  # One row per speaker within each speech element
  segments <- lapply(speeches, speech_segments)

  n_segments <- vapply(segments, nrow, integer(1))

  segments <- do.call(rbind, c(list(speech_segments(NULL)), segments))

  tmp <- cbind(elements[rep(seq_along(speeches), n_segments), ], segments)

  tmp <- tmp[, c("publication_id", "session_id", "meeting_order", "meeting_id", "meeting_title",
                 "meeting_date", "section", "case_id", "agenda_no", "agenda_merged", "speech_order",
                 "speech_part", "speech_id", "speech_type", "speaker_raw", "speaker_title",
                 "speaker_name", "speaker_party", "speech_time", "person_id", "chair_name",
                 "chair_id", "text")]

  tmp$agenda_merged[tmp$agenda_merged == ""] <- NA

  rownames(tmp) <- NULL

  return(tmp)

}

#' Squish whitespace and remove soft hyphens
#' @keywords internal
#' @noRd
clean_text <- function(x) {

  x <- x |>
    str_remove_all("\u00ad") |>
    str_squish()

  x[x == ""] <- NA

  x

}

#' Cleaned text of the first node matching an XPath, or NA
#' @keywords internal
#' @noRd
first_text <- function(x, xpath) {

  clean_text(xml_text(xml_find_first(x, xpath, ns = character())))

}

#' Split a speech element into one row per speaker
#'
#' A speech element usually holds one speaker, but some hold an exchange (e.g. a
#' question and an answer in a hearing, or the president handing over the
#' floor). A new row starts at every paragraph with a non-empty speaker name.
#' Text before the first name gets a row without a speaker. Called with
#' \code{NULL}, returns an empty data.frame with the right columns.
#'
#' @return A data.frame with \code{speech_part}, the speaker variables, and the
#'   text (one line per paragraph, without the speaker name).
#'
#' @keywords internal
#' @noRd
speech_segments <- function(x) {

  if(is.null(x)) {
    return(data.frame(speech_part = integer(0), speaker_raw = character(0), parse_speaker(character(0)),
                      person_id = character(0), text = character(0)))
  }

  blocks <- xml_find_all(x, "./navn | .//a[not(ancestor::a)] | .//merknad[not(ancestor::a)] | .//A[not(ancestor::A)]", ns = character())

  navn <- lapply(blocks, function(b) if(xml_name(b) == "navn") b else xml_find_first(b, "./Navn", ns = character()))

  speaker_raw <- vapply(navn, function(n) clean_text(xml_text(n)), character(1))
  speaker_raw[str_detect(speaker_raw, "^[\\s:]*$")] <- NA

  text <- vapply(blocks, function(b) {
    if(xml_name(b) == "navn") return(NA_character_)
    paste(xml_text(xml_find_all(b, ".//text()[not(ancestor::Navn)]", ns = character())), collapse = "")
  }, character(1)) |>
    clean_text()

  starts <- !is.na(speaker_raw)
  part <- cumsum(starts)

  # Text before the first speaker name (if any) is part 0
  parts <- unique(c(if(length(part) == 0 || part[1] == 0) 0L, part[starts]))

  out <- lapply(parts, function(p) {
    first <- which(part == p & starts)[1]
    txt <- text[part == p & !is.na(text)]
    data.frame(
      speaker_raw = if(is.na(first)) NA_character_ else speaker_raw[first],
      # Some ids carry a numeric suffix (e.g. "ARK_775612110" for "ARK")
      person_id = if(is.na(first)) NA_character_ else str_remove(xml_attr(navn[[first]], "personID"), "_\\d+$"),
      text = if(length(txt) == 0) NA_character_ else paste(txt, collapse = "\n")
    )
  })

  out <- do.call(rbind, out)

  # Drop an empty lead-in row when a named speaker follows
  if(nrow(out) > 1 && is.na(out$speaker_raw[1]) && is.na(out$text[1])) out <- out[-1, ]

  out$person_id[out$person_id %in% ""] <- NA

  data.frame(speech_part = seq_len(nrow(out)), speaker_raw = out$speaker_raw,
             parse_speaker(out$speaker_raw), person_id = out$person_id, text = out$text)

}

#' The sitting chair at each speech element
#'
#' Walks the transcript in document order: the chair is the president (or
#' meeting leader) named at the start of each meeting, and changes at notes like
#' "X hadde her overtatt presidentplassen" or "X overtok her som møteleder".
#' Notes about the chair that cannot be parsed, or that name a role rather than
#' a person, set the chair to unknown (NA) until the next recognized change.
#' \code{chair_id} is only known for the chair named at the start of a meeting,
#' and only when the transcript gives it. A numeric suffix that some of these ids
#' carry in the transcripts (e.g. "OLET_62710109") is removed.
#'
#' @return A data.frame with \code{chair_name} and \code{chair_id}, one row per
#'   speech element.
#'
#' @keywords internal
#' @noRd
track_chair <- function(doc, speeches) {

  chair_name <- rep(NA_character_, length(speeches))
  chair_id <- rep(NA_character_, length(speeches))

  if(length(speeches) == 0) return(data.frame(chair_name, chair_id))

  # e.g. "X hadde her overtatt presidentplassen", "X gjeninntok her presidentplassen",
  # "X overtok her igjen som m\u00f8teleder"
  change_pattern <- "^(.+?) (hadde|overtok|gjeninntok|tok)\\b.*(presidentplass|som m\u00f8tele)"

  notes_xpath <- paste0("[contains(., 'presidentplass') or contains(., 'som m\u00f8tele')]")

  events <- xml_find_all(doc, paste(
    "/*/mote | /*/Mote | /*/mote/formalia/president | /*/Mote/Startseksjon/Motestart/President",
    paste0("| //a", notes_xpath), paste0("| //A", notes_xpath),
    "| //innlegg | //presinnl | //Hovedinnlegg | //Replikk | //Presinnlegg"
  ), ns = character())

  speech_paths <- xml_path(speeches)

  current_name <- NA_character_
  current_id <- NA_character_

  for(e in events) {

    type <- xml_name(e)

    if(type %in% c("mote", "Mote")) {

      current_name <- NA_character_
      current_id <- NA_character_

    } else if(type %in% c("president", "President")) {

      # e.g. "President: X", "Møteleder: X (A) (komiteens leder)", or
      # "Møtet ble ledet av utenriks- og forsvarskomiteens leder, X."
      current_name <- xml_text(e) |>
        clean_text() |>
        str_remove(regex("^m\u00f8tet ble (ledet|leia|leidd) av .*,\\s*", ignore_case = TRUE)) |>
        str_remove(regex("^(president|m\u00f8teleder|m\u00f8teleiar|leder|leiar)\\s*:\\s*", ignore_case = TRUE)) |>
        str_remove_all("\\s*\\([^)]*\\)") |>
        str_remove("\\.$") |>
        clean_text()
      # Some chair ids carry a numeric suffix (e.g. "OLET_62710109" for "OLET")
      current_id <- str_remove(xml_attr(e, "personID"), "_\\d+$")
      if(current_id %in% "") current_id <- NA_character_

    } else if(type %in% c("a", "A")) {

      txt <- clean_text(xml_text(e))
      name <- str_match(txt, change_pattern)[, 2]
      is_note <- xml_name(xml_parent(e)) %in% c("handling", "Handling")

      if(!is.na(name)) {
        valid <- str_detect(name, "^[A-Z\u00c6\u00d8\u00c5]") && !str_detect(name, regex("president|leder|leiar", ignore_case = TRUE))
        current_name <- if(valid) name else NA_character_
        current_id <- NA_character_
      } else if(is_note) {
        # A note about the chair we cannot read: chair unknown from here
        current_name <- NA_character_
        current_id <- NA_character_
      }

    } else {

      i <- match(xml_path(e), speech_paths)
      chair_name[i] <- current_name
      chair_id[i] <- current_id

    }

  }

  data.frame(chair_name, chair_id)

}

#' Parliamentary session of a date, in the API's session id format
#'
#' Sessions run from October to September. Sessions starting in 1998 or earlier
#' have ids like "1998-99"; later sessions have ids like "1999-2000".
#'
#' @keywords internal
#' @noRd
session_from_date <- function(x) {

  start <- as.integer(format(x, "%Y")) - (as.integer(format(x, "%m")) < 10)

  out <- sprintf("%d-%d", start, start + 1)

  short <- !is.na(start) & start <= 1998
  out[short] <- sprintf("%d-%02d", start[short], (start[short] + 1) %% 100)

  out[is.na(start)] <- NA

  out

}

#' Split a raw speaker string into title, name, party, and time stamp
#'
#' Speaker strings follow the pattern `[Title] [Name] [(Party)] [[hh:mm:ss]][:]`,
#' e.g. `Statsråd Jan Tore Sanner [11:03:37]:` or `Kjersti Toppe (Sp) [10:00:37]:`.
#'
#' @keywords internal
#' @noRd
parse_speaker <- function(x) {

  # Time stamps are usually in brackets, but sometimes in parentheses or with a bracket missing.
  # A garbled time stamp (extra digits, e.g. "12:02:329") is removed, but gives no time
  time_pattern <- "[\\[(]?\\s*(\\d{1,2})[:.](\\d{2})[:.](\\d{2})(\\d*)\\s*[\\])]?"

  time <- str_match(x, time_pattern)

  rest <- x |>
    str_remove_all(time_pattern) |>
    str_remove_all("\\[[^\\]]*\\]") |>
    str_remove_all("[\\[\\]]") |>
    str_squish() |>
    str_remove("[\\s:]+$") |>
    str_remove("\\s*\\($")

  # Trailing parentheticals, e.g. "(Sp)" or "(A) (komiteens leder)": the party is the
  # first one that is a party id, and NA when none is (the text is kept in speaker_raw)
  parens <- str_extract(rest, "(\\s*\\([^()]+\\))+$")

  rest <- str_remove(rest, "(\\s*\\([^()]+\\))+$")

  party <- vapply(parens, function(p) {
    if(is.na(p)) return(NA_character_)
    p <- str_squish(str_match_all(p, "\\(([^()]+)\\)")[[1]][, 2])
    known <- harmonize_party(p, known_only = TRUE)
    known[!is.na(known)][1]
  }, character(1), USE.NAMES = FALSE)

  title_pattern <- regex(
    paste0("^(Statsr\u00e5d|Statsminister|Stortingspresident(en)?|Visepresident(en)?|Presidenten|",
           "M\u00f8telederen|M\u00f8teleiaren|Lederen|Leiaren|Fung\\. leder|Fungerende leder|Komiteens leder|",
           "[A-Z\u00c6\u00d8\u00c5][a-z\u00e6\u00f8\u00e5-]*minister)(?=\\s|$)"),
    ignore_case = TRUE
  )

  title <- str_extract(rest, title_pattern)
  has_title <- !is.na(title)
  title[has_title] <- paste0(toupper(substr(title[has_title], 1, 1)), substring(title[has_title], 2))

  name <- str_squish(str_remove(rest, title_pattern))
  name[name == ""] <- NA

  speech_time <- sprintf("%02d:%s:%s", as.integer(time[, 2]), time[, 3], time[, 4])
  speech_time[is.na(time[, 1]) | time[, 5] != ""] <- NA

  data.frame(
    speaker_title = title,
    speaker_name = name,
    speaker_party = harmonize_party(party),
    speech_time = speech_time
  )

}

#' Harmonize party abbreviations in transcripts to the API's party ids
#'
#' Matches case-insensitively and without a trailing period (e.g. "Frp" and
#' "FrP." -> "FrP"), maps variants of
#' independent ("uavh.", "uav") to "Uav", and keeps unknown values as they are
#' (or returns NA for them when \code{known_only = TRUE}).
#'
#' @keywords internal
#' @noRd
harmonize_party <- function(x, known_only = FALSE) {

  party_ids <- c("A", "ALP", "B", "DNF", "FFF", "FrP", "H", "Kp", "KrF", "MDG", "NKP", "NSA",
                 "PF", "R", "RF", "RV", "SF", "Sp", "SV", "SVf", "TF", "Uav", "V")

  out <- party_ids[match(tolower(str_remove(x, "\\.$")), tolower(party_ids))]

  out[is.na(out) & str_detect(x, regex("^uav", ignore_case = TRUE))] <- "Uav"
  out[is.na(out) & x %in% "Ap"] <- "A"

  if(!known_only) out[is.na(out)] <- x[is.na(out)]

  out

}

#' Meeting date encoded in a transcript id
#'
#' Transcripts up to 2015-2016 have ids like "s140213" (chamber letter and
#' yymmdd, optionally followed by "k" for evening sittings). Later transcripts
#' have ids like "refs-201718-02-14" (series, session, month, day), where the
#' year follows from the session running from October to September.
#'
#' @keywords internal
#' @noRd
date_from_publicationid <- function(x) {

  old <- str_match(x, "^[A-Za-z](\\d{2})(\\d{2})(\\d{2})")
  new <- str_match(x, "^ref[a-z]-(\\d{4})\\d{2}-(\\d{2})-(\\d{2})")

  if(!is.na(old[1, 1])) {
    year <- as.integer(old[1, 2])
    year <- ifelse(year > 50, 1900 + year, 2000 + year)
    return(as.Date(paste(year, old[1, 3], old[1, 4], sep = "-"), format = "%Y-%m-%d"))
  }

  if(!is.na(new[1, 1])) {
    month <- as.integer(new[1, 3])
    year <- as.integer(new[1, 2]) + (month < 10)
    return(as.Date(paste(year, month, new[1, 4], sep = "-"), format = "%Y-%m-%d"))
  }

  as.Date(NA)

}

#' Meeting date from the meeting heading and the transcript id
#'
#' The heading gives a weekday, day, and month (e.g. "torsdag den 13. februar
#' 2014"), and from 2007 onward also the year; when it does not, the year is
#' taken from the id. The heading is anchored on the weekday, because headings
#' can mention other dates. Both sources contain errors (a wrong year in the
#' heading, an id off by a day or a year, or a wrong weekday), so: when the
#' heading and the id agree, that date is used; when they disagree, the one whose
#' weekday matches the weekday in the heading is used, and NA if neither or both
#' match.
#'
#' @param x Character. Meeting headings.
#' @param publicationid Character. Id of the transcript.
#'
#' @keywords internal
#' @noRd
meeting_dates <- function(x, publicationid) {

  months <- c("januar", "februar", "mars", "april", "mai", "juni", "juli",
              "august", "september", "oktober", "november", "desember")

  weekdays <- c(mandag = 1, "m\u00e5ndag" = 1, tirsdag = 2, tysdag = 2, onsdag = 3, torsdag = 4,
                fredag = 5, "l\u00f8rdag" = 6, laurdag = 6, "s\u00f8ndag" = 0)

  from_id <- rep(date_from_publicationid(publicationid), length(x))

  m <- str_match(tolower(x), paste0("(", paste(names(weekdays), collapse = "|"), ")\\s+(?:den\\s+)?(\\d{1,2})\\.\\s*(",
                                    paste(months, collapse = "|"), ")(?:\\s+(\\d{4}))?"))

  weekday <- unname(weekdays[m[, 2]])
  year <- ifelse(is.na(m[, 5]), format(from_id, "%Y"), m[, 5])
  from_title <- as.Date(paste(year, match(m[, 4], months), m[, 3], sep = "-"), format = "%Y-%m-%d")

  fits <- function(d) !is.na(d) & !is.na(weekday) & as.POSIXlt(d)$wday == weekday

  out <- from_title
  out[is.na(from_title)] <- from_id[is.na(from_title)]

  disagree <- !is.na(from_title) & !is.na(from_id) & from_title != from_id
  use_title <- disagree & fits(from_title) & !fits(from_id)
  use_id <- disagree & fits(from_id) & !fits(from_title)

  out[disagree] <- NA
  out[use_title] <- from_title[use_title]
  out[use_id] <- from_id[use_id]

  out

}
