## Build the `speaker_links` dataset
##
## A crosswalk from speaker and chair names, as written in the debate transcripts
## (publication type "referat"), to person ids, by parliamentary session.
##
## Steps:
##   1. List all transcripts with get_session_publications(). The list for a
##      session can fail on the API side (2008-2009 returns HTTP 500); for such
##      sessions, ids are constructed from get_session_meetings() as chamber
##      letter and date ("s081015"), with and without "k" for evening sittings.
##      Constructed ids that do not exist return 404 and are dropped.
##   2. Download every transcript once to `cache_dir` (raw XML) and parse it with
##      the package's speech parser (the parser behind get_speeches()).
##   3. Collect every speaker name and chair name by session.
##   4. Match names to persons. Candidates are MPs and substitutes in the
##      session's parliamentary period (get_parlperiod_mps()) and ministers in
##      office during the session (ministers.rds, with spells for ministers in
##      office after the file ends taken from get_mp_bio()). A name links when
##      exactly one person matches, first on the full name, then on first and
##      last name. A name that only matches a substitute registration, and is
##      never seen with a party, a title, or as chair in the session (e.g. a
##      witness in a hearing), is not linked. There are no manual corrections:
##      anything that cannot be resolved this way is NA.
##   5. Save with usethis::use_data().
##
## ministers.rds is an extract (person id, name, cabinet, start, end) of a list of
## ministers from 1945 to January 2024; it will be replaced by an updated minister
## dataset in a later version.
##
## Run from the package root. The cache (raw XML, a download log, and the parsed
## speeches; about 1.4 GB) is kept in data-raw/cache, which is ignored by git and
## by the package build; set STORTINGSCRAPE_CACHE to use another folder. A rerun
## only downloads what is missing.

pkgload::load_all()
library(httr2)
library(dplyr)
library(stringr)

cache_dir <- Sys.getenv("STORTINGSCRAPE_CACHE", "data-raw/cache")
dir.create(file.path(cache_dir, "xml"), recursive = TRUE, showWarnings = FALSE)
message("Cache: ", cache_dir)

# Requests made directly by this script: at most 10 in a burst and 90 per minute
# after, so no 60-second window exceeds the API limit of 100 calls per minute
api_raw <- function(url) {
  tryCatch(
    request(url) |>
      req_throttle(capacity = 10, fill_time_s = 10 / 1.5) |>
      req_retry(max_tries = 5, is_transient = function(r) resp_status(r) %in% c(429, 503)) |>
      req_timeout(120) |>
      req_error(is_error = function(r) FALSE) |>
      req_perform(),
    error = function(e) NULL)
}

norm_name <- function(x) x |> tolower() |> str_replace_all("[-.,]", " ") |> str_squish()
first_last <- function(x) paste(word(x, 1), word(x, -1))
as_day <- function(x) { d <- as.Date(substr(x, 1, 10)); d[d < as.Date("1800-01-01")] <- NA; d }


# 1. Transcript ids ------------------------------------------------------------

sessions <- get_parlsessions() |>
  mutate(from = as_day(from), to = as_day(to)) |>
  filter(from <= Sys.Date())

lists <- lapply(sessions$id, function(s) {
  tryCatch(suppressMessages(get_session_publications(s, type = "referat")), error = function(e) NULL)
})
names(lists) <- sessions$id

failed_lists <- names(lists)[vapply(lists, is.null, TRUE)]
message("Transcript lists failing on the API side: ", paste(failed_lists, collapse = ", "))

ids <- bind_rows(lists) |> filter(!is.na(publication_id)) |> distinct(publication_id) |> pull()

constructed <- unlist(lapply(failed_lists, function(s) {
  stems <- get_session_meetings(s) |>
    filter(meeting_place %in% c("storting", "odelsting", "lagting")) |>
    mutate(stem = paste0(substr(meeting_place, 1, 1), format(as_day(meeting_date), "%y%m%d"))) |>
    distinct(stem) |> pull()
  c(stems, paste0(stems, "k"))
}))

ids <- unique(c(ids, constructed))


# 2. Download and parse --------------------------------------------------------

log_file <- file.path(cache_dir, "download_log.csv")

done <- sub("\\.xml$", "", list.files(file.path(cache_dir, "xml")))
if(file.exists(log_file)) {
  log <- read.csv(log_file)
  done <- c(done, log$id[log$status >= 400 & log$status < 500])
}
todo <- setdiff(ids, done)
message(length(ids), " transcript ids, ", length(todo), " to download")

for(i in seq_along(todo)) {
  id <- todo[i]
  t0 <- Sys.time()
  resp <- api_raw(paste0("https://data.stortinget.no/eksport/publikasjon?publikasjonid=", URLencode(id, reserved = TRUE)))
  status <- if(is.null(resp)) NA else resp_status(resp)
  if(status %in% 200) writeBin(resp_body_raw(resp), file.path(cache_dir, "xml", paste0(id, ".xml")))
  write.table(data.frame(id, status, bytes = if(is.null(resp)) NA else length(resp_body_raw(resp)),
                         seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1),
                         time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
              log_file, sep = ",", row.names = FALSE, col.names = !file.exists(log_file), append = TRUE)
  if(i %% 100 == 0) message(format(Sys.time(), "%H:%M:%S"), " ", i, "/", length(todo))
}

files <- file.path(cache_dir, "xml", paste0(ids, ".xml"))
files <- files[file.exists(files)]

parsed <- parallel::mclapply(files, function(f) {
  tryCatch(parse_speeches(xml2::read_xml(f), sub("\\.xml$", "", basename(f))),
           error = function(e) conditionMessage(e))
}, mc.cores = max(1, parallel::detectCores() - 1))

parse_failed <- vapply(parsed, is.character, TRUE)
if(any(parse_failed)) {
  message(sum(parse_failed), " transcripts failed to parse:\n",
          paste(basename(files[parse_failed]), unlist(parsed[parse_failed]), sep = ": ", collapse = "\n"))
}

speeches <- bind_rows(parsed[!parse_failed])
saveRDS(speeches, file.path(cache_dir, "speeches.rds"))
message(nrow(speeches), " speeches from ", sum(!parse_failed), " transcripts")


# 3. Names by session ----------------------------------------------------------

names_sessions <- bind_rows(
  speeches |> filter(!is.na(speaker_name)) |> distinct(name = speaker_name, session_id),
  speeches |> filter(!is.na(chair_name)) |> distinct(name = chair_name, session_id)) |>
  distinct() |>
  left_join(sessions |> select(session_id = id, from, to), by = "session_id")

# Names seen in an MP role in the session: with a party or title, or as chair
in_role <- bind_rows(
  speeches |> filter(!is.na(speaker_name), !is.na(speaker_party) | !is.na(speaker_title)) |>
    distinct(name = speaker_name, session_id),
  speeches |> filter(!is.na(chair_name)) |> distinct(name = chair_name, session_id)) |>
  distinct() |>
  mutate(in_role = TRUE)


# 4. Candidates and links ------------------------------------------------------

# Parliamentary period of each session
periods <- get_parlperiods() |> mutate(from = as_day(from), to = as_day(to))
names_sessions$period_id <- vapply(names_sessions$from, function(d) {
  p <- periods$id[periods$from <= d & periods$to >= d]
  if(length(p) == 1) p else NA_character_
}, character(1))

# MPs and substitutes by period; substitute-only when the person holds no regular seat in the period
mps <- get_parlperiod_mps(unique(na.omit(names_sessions$period_id)), substitute = TRUE) |>
  transmute(id = mp_id, period_id, full = norm_name(paste(firstname, lastname)), party = party_id,
            sub = substitute_mp == "true") |>
  group_by(id, period_id, full) |>
  summarise(sub_only = all(sub), parties = list(unique(party)), .groups = "drop") |>
  mutate(fl = first_last(full))

# Ministers: spells from ministers.rds, except open spells, which are replaced by the cabinet
# posts in get_mp_bio() for those persons and for the members of the current government
ministers_file <- readRDS("data-raw/ministers.rds")

current <- api_raw("https://data.stortinget.no/eksport/regjering") |>
  resp_body_xml() |>
  xml2::xml_find_all("//*[local-name() = 'regjeringsmedlem']")
current <- data.frame(
  person_id = xml2::xml_text(xml2::xml_find_first(current, "./*[local-name() = 'id']")),
  first_name = xml2::xml_text(xml2::xml_find_first(current, "./*[local-name() = 'fornavn']")),
  last_name = xml2::xml_text(xml2::xml_find_first(current, "./*[local-name() = 'etternavn']")))

from_bios <- union(ministers_file$person_id[is.na(ministers_file$end)], current$person_id)

bio_spells <- bind_rows(lapply(from_bios, function(id) {
  p <- suppressMessages(get_mp_bio(id))$parl_positions
  p <- p[p$committee_type == "REGJ", ]
  if(nrow(p) == 0) return(NULL)
  data.frame(person_id = id, start = as_day(p$from_date), end = as_day(p$to_date))
})) |>
  left_join(bind_rows(ministers_file |> distinct(person_id, first_name, last_name), current) |>
              distinct(person_id, .keep_all = TRUE), by = "person_id")

ministers <- bind_rows(ministers_file |> filter(!is.na(end)), bio_spells) |>
  transmute(id = person_id, full = norm_name(paste(first_name, last_name)), start, end) |>
  distinct() |>
  mutate(fl = first_last(full))

key <- names_sessions |> mutate(nm = norm_name(name), fl = first_last(nm))

candidates <- function(by) {
  bind_rows(
    key |> inner_join(mps, by = c(by, "period_id"), relationship = "many-to-many") |>
      transmute(name, session_id, id, source = "mp", sub_only),
    key |> inner_join(ministers, by = by, relationship = "many-to-many") |>
      filter(start <= to, is.na(end) | end >= from) |>
      transmute(name, session_id, id, source = "minister", sub_only = FALSE))
}

resolve <- function(cand, method) {
  cand |>
    group_by(name, session_id) |>
    summarise(n_ids = n_distinct(id), linked_person_id = first(id), sub_only = all(sub_only),
              link_method = paste(method, paste(sort(unique(source)), collapse = "+")), .groups = "drop")
}

by_full <- resolve(candidates(c("nm" = "full")), "full name,")
by_first_last <- resolve(candidates("fl"), "first + last name,") |>
  anti_join(by_full, by = c("name", "session_id"))

# Changed names (e.g. a surname added or dropped at marriage): only for names with no other
# candidate, written as one clean name (no parentheses, digits, commas, or " og ") with one and
# the same party throughout the session. An MP in the session's period fits when the first name
# is the same and every word of the shorter name (at least two words) is in the longer one. The
# name links when exactly one MP in the period fits, across all parties, and that MP's party in
# the period is the party written in the transcripts.
one_party <- speeches |>
  filter(!is.na(speaker_name), !is.na(speaker_party)) |>
  group_by(name = speaker_name, session_id) |>
  summarise(party = if(n_distinct(speaker_party) == 1) first(speaker_party) else NA_character_,
            .groups = "drop") |>
  filter(!is.na(party))

nested_names <- function(a, b) {
  a <- strsplit(a, " ")[[1]]
  b <- strsplit(b, " ")[[1]]
  min(length(a), length(b)) >= 2 && (all(a %in% b) || all(b %in% a))
}

by_name_change <- key |>
  anti_join(bind_rows(by_full, by_first_last), by = c("name", "session_id")) |>
  filter(!str_detect(name, "[()0-9,]| og ")) |>
  inner_join(one_party, by = c("name", "session_id")) |>
  mutate(first_word = word(nm, 1)) |>
  inner_join(mps |> mutate(first_word = word(full, 1)), by = c("period_id", "first_word"), relationship = "many-to-many") |>
  filter(mapply(nested_names, nm, full)) |>
  group_by(name, session_id) |>
  filter(n_distinct(id) == 1, mapply(function(p, ps) p %in% ps, party, parties)) |>
  summarise(n_ids = 1L, linked_person_id = first(id), sub_only = FALSE,
            link_method = "changed name (same first name and party), mp", .groups = "drop")

speaker_links <- names_sessions |>
  select(speaker_name = name, session_id) |>
  left_join(bind_rows(by_full, by_first_last, by_name_change), by = c("speaker_name" = "name", "session_id")) |>
  left_join(in_role, by = c("speaker_name" = "name", "session_id")) |>
  mutate(unverified = n_ids %in% 1 & sub_only & is.na(in_role),
         linked_person_id = ifelse(n_ids %in% 1 & !unverified, linked_person_id, NA),
         link_method = case_when(n_ids > 1 ~ "ambiguous",
                                 unverified ~ "substitute only, no party or title",
                                 TRUE ~ link_method)) |>
  select(speaker_name, session_id, linked_person_id, link_method) |>
  arrange(session_id, speaker_name) |>
  as.data.frame()


# 5. Save ----------------------------------------------------------------------

message(nrow(speaker_links), " names by session, ",
        sum(!is.na(speaker_links$linked_person_id)), " linked")

message("Links from changed names, for review:")
print(by_name_change |>
        left_join(mps |> distinct(linked_person_id = id, roster_name = full), by = "linked_person_id") |>
        group_by(name, linked_person_id) |>
        summarise(roster_name = first(roster_name), sessions = paste(session_id, collapse = ", "), .groups = "drop") |>
        as.data.frame())

usethis::use_data(speaker_links, overwrite = TRUE)
