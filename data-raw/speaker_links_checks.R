## Checks for the `speaker_links` dataset
##
## Tests every linked speech against the linked person's biography
## (get_mp_bio()) on the date of the meeting. Does not change the dataset; prints
## a summary and saves the flagged cases to the cache for inspection.
##
##   - speaker held a seat (MP or substitute) or a cabinet post on the date
##   - the party in the transcript matches the person's party on the date
##   - speakers titled Statsråd/Statsminister/Utenriksminister held a cabinet post
##   - plenary chairs were members of the presidency on the date (this flags many
##     correct links: the transcripts show that other MPs regularly took the chair,
##     e.g. "Eirin Faldet hadde her overtatt presidentplassen", 2005-2009)
##   - where the transcript also gives an id (person_id, chair_id), it agrees
##
## Run from the package root after data-raw/speaker_links.R, with the same cache
## (data-raw/cache, or STORTINGSCRAPE_CACHE if set). Biographies are cached in
## <cache>/bios.

pkgload::load_all()
library(dplyr)
library(stringr)

cache_dir <- Sys.getenv("STORTINGSCRAPE_CACHE", "data-raw/cache")
dir.create(file.path(cache_dir, "bios"), showWarnings = FALSE)

as_day <- function(x) { d <- as.Date(substr(x, 1, 10)); d[d < as.Date("1800-01-01")] <- NA; d }
covers <- function(from, to, d) !is.na(from) & from <= d & (is.na(to) | to >= d)

speeches <- readRDS(file.path(cache_dir, "speeches.rds")) |>
  left_join(speaker_links, by = c("speaker_name", "session_id")) |>
  left_join(speaker_links |> select(speaker_name, session_id, chair_linked_id = linked_person_id),
            by = c("chair_name" = "speaker_name", "session_id")) |>
  mutate(row = row_number(),
         series = str_extract(publication_id, "^(ref[a-z]|[a-z])"),
         plenary = series %in% c("s", "o", "l", "refs"))

# Biographies of every linked person
ids <- sort(unique(na.omit(c(speeches$linked_person_id, speeches$chair_linked_id))))
for(id in ids[!file.exists(file.path(cache_dir, "bios", paste0(ids, ".rds")))]) {
  saveRDS(tryCatch(suppressMessages(get_mp_bio(id)), error = function(e) NULL),
          file.path(cache_dir, "bios", paste0(id, ".rds")))
}
bios <- setNames(lapply(file.path(cache_dir, "bios", paste0(ids, ".rds")), readRDS), ids)
message(sum(vapply(bios, is.null, TRUE)), " of ", length(ids), " biographies missing")

seats <- bind_rows(lapply(names(bios), function(id) {
  p <- bios[[id]]$parl_periods
  if(is.null(p) || nrow(p) == 0) return(NULL)
  data.frame(id, party = p$party_id, from = as_day(p$from_date), to = as_day(p$to_date))
}))
positions <- bind_rows(lapply(names(bios), function(id) {
  p <- bios[[id]]$parl_positions
  if(is.null(p) || nrow(p) == 0) return(NULL)
  data.frame(id, ctype = p$committee_type, cid = p$committee_id, from = as_day(p$from_date), to = as_day(p$to_date))
}))

facts <- function(d) {
  s <- d |> inner_join(seats, by = "id", relationship = "many-to-many") |> filter(covers(from, to, date)) |>
    group_by(row) |> summarise(seat = TRUE, parties = list(unique(party)), .groups = "drop")
  p <- d |> inner_join(positions, by = "id", relationship = "many-to-many") |> filter(covers(from, to, date)) |>
    group_by(row) |> summarise(regj = any(ctype == "REGJ"), pres = any(ctype == "PRES"),
                               part = list(unique(cid[ctype == "PART"])), .groups = "drop")
  d |> left_join(s, by = "row") |> left_join(p, by = "row") |>
    mutate(across(c(seat, regj, pres), \(x) coalesce(x, FALSE)))
}

spk <- speeches |> filter(!is.na(linked_person_id)) |>
  transmute(row, id = linked_person_id, date = meeting_date) |> facts() |>
  left_join(speeches |> select(-any_of(c("id", "date"))), by = "row") |>
  mutate(present = seat | regj,
         party_known = lengths(Map(c, part, parties)) > 0,
         party_ok = ifelse(is.na(speaker_party) | !party_known, NA,
                           mapply(function(p, a, b) p %in% c(a, b), speaker_party, part, parties)),
         title_ok = ifelse(speaker_title %in% c("Statsr\u00e5d", "Statsminister", "Utenriksminister"), regj, NA),
         api_ok = ifelse(is.na(person_id), NA, person_id == linked_person_id))

chr <- speeches |> filter(!is.na(chair_linked_id)) |>
  distinct(publication_id, meeting_order, chair_name, .keep_all = TRUE) |>
  transmute(row, id = chair_linked_id, date = meeting_date) |> facts() |>
  left_join(speeches |> select(-any_of(c("id", "date"))), by = "row") |>
  mutate(api_ok = ifelse(is.na(chair_id), NA, chair_id == chair_linked_id))

cat("\n== Speakers (speeches with a linked id) ==\n")
print(spk |> group_by(plenary, era = ifelse(meeting_date < as.Date("2016-10-01"), "before 2016-17", "2016-17 on")) |>
  summarise(speeches = n(), no_seat_or_post = sum(!present), party_differs = sum(party_ok %in% FALSE),
            minister_title_no_post = sum(title_ok %in% FALSE), api_id_differs = sum(api_ok %in% FALSE), .groups = "drop") |>
  as.data.frame())

cat("\n== Chairs (distinct chair per meeting) ==\n")
print(chr |> group_by(plenary) |>
  summarise(chairs = n(), plenary_not_in_presidency = sum(plenary & !pres), api_id_differs = sum(api_ok %in% FALSE), .groups = "drop") |>
  as.data.frame())

flags <- bind_rows(
  spk |> filter(!present) |> mutate(check = "speaker: no seat or cabinet post on date"),
  spk |> filter(party_ok %in% FALSE) |> mutate(check = "speaker: party differs on date"),
  spk |> filter(title_ok %in% FALSE) |> mutate(check = "speaker: minister title, no cabinet post"),
  spk |> filter(api_ok %in% FALSE) |> mutate(check = "speaker: API person_id differs"),
  chr |> filter(plenary, !pres) |> mutate(check = "chair: plenary, not in presidency", speaker_raw = chair_name),
  chr |> filter(api_ok %in% FALSE) |> mutate(check = "chair: API chair_id differs", speaker_raw = chair_name)) |>
  mutate(api_id = ifelse(grepl("^chair", check), chair_id, person_id)) |>
  count(check, session_id, speaker_raw, linked = id, api_id, name = "speeches")

saveRDS(flags, file.path(cache_dir, "speaker_links_flags.rds"))
cat("\n== Flags by check (distinct person x session) ==\n")
print(count(flags, check))
