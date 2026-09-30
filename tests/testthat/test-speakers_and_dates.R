test_that("speaker strings are split into title, name, party, and time", {
  x <- c("Kjersti Toppe (Sp) [10:00:37]:", "Statsråd Jan Tore Sanner [11:03:37]:",
         "Karin Yrvin (A) (komiteens leder):", "Statsråd Erik Solheim (11:37:38):",
         "Eivind Drivenes (Sp.):", "Carl I. Hagen (Frp):", "presidenten:", NA)
  p <- parse_speaker(x)
  expect_equal(p$speaker_name, c("Kjersti Toppe", "Jan Tore Sanner", "Karin Yrvin", "Erik Solheim",
                                 "Eivind Drivenes", "Carl I. Hagen", NA, NA))
  expect_equal(p$speaker_party, c("Sp", NA, "A", NA, "Sp", "FrP", NA, NA))
  expect_equal(p$speaker_title, c(NA, "Statsråd", NA, "Statsråd", NA, NA, "Presidenten", NA))
  expect_equal(p$speech_time, c("10:00:37", "11:03:37", NA, "11:37:38", NA, NA, NA, NA))
})

test_that("broken time stamps and non-party parentheticals are handled", {
  x <- c("Ine Eriksen S\u00f8reide (H) [10:22:31:", "Statsr\u00e5d Lene V\u00e5gslid 14:25:31]:",
         "Dagfinn Henrik Olsen (", "Ulf Leirstein (av):", "Martin Kolberg (komiteens leder):",
         "Statsr\u00e5d Terje Aasland [12:02:329:")
  p <- parse_speaker(x)
  expect_equal(p$speaker_name, c("Ine Eriksen S\u00f8reide", "Lene V\u00e5gslid", "Dagfinn Henrik Olsen",
                                 "Ulf Leirstein", "Martin Kolberg", "Terje Aasland"))
  expect_equal(p$speech_time, c("10:22:31", "14:25:31", NA, NA, NA, NA))
  expect_equal(p$speaker_party, c("H", NA, NA, NA, NA, NA))
})

test_that("party abbreviations are harmonized to the API's party ids", {
  expect_equal(harmonize_party(c("Frp", "uavh.", "Ap", "Sp.", "av")), c("FrP", "Uav", "A", "Sp", "av"))
  expect_equal(harmonize_party("av", known_only = TRUE), NA_character_)
})

test_that("meeting dates resolve conflicts between heading and id by the weekday", {
  m <- function(title, id) as.character(meeting_dates(title, id))
  # Wrong year in the heading
  expect_equal(m("Møte onsdag den 18. juni 2013 kl. 9", "s140618"), "2014-06-18")
  # Wrong id
  expect_equal(m("Møte i Europautvalget mandag den 10. juni 2024 kl. 8.30", "refe-202425-06-10"), "2024-06-10")
  # Wrong weekday, but heading and id agree
  expect_equal(m("Møte tirsdag den 8. april 2026 kl. 10", "refs-202526-04-08"), "2026-04-08")
  # Sitting past midnight, heading without year
  expect_equal(m("Møte tirsdag den 18. juni kl. 02.25 (Dagsorden for mandag den 17. juni)", "l020617"), "2002-06-18")
  # Heading mentioning another date
  expect_equal(m(paste("Open høring i Den særskilte komité ... i Stortingets møte 10. november 2011",
                       "onsdag den 18. januar 2012 kl. 11.15"), "h120118"), "2012-01-18")
  # No weekday in the heading
  expect_equal(m("Åpning av det 154. storting", "s091009"), "2009-10-09")
})

test_that("sessions follow the API's id format", {
  expect_equal(session_from_date(as.Date(c("1998-11-16", "1999-10-01", "2014-02-13", NA))),
               c("1998-99", "1999-2000", "2013-2014", NA))
  expect_type(session_from_date(as.Date(character(0))), "character")
})

test_that("speaker and chair names are linked from speaker_links", {
  x <- data.frame(session_id = "2013-2014", speaker_name = c("Kjersti Toppe", NA), person_id = NA,
                  chair_name = "Olemic Thommessen", chair_id = NA, text = "")
  y <- link_speakers(x)
  expect_equal(y$linked_person_id, c("KJT", NA))
  expect_equal(y$chair_linked_id, c("OLET", "OLET"))
  expect_equal(names(y), c("session_id", "speaker_name", "person_id", "linked_person_id", "link_method",
                           "chair_name", "chair_id", "chair_linked_id", "text"))
})
