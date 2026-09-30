# Small transcripts in the two formats, written to cover specific behavior

old_format <- xml2::read_xml(paste0(
  "<forhandling><mote><formalia>",
  "<dato>Møte torsdag den 13. februar 2014 kl. 10</dato>",
  "<president>President: Olemic Thommessen</president></formalia>",
  "<saker><sak sakid=\"58920\">",
  "<innlegg type=\"hovedinnlegg\"><navn>Kjersti Toppe (Sp) [10:00:37]:</navn>",
  "<a>Tekst en.</a><a>Tekst to.</a></innlegg>",
  "<handling><a>Morten Wold hadde her overtatt presidentplassen.</a></handling>",
  "<presinnl><a>Presidentens innlegg.</a></presinnl>",
  "</sak></saker></mote></forhandling>"))

new_format <- xml2::read_xml(paste0(
  "<Forhandlinger><Mote moteID=\"1\"><Startseksjon><Motestart>",
  "<Tittel>Møte onsdag den 18. juni 2014 kl. 10</Tittel>",
  "<President personID=\"OLET_62710109\"><A>President: Olemic Thommessen</A></President>",
  "</Motestart></Startseksjon><Hovedseksjon><Saker>",
  "<Sak sakID=\"100\" saksKartNr=\"1\"><Hovedinnlegg Id=\"i1\">",
  "<A><Navn personID=\"ARK_775612110\">Arve Kambe (H) [10:01:00]:</Navn> Første.</A>",
  "<A><Navn personID=\"SVES\">Sveinung Stensland (H):</Navn> Svar.</A>",
  "</Hovedinnlegg></Sak></Saker></Hovedseksjon></Mote></Forhandlinger>"))

test_that("old-format transcripts are parsed", {
  d <- parse_speeches(old_format, "s140213")
  expect_equal(nrow(d), 2)
  expect_equal(d$speaker_name, c("Kjersti Toppe", NA))
  expect_equal(d$speaker_party, c("Sp", NA))
  expect_equal(d$speech_time, c("10:00:37", NA))
  expect_equal(d$speech_type, c("hovedinnlegg", "presinnlegg"))
  expect_equal(d$case_id, c("58920", "58920"))
  expect_equal(d$meeting_date, as.Date(c("2014-02-13", "2014-02-13")))
  expect_equal(d$session_id, c("2013-2014", "2013-2014"))
  expect_equal(d$text[1], "Tekst en.\nTekst to.")
  expect_true(all(is.na(d$person_id)))
})

test_that("the chair is tracked through the transcript", {
  d <- parse_speeches(old_format, "s140213")
  expect_equal(d$chair_name, c("Olemic Thommessen", "Morten Wold"))
})

test_that("new-format transcripts are parsed, with several speakers split into rows", {
  d <- parse_speeches(new_format, "refs-201314-06-18")
  expect_equal(nrow(d), 2)
  expect_equal(d$speech_order, c(1L, 1L))
  expect_equal(d$speech_part, 1:2)
  expect_equal(d$speaker_name, c("Arve Kambe", "Sveinung Stensland"))
  expect_equal(d$person_id, c("ARK", "SVES"))
  expect_equal(d$text, c("Første.", "Svar."))
  expect_equal(d$case_id, c("100", "100"))
  expect_equal(d$agenda_no, c("1", "1"))
  expect_equal(d$chair_id, c("OLET", "OLET"))
})

test_that("all columns keep their type when there are no speeches", {
  empty <- xml2::read_xml("<forhandling><mote><formalia><dato>Møte</dato></formalia></mote></forhandling>")
  d <- parse_speeches(empty, "o051020")
  expect_equal(nrow(d), 0)
  expect_type(d$session_id, "character")
  expect_type(d$speech_time, "character")
})

test_that("other publication types are refused", {
  expect_error(parse_speeches(xml2::read_xml("<Innstilling/>"), "inns-202324-065s"), "not a debate transcript")
})
