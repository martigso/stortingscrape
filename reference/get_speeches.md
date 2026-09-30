# Retrieve the speeches in a debate transcript

A function for retrieving all speeches in a debate transcript
(publication type "referat"), one row per speech. Handles both the
transcript format used up to the 2015-2016 session and the format used
from the 2016-2017 session onward, returning the same variables for
both. Plenary sittings, open hearings, and meetings in the European
Committee are all published as transcripts.

## Usage

``` r
get_speeches(publicationid = NA, good_manners = 0)
```

## Arguments

- publicationid:

  Character string, or a vector of strings, indicating the id of the
  transcript to retrieve. Ids can be found with
  [get_session_publications](https://martigso.github.io/stortingscrape/reference/get_session_publications.md)
  (`type = "referat"`)

- good_manners:

  Integer. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

A data.frame with the following variables:

|  |  |
|----|----|
|  |  |
| **publication_id** | Id of the transcript |
| **session_id** | Id of the parliamentary session (see [get_parlsessions](https://martigso.github.io/stortingscrape/reference/get_parlsessions.md)), from `meeting_date` |
| **meeting_order** | Order of the meeting within the transcript (some transcripts hold several meetings) |
| **meeting_id** | Meeting id (see [get_session_meetings](https://martigso.github.io/stortingscrape/reference/get_session_meetings.md)), when given in the transcript |
| **meeting_title** | Raw meeting heading (e.g. "Møte torsdag den 13. februar 2014 kl. 10") |
| **meeting_date** | Date of the meeting, from `meeting_title` and the publication id (see details) |
| **section** | Name of the XML element directly containing the speech (e.g. sak, spm, formalia) |
| **case_id** | Id of the case the speech belongs to (see [get_case](https://martigso.github.io/stortingscrape/reference/get_case.md)), when given in the transcript |
| **agenda_no** | Agenda item number (from 2016-2017 onward) |
| **agenda_merged** | Agenda item numbers debated together, comma separated (from 2016-2017 onward) |
| **speech_order** | Order of the speech element within the transcript |
| **speech_part** | Order of the speaker within the speech element (usually 1) |
| **speech_id** | Speech element id (from 2016-2017 onward) |
| **speech_type** | Type of speech ("hovedinnlegg", "replikk", or "presinnlegg") |
| **speaker_raw** | Speaker as written in the transcript |
| **speaker_title** | Title parsed from `speaker_raw` (e.g. "Statsråd", "Presidenten") |
| **speaker_name** | Name parsed from `speaker_raw` |
| **speaker_party** | Party parsed from `speaker_raw`, harmonized to the party ids of [get_all_parties](https://martigso.github.io/stortingscrape/reference/get_all_parties.md) |
| **speech_time** | Time stamp parsed from `speaker_raw` (hh:mm:ss) |
| **person_id** | Id of the speaker (see [get_mp](https://martigso.github.io/stortingscrape/reference/get_mp.md)), when given in the transcript |
| **chair_name** | Name of the sitting chair (president or meeting leader) |
| **chair_id** | Id of the sitting chair, when given in the transcript |
| **text** | Speech text, one line per paragraph |

## Details

Some speech elements in the transcripts hold more than one speaker (e.g.
a question and an answer in a hearing). These are split into one row per
speaker; `speech_order` identifies the speech element and `speech_part`
the speaker within it.

The meeting date is given both in the meeting heading (weekday, day,
month, and, from 2007 onward, year) and in the publication id, and both
contain occasional errors. When they agree, that date is used; when they
disagree, the one whose weekday matches the weekday in the heading is
used, and `NA` if that does not settle it.

Speakers are only identified by the API (`person_id`) in transcripts
from the 2016-2017 session onward, and not for all speeches (e.g. not
for the president or committee chair). Note that these ids are not
always correct: speeches are sometimes tagged with the id of another
person than the one named in `speaker_raw`. A numeric suffix that some
of these ids carry (e.g. "ARK_775612110") is removed. The raw speaker
string is always kept in `speaker_raw`; `speaker_title`, `speaker_name`,
`speaker_party`, and `speech_time` are parsed from it. See
[speaker_links](https://martigso.github.io/stortingscrape/reference/speaker_links.md)
for person ids linked from the names.

The sitting chair (president or meeting leader) is tracked through the
transcript: the chair named at the start of each meeting, updated at the
transcript's notes on changes of chair (e.g. "X hadde her overtatt
presidentplassen"). When a note about the chair cannot be read, the
chair is set to `NA` until the next recognized change. `chair_id` is
only given for the chair named at the start of a meeting, and only when
the transcript includes it (a numeric suffix that some of these ids
carry, e.g. "OLET_62710109", is removed).

## See also

[get_publication](https://martigso.github.io/stortingscrape/reference/get_publication.md)
[get_session_publications](https://martigso.github.io/stortingscrape/reference/get_session_publications.md)
[get_session_meetings](https://martigso.github.io/stortingscrape/reference/get_session_meetings.md)
[get_case](https://martigso.github.io/stortingscrape/reference/get_case.md)

## Examples

``` r

if (FALSE) { # \dontrun{
speeches <- get_speeches("refs-202425-06-12")
head(speeches[, c("speech_type", "speaker_name", "speaker_party", "person_id")])
} # }
```
