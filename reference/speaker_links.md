# Person ids for speakers and chairs in debate transcripts

A crosswalk from the names of speakers and chairs, as written in the
debate transcripts (see
[get_speeches](https://martigso.github.io/stortingscrape/reference/get_speeches.md)),
to person ids, by parliamentary session. Join it to the output of
[get_speeches](https://martigso.github.io/stortingscrape/reference/get_speeches.md)
by `speaker_name` and `session_id` (speakers) or by `chair_name` and
`session_id` (chairs).

## Usage

``` r
speaker_links
```

## Format

A data frame with 4 columns and 8195 rows

- speaker_name:

  Name as written in the transcripts (`speaker_name` or `chair_name` in
  [get_speeches](https://martigso.github.io/stortingscrape/reference/get_speeches.md))

- session_id:

  Id of the parliamentary session

- linked_person_id:

  Id of the person (see
  [get_mp](https://martigso.github.io/stortingscrape/reference/get_mp.md)),
  or `NA` when the name cannot be linked

- link_method:

  How the name was linked: on full name, first and last name, or as a
  changed name, to an MP roster and/or a minister spell; "ambiguous"
  when several persons match; "substitute only, no party or title" when
  not linked for that reason

## Source

Built by `data-raw/speaker_links.R` from
<https://data.stortinget.no/eksport/publikasjon>,
<https://data.stortinget.no/eksport/representanter>, and a list of
ministers and their periods in office from regjeringen.no, supplemented
by <https://data.stortinget.no/eksport/kodetbiografi>.

## Details

Names are linked to MPs and substitutes of the session's parliamentary
period
([get_parlperiod_mps](https://martigso.github.io/stortingscrape/reference/get_parlperiod_mps.md))
and to ministers in office during the session. A name is linked only
when exactly one person matches, first on the full name and then on
first and last name. Names with no such match can still be linked as a
changed name (e.g. a surname added or dropped at marriage): when the
first name is the same, every word of the shorter name is in the longer
one, exactly one MP in the period fits, and the name is written with
that MP's party throughout the session. A name that only matches a
substitute registration, and is never written with a party, a title, or
as chair in the session (e.g. a witness in a hearing), is not linked.
There are no manual corrections: names that cannot be resolved this way
(e.g. witnesses in hearings, misspelled names, and some changed names)
have `linked_person_id` `NA`.

The links are made from names only and do not use the `person_id` given
in the transcripts from 2016-2017 onward, which is sometimes wrong in
hearings and committee meetings.

Ministers and their periods in office come from a list of ministers from
1945 to January 2024, supplemented by the cabinet posts in
[get_mp_bio](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md)
for ministers in office after that. The minister data will be updated in
a later version.

The dataset covers the transcripts available in the API when it was
built (sessions 1998-99 onward). The API's list of transcripts for
2008-2009 fails, so these transcripts were found by their ids,
constructed from
[get_session_meetings](https://martigso.github.io/stortingscrape/reference/get_session_meetings.md);
open hearings in that session are therefore missing. Four listed
transcripts (s031205, s050526k, s602081, o006051) could not be retrieved
from the API.

## Examples

``` r
if (FALSE) { # \dontrun{

speeches <- get_speeches("refs-202425-06-12")

speeches <- merge(speeches, speaker_links, by = c("speaker_name", "session_id"), all.x = TRUE)

} # }
```
