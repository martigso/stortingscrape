# Speeches in the Storting's meeting on 13 February 2014

A dataset containing all speeches in the transcript of the Storting's
meeting on 13 February 2014 (publication id "s140213"), as returned by
[get_speeches](https://martigso.github.io/stortingscrape/reference/get_speeches.md)

## Usage

``` r
speeches140213
```

## Format

A data frame with 23 columns and 48 rows (see
[get_speeches](https://martigso.github.io/stortingscrape/reference/get_speeches.md)
for details)

- publication_id:

  Id of the transcript

- session_id:

  Id of the parliamentary session

- meeting_order:

  Order of the meeting within the transcript

- meeting_id:

  Meeting id, when given in the transcript

- meeting_title:

  Raw meeting heading

- meeting_date:

  Date of the meeting

- section:

  Name of the XML element directly containing the speech

- case_id:

  Id of the case the speech belongs to, when given in the transcript

- agenda_no:

  Agenda item number (from 2016-2017 onward)

- agenda_merged:

  Agenda item numbers debated together (from 2016-2017 onward)

- speech_order:

  Order of the speech element within the transcript

- speech_part:

  Order of the speaker within the speech element

- speech_id:

  Speech element id (from 2016-2017 onward)

- speech_type:

  Type of speech

- speaker_raw:

  Speaker as written in the transcript

- speaker_title:

  Title parsed from `speaker_raw`

- speaker_name:

  Name parsed from `speaker_raw`

- speaker_party:

  Party parsed from `speaker_raw`

- speech_time:

  Time stamp parsed from `speaker_raw`

- person_id:

  Id of the speaker, when given in the transcript

- chair_name:

  Name of the sitting chair

- chair_id:

  Id of the sitting chair, when given in the transcript

- text:

  Speech text, one line per paragraph

## Source

<https://data.stortinget.no/eksport/publikasjon?publikasjonid=s140213>
