# Retrieve all meetings for a specified parliamentary session

A function for retrieving meetings from a specific parliamentary session

## Usage

``` r
get_session_meetings(sessionid = NA, good_manners = 0)
```

## Arguments

- sessionid:

  Character string, or a vector of strings, indicating the id of the
  session to retrieve meetings from

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
| **response_date** | Date of data retrieval |
| **version** | Data version from the API |
| **session_id** | Session id |
| **agenda_number** | Agenda number within the session |
| **footnote** | Footnotes for the meeting |
| **meeting_id** | Meeting id |
| **no_meeting_text** | Description of why there was no meeting, if relevant |
| **evening_meeting** | Whether the meeting was an evening meeting or not |
| **note** | Note for the meeting |
| **meeting_date** | Date the meeting took place |
| **meeting_order** | Indicator for meeting order |
| **meeting_place** | Where the meeting took place |
| **transcript_id** | Id for transcript (usually empty) |
| **additional_agenda** | Logical indicator for whether there was additional agenda to the meeting |

## See also

[get_meeting_agenda](https://martigso.github.io/stortingscrape/reference/get_meeting_agenda.md)
[get_question_hour](https://martigso.github.io/stortingscrape/reference/get_question_hour.md)

## Examples

``` r

if (FALSE) { # \dontrun{

meet <- get_session_meetings("2013-2014")
head(meet)

} # }
 
```
