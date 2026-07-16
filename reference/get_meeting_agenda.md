# Retreive agenda for a specified meeting

A function for retrieving the agenda for a specific meeting.

## Usage

``` r
get_meeting_agenda(meetingid = NA, good_manners = 0)
```

## Arguments

- meetingid:

  Character string, or a vector of strings, indicating the id of the
  meeting to retrieve the agenda from

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
| **agenda_number** | The agenda number for the session |
| **meeting_date** | Date of the meeting |
| **meeting_id** | Meeting id |
| **meeting_place** | Where the meeting was held |
| **agenda_case_reference** | Reference for the case on the agenda |
| **agenda_case_number** | Case number |
| **agenda_case_text** | Case description |
| **agenda_case_type** | Case type |
| **footnote** | Footnote for the case |
| **proposition_id** | If relevant, belonging proposition id |
| **committee_id** | If relevant, id of the responsible committee |
| **legacy_question_id** | Legacy question id, only used for mathing with old data (pre 10.12.2024) |
| **loose_proposals** | Whether there are loose proposals to the case |
| **case_id** | Id of the case |
| **question_hour_type** | If relevant, type of question hour |
| **question_id** | If relevant, question id |

## See also

[get_session_meetings](https://martigso.github.io/stortingscrape/reference/get_session_meetings.md)
[get_case](https://martigso.github.io/stortingscrape/reference/get_case.md)
[get_question](https://martigso.github.io/stortingscrape/reference/get_question.md)
[get_question_hour](https://martigso.github.io/stortingscrape/reference/get_question_hour.md)

## Examples

``` r

if (FALSE) { # \dontrun{

meetings0910 <- get_session_meetings("2009-2010")
meeting_agenda <- get_meeting_agenda(meetings0910$meeting_id[161])
meeting_agenda
} # }
 
```
