# Retrieve question hour details for a specified meeting

A function for retrieving detailed overview of the question hour for a
specific meeting

## Usage

``` r
get_question_hour(meetingid = NA, good_manners = 0)
```

## Arguments

- meetingid:

  Character string, or a vector of strings, indicating the id of the
  meeting to retrieve the question hour from

- good_manners:

  Integer. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

A list with ten data frames:

1.  **\$root** (download meta data)

    |                   |                           |
    |-------------------|---------------------------|
    |                   |                           |
    | **response_date** | Date of data retrieval    |
    | **version**       | Data version from the API |
    | **meetingid**     | The called meeting id     |

2.  **\$question_hour_ministers** (id of ministers in parliament during
    question hour/time)

    |        |                           |
    |--------|---------------------------|
    |        |                           |
    | **id** | Id of ministers attending |

3.  **\$question_time**

    |  |  |
    |----|----|
    |  |  |
    | question_justification | Justification for question |
    | answer_by_id | Id for answering minister |
    | answer_by_minister_id | Id for department of answering minister |
    | answer_by_minister_title | Title for department of answering minister |
    | answer_date | Date of receiving answer |
    | answer_on_behalf_of_id | Id of minister answered on behalf of, when relevant |
    | answer_on_behalf_of_minister_id | Id of department answered on behalf of, when relevant |
    | answer_on_behalf_of_minister_title | Title of department answered on behalf of, when relevant |
    | agenda_case_number | Case number on agenda |
    | date | Date question hour was held |
    | moved_to | Date moved to |
    | asked_by_other_id | Id for minister asking on behalf of another, when relevant |
    | question_id | Question id |
    | correct_person | Not documented in API |
    | correct_person_minister_id | Not documented in API |
    | correct_person_minister_title | Not documented in API |
    | sent_date | Date question was sent |
    | session_id | Session id |
    | question_text | Full question text |
    | question_from_id | Id of MP asking the question |
    | question_number | Question number |
    | question_to_id | Id of minister the question was asked to |
    | question_to_minister_id | Department id of minister the question was asked to |
    | question_to_minister_title | Department title of minister the question was asked to |
    | status | Question status |
    | answer | Answer text (often empty) |
    | title | Question title |
    | type | Question type |

4.  **\$publication_reference**

    |               |                                                   |
    |---------------|---------------------------------------------------|
    |               |                                                   |
    | **export_id** | Export id for publication (see get_publication()) |
    | **link_text** | Description text for publication                  |
    | **link_url**  | URL for publication                               |
    | **type**      | Type of publication                               |
    | **sub_type**  | Subtype for publication (location)                |

## See also

[get_question](https://martigso.github.io/stortingscrape/reference/get_question.md)
[get_session_questions](https://martigso.github.io/stortingscrape/reference/get_session_questions.md)
[get_publication](https://martigso.github.io/stortingscrape/reference/get_publication.md)

## Examples

``` r

if (FALSE) { # \dontrun{
get_question_hour(10232)
} # }

 
```
