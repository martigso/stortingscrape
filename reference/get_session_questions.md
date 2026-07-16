# Parliamentary questions in a session

A function for retrieving all questions within a parliamentary session.

## Usage

``` r
get_session_questions(sessionid = NA, q_type = NA, status = NA, good_manners = 0)
```

## Arguments

- sessionid:

  Character string, or a vector of strings, indicating the id of the
  session to retrieve questions from

- q_type:

  Character string indicating type of question to retrieve. Options are
  "interpellasjoner" (interpellations), "sporretimesporsmal" (oral
  questions), or "skriftligesporsmal" (written questions).

- status:

  Character string question status extraction. Possible values are NA
  (extract all questions), "til_behandling" (pending questions),
  "trukket" (withdrawn questions), "bortfalt" (lapsed questions), or
  "alle" (all questions)

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
| **answ_by_id** | Id of minister answering question |
| **answ_by_minister_id** | Department id of answering minister |
| **answ_by_minister_title** | Department title of answering minister |
| **answ_date** | Date answer was given |
| **answ_on_belhalf_of** | Answer given on behalf of |
| **answ_on_belhalf_of_minister_id** | Department id of minister given answer on behalf of |
| **answ_on_belhalf_of_minister_title** | Department title of minister given answer on behalf of |
| **topic_ids** | Id of relevant topics for question |
| **moved_to** | Question moved to |
| **asked_by_other_id** | MP id, if question was not asked by the questioning MP |
| **id** | Question id |
| **correct_person** | Not documented in API |
| **correct_person_minister_id** | Not documented in API |
| **correct_person_minister_title** | Not documented in API |
| **sendt_date** | Date the question was sent |
| **session_id** | Session id |
| **question_from_id** | Question from MP id |
| **question_number** | Question number within session |
| **question_to_id** | Question directed to minister id |
| **question_to_minister_id** | Question directed to minister department id |
| **question_to_minister_title** | Question directed to minister department title |
| **status** | Question status |
| **title** | Question title |
| **type** | Question type |

## See also

[get_question_hour](https://martigso.github.io/stortingscrape/reference/get_question_hour.md)
[get_question](https://martigso.github.io/stortingscrape/reference/get_question.md)

## Examples

``` r

if (FALSE) { # \dontrun{
interp <- get_session_questions(sessionid = "2013-2014", 
                                q_type = "interpellasjoner", 
                                status = "trukket")
interp
} # }
```
