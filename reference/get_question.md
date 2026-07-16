# Parliamentary question

A function for retrieving single parliamentary questions by id. For
retrieving the whole debate over a question, the
[get_publication](https://martigso.github.io/stortingscrape/reference/get_publication.md)
function has to be used

## Usage

``` r
get_question(questionid = NA, good_manners = 0)
```

## Arguments

- questionid:

  Character string, or a vector of strings, indicating the id of the
  question to retrieve

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
| **justification** | Justification for question |
| **answ_by_id** | Id for answering minister |
| **answ_by_minister_id** | Id for department of answering minister |
| **answ_by_minister_title** | Title for department of answering minister |
| **answ_date** | Date question was asked |
| **answ_on_belhalf_of** | Id of minister answered on behalf of, when relevant |
| **answ_on_belhalf_of_minister_id** | Id of department answered on behalf of, when relevant |
| **answ_on_belhalf_of_minister_title** | Title of department answered on behalf of, when relevant |
| **agenda_number** | Agenda number in meeting |
| **moved_to** | Date moved to |
| **id** | Question id |
| **legacy_id** | Question legacy id (only use to match with data downloaded before 10.12.2024) |
| **correct_person_id** | Not documented in API |
| **correct_person_minister_id** | Not documented in API |
| **correct_person_minister_title** | Not documented in API |
| **sendt_date** | Date question was sent |
| **session_id** | Session id |
| **question_text** | Full question text |
| **question_from_id** | Id of MP asking the question |
| **qustion_number** | Question number |
| **qustion_to_id** | Id of minister the question was asked to |
| **qustion_to_minister_id** | Department id of minister the question was asked to |
| **qustion_to_minister_title** | Department title of minister the question was asked to |
| **answer_text** | Answer text (often empty) |
| **title** | Question title |
| **type** | Question type |
| **asked_by_other_id** | ID of person that asked the question on behalf of `question_from_id` |

## See also

[get_question_hour](https://martigso.github.io/stortingscrape/reference/get_question_hour.md)
[get_publication](https://martigso.github.io/stortingscrape/reference/get_publication.md)
[get_meeting_agenda](https://martigso.github.io/stortingscrape/reference/get_meeting_agenda.md)

## Examples

``` r

if (FALSE) { # \dontrun{
# An example of a possible workflow

## Retreive sessions
sessions <- get_parlsessions()

## Retreive all interpellations for a particular session
qsesh <- get_session_questions(sessions$id[9], q_type = "interpellasjoner")

## Retreve detailed information on all interpellations in that session
library(pbmcapply) # for progress bar. never use paralell on scraping
int1213 <- pbmclapply(qsesh$id, function(x){
    get_question(x, good_manners = 2)
}, mc.cores = 1)

quest1213 <- do.call(rbind, int1213)
} # }
```
