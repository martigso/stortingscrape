# Retreive votes for a specific case

A function for retrieving all votes from a case. Vote data are only
available from the 2011-2012 session

## Usage

``` r
get_vote(caseid = NA, good_manners = 0)
```

## Arguments

- caseid:

  Character string, or a vector of strings, indicating the id of the
  case to request all votes from

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
| **case_id** | Case id up for vote |
| **alternative_vote** | Whether vote is an alternative vote |
| **n_for** | Number of votes for |
| **n_absent** | Number of MPs absent |
| **n_against** | Number of votes against |
| **treatment_order** | Order of treated votes |
| **agenda_case_number** | Case number on the agenda of the meeting |
| **free_vote** | Logical indication of whether the vote is related to the case as a whole |
| **comment** | Vote comment |
| **meeting_map_number** | Number on the meeting map |
| **personal_vote** | Logical indication of whether vote was recorded as roll call or not |
| **president_id** | Id of president holding president chair at the time of voting |
| **president_party_id** | Party of the sitting president |
| **adopted** | Logical indication of whether the proposal voted on was adopted |
| **vote_id** | Id of vote |
| **vote_method** | Voting method |
| **vote_result_type** | Result type (enstemmig_vedtatt = unanimously adopted) |
| **vote_result_type_text** | See **vote_result_type** |
| **vote_topic** | Description of the proposal voted upon |
| **vote_datetime** | Date and time of vote |

## See also

[get_decision_votes](https://martigso.github.io/stortingscrape/reference/get_decision_votes.md)
[get_proposal_votes](https://martigso.github.io/stortingscrape/reference/get_proposal_votes.md)
get_vote
[get_session_cases](https://martigso.github.io/stortingscrape/reference/get_session_cases.md)
[get_case](https://martigso.github.io/stortingscrape/reference/get_case.md)

## Examples

``` r

if (FALSE) { # \dontrun{

get_vote(63033)

} # }

 
```
