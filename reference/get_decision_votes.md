# Retreive vote decision for a specified vote

A function for retrieving vote decisions from a specific vote. Vote data
are only available from the 2011-2012 session

## Usage

``` r
get_decision_votes(voteid = NA, good_manners = 0)
```

## Arguments

- voteid:

  Character string, or a vector of strings, indicating the id of the
  vote to retrieve decisions from

- good_manners:

  Integer. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

A data.frame with the following variables:

|                        |                            |
|------------------------|----------------------------|
|                        |                            |
| **response_date**      | Date of data retrieval     |
| **version**            | Data version from the API  |
| **vote_id**            | Id of the vote             |
| **decision_code**      | General code for decision  |
| **decision_comment**   | Comments for the decision  |
| **decision_number**    | Decision number            |
| **decision_reference** | Reference for the decision |
| **decision_text**      | Full text of the decision  |

## See also

[get_session_decisions](https://martigso.github.io/stortingscrape/reference/get_session_decisions.md)
[get_proposal_votes](https://martigso.github.io/stortingscrape/reference/get_proposal_votes.md)
[get_vote](https://martigso.github.io/stortingscrape/reference/get_vote.md)
[get_result_vote](https://martigso.github.io/stortingscrape/reference/get_result_vote.md)

## Examples

``` r
if (FALSE) { # \dontrun{
decision <- get_decision_votes(123)
decision
} # }
 
```
