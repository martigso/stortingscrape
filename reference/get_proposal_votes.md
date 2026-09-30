# Retrieve all proposals for a specified vote

A function for retrieving all votes from a specific vote proposal. Vote
data are only available from the 2011-2012 session

## Usage

``` r
get_proposal_votes(voteid = NA, good_manners = 0)
```

## Arguments

- voteid:

  Character string, or a vector of strings, indicating the id of the
  vote to retrieve proposals from

- good_manners:

  Numeric. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

A list with two elements:

1.  **\$proposal_vote** (main data on the proposals, one row per
    proposal)

    |                                |                                      |
    |--------------------------------|--------------------------------------|
    |                                |                                      |
    | **response_date**              | Date of data retrieval               |
    | **version**                    | Data version from the API            |
    | **vote_id**                    | Id of the vote                       |
    | **proposal_designation**       | Designation of the proposal          |
    | **proposal_designation_short** | Short designation of the proposal    |
    | **proposal_id**                | Id of the proposal                   |
    | **proposal_delivered_by_mp**   | Id of the MP delivering the proposal |
    | **proposal_on_behalf_of_text** | Text on whose behalf the proposal is |
    | **proposal_sortingnumber**     | Sorting number of the proposal       |
    | **proposal_text**              | Text of the proposal                 |
    | **proposal_type**              | Type of proposal                     |

2.  **\$proposal_by_parties** (a list named by `proposal_id`, each
    element a character vector of the ids of the parties behind the
    proposal)

## See also

[get_vote](https://martigso.github.io/stortingscrape/reference/get_vote.md)
[get_decision_votes](https://martigso.github.io/stortingscrape/reference/get_decision_votes.md)
[get_result_vote](https://martigso.github.io/stortingscrape/reference/get_result_vote.md)

## Examples

``` r

if (FALSE) { # \dontrun{

prop <- get_proposal_votes(7523)
prop

for(i in 1:length(prop$proposal_by_parties)){
    prop$proposal_vote$parties[i] <- paste0(prop$proposal_by_parties[[i]], 
                                            collapse = ", ")

}

} # }

```
