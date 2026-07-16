# Retreive all votes for a specified vote proposal

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

  Integer. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

A list with two elements:

1.  **\$proposal_vote** (main data on the vote proposal)

    |                   |                           |
    |-------------------|---------------------------|
    |                   |                           |
    | **response_date** | Date of data retrieval    |
    | **version**       | Data version from the API |
    | **vote_id**       | Id of the vote            |

2.  **\$proposal_by_parties\${proposal_id}** (what parties (id) stood
    behind proposal(s))

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
