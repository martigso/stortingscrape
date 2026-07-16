# Retrieve vote results on MP level for a specified vote

A function for retrieving vote results from a specific vote on MP level.
Vote data are only available from the 2011-2012 session. Needs some
preprocessing for use with rollcall packages, such as
[ideal](https://rdrr.io/pkg/pscl/man/ideal.html).

## Usage

``` r
get_result_vote(voteid = NA, good_manners = 0)
```

## Arguments

- voteid:

  Character string, or a vector of strings, indicating the id of the
  vote to retrieve results from

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
| **vote_id** | Id of vote |
| **mp_id** | MP id |
| **party_id** | Party id |
| **vote** | Vote: for, mot (against), ikke_tilstede (absent) |
| **permanent_sub_for** | Id of the MP originally holding the seat, if the substitute is permanent |
| **sub_for** | Id of the MP originally holding the seat |

## See also

[get_decision_votes](https://martigso.github.io/stortingscrape/reference/get_decision_votes.md)
[get_proposal_votes](https://martigso.github.io/stortingscrape/reference/get_proposal_votes.md)
[get_vote](https://martigso.github.io/stortingscrape/reference/get_vote.md)
[get_mp_bio](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md)

## Examples

``` r

if (FALSE) { # \dontrun{

v <- get_result_vote(12345)
table(v$vote)

p <- get_proposal_votes(12345)

stringr::str_replace_all(p$proposal_vote$proposal_text, 
                         "\\<(.*)\\>|\\r\\n", "")  |> 
  stringr::str_trim()
} # }
 
```
