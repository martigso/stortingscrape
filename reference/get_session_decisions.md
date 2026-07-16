# Retrieve all decisions for a specified session

A function for retrieving all decisions from a specific parliamentary
session.

## Usage

``` r
get_session_decisions(sessionid = NA, good_manners = 0)
```

## Arguments

- sessionid:

  Character string, or a vector of strings, indicating the id of the
  session to retrieve decisions from

- good_manners:

  Integer. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

A data.frame with the following variables:

|                        |                                        |
|------------------------|----------------------------------------|
|                        |                                        |
| **response_date**      | Date of data retrieval                 |
| **version**            | Data version from the API              |
| **session_id**         | Session id                             |
| **decision_id**        | Decision id                            |
| **case_id**            | Case id                                |
| **case_link_url**      | URL for case to front end web page     |
| **decision_date**      | Decision date                          |
| **decision_link_url**  | URL for decision to front end web page |
| **decision_number**    | Decision number within session         |
| **decision_text**      | Decision text                          |
| **decision_title**     | Decision title                         |
| **decision_type_id**   | Decision type id                       |
| **decision_type_name** | Decision type name                     |

## See also

[get_decision_votes](https://martigso.github.io/stortingscrape/reference/get_decision_votes.md)

## Examples

``` r

if (FALSE) { # \dontrun{

desci <- get_session_decisions("2004-2005")
head(desci)
} # }
 
```
