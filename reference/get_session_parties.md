# Parliamentary parties in specified session

A function for retrieving Norwegian parliamentary parties for a
specified parliamentary session

## Usage

``` r
get_session_parties(sessionid = NA, good_manners = 0)
```

## Arguments

- sessionid:

  Character string, or a vector of strings, indicating the id of the
  parliamentary session to retrieve.

- good_manners:

  Integer. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

A data.frame with the following variables:

|                       |                                                     |
|-----------------------|-----------------------------------------------------|
|                       |                                                     |
| **response_date**     | Date of data retrieval                              |
| **version**           | Data version from the API                           |
| **id**                | Party id                                            |
| **name**              | Party name                                          |
| **represented_party** | Logical indication of whether party was represented |
| **session_id**        | Session id                                          |
| **period_id**         | Parliamentary period id                             |

## See also

[get_all_parties](https://martigso.github.io/stortingscrape/reference/get_all_parties.md)

## Examples

``` r

if (FALSE) { # \dontrun{

parties <- get_session_parties("2003-2004")
parties

} # }
 
```
