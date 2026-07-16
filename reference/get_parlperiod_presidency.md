# Get list of presidency in a given parliamentary period

A function for retrieving the presidency for a given parliamentary
period from the parliament API.

## Usage

``` r
get_parlperiod_presidency(periodid = NA, good_manners = 0)
```

## Arguments

- periodid:

  Character string, or a vector of strings, indicating the id of the
  parliamentary period to retrieve.

- good_manners:

  Integer. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

A data.frame with the following variables:

|                   |                                        |
|-------------------|----------------------------------------|
|                   |                                        |
| **response_date** | Date of data retrieval                 |
| **version**       | Data version from the API              |
| **last_name**     | Last name of presidency member         |
| **first_name**    | First name of presidency member        |
| **from_date**     | Presidency member from date            |
| **party_id**      | Party affiliation of presidency member |
| **person_id**     | Id of the presidency member            |
| **to_date**       | Presidency member to date              |
| **position**      | Presidency position                    |

## See also

[get_mp](https://martigso.github.io/stortingscrape/reference/get_mp.md)
[get_mp_bio](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md)

## Examples

``` r
if (FALSE) { # \dontrun{
 
# Request one MP by id
get_parlperiod_presidency("2005-2009")

} # }
```
