# Parliamentary delegations in specified session

A function for retrieving delegations for a specified parliamentary
session.

## Usage

``` r
get_session_delegations(sessionid = NA, good_manners = 0)
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

|                   |                           |
|-------------------|---------------------------|
|                   |                           |
| **response_date** | Date of data retrieval    |
| **version**       | Data version from the API |
| **id**            | Delegation id             |
| **name**          | Name of delegation        |
| **session_id**    | Session id                |

## See also

[get_session_committees](https://martigso.github.io/stortingscrape/reference/get_session_committees.md)
[get_all_committees](https://martigso.github.io/stortingscrape/reference/get_all_committees.md)

## Examples

``` r

if (FALSE) { # \dontrun{

deleg <- get_session_delegations("2015-2016")
deleg

} # }
```
