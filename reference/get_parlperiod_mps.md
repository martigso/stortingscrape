# Get list of MPs in a given parliamentary period

A function for retrieving Norwegian MPs for a given parliamentary period
from the parliament API

## Usage

``` r
get_parlperiod_mps(periodid = NA, substitute = FALSE, good_manners = 0)
```

## Arguments

- periodid:

  Character string, or a vector of strings, indicating the id of the
  parliamentary period to retrieve.

- substitute:

  Logical. Whether or not to include substitute MPs.

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
| **death**         | Date of death                          |
| **lastname**      | MP last name                           |
| **birth**         | Date of birth                          |
| **firstname**     | MP first name                          |
| **mp_id**         | MP id                                  |
| **gender**        | MP gender                              |
| **county_id**     | Id of county MP represented            |
| **party_id**      | Id of party MP represented             |
| **substitute_mp** | Logical for whether MP is a substitute |
| **period_id**     | Id of period represented in            |

## See also

[get_mp_bio](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md)
[get_mp](https://martigso.github.io/stortingscrape/reference/get_mp.md)
[get_mp_pic](https://martigso.github.io/stortingscrape/reference/get_mp_pic.md)
[get_session_mp_speech_activity](https://martigso.github.io/stortingscrape/reference/get_session_mp_speech_activity.md)

## Examples

``` r

if (FALSE) { # \dontrun{

# Request one MP by id
get_parlperiod_mps("2005-2009")

# Request MPs from several periods by id
ids <- c("1961-65", "1997-01", "2009-2013")
mps <- lapply(ids, get_parlperiod_mps, good_manners = 2)
mps <- do.call(rbind, mps)

} # }
```
