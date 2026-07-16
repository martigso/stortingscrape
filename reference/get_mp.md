# Extract information on specific MPs

A function for retrieving information on Norwegian MPs from the
parliament API

## Usage

``` r
get_mp(mpid = NA, good_manners = 0)
```

## Arguments

- mpid:

  Character string, or a vector of strings, indicating the id of the MP
  to retrieve.

- good_manners:

  Integer. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

A data.frame with the following variables:

|                   |                                 |
|-------------------|---------------------------------|
|                   |                                 |
| **response_date** | Date of data retrieval          |
| **version**       | Data version from the API       |
| **death**         | MP date of death, if applicable |
| **last_name**     | MP last name                    |
| **birth**         | MP date of birth                |
| **first_name**    | MP first name                   |
| **id**            | MP id                           |
| **gender**        | MP gender                       |

## See also

[get_mp_bio](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md)
[get_parlperiod_mps](https://martigso.github.io/stortingscrape/reference/get_parlperiod_mps.md)
[get_mp_pic](https://martigso.github.io/stortingscrape/reference/get_mp_pic.md)
[get_session_mp_speech_activity](https://martigso.github.io/stortingscrape/reference/get_session_mp_speech_activity.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Request one MP by id
get_mp("AAMH")

# Request several MPs by id
ids <- c("AAMH", "AMSK", "MAAA")

mps <- lapply(ids, get_mp, good_manners = 2)

mps <- do.call(rbind, mps)
} # }
```
