# Retrieve the hearing input for a specified hearing

A function for retrieving the hearing input for a specified hearing.

## Usage

``` r
get_hearing_input(hearingid = NA, good_manners = 0)
```

## Arguments

- hearingid:

  Character string, or a vector of strings, indicating the id of the
  hearing to retrieve.

- good_manners:

  Integer. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

A data.frame the following variables:

|  |  |
|----|----|
|  |  |
| **response_date** | Date of data retrieval |
| **version** | Data version from the API |
| **hearing_id** | Id of the hearing |
| **hearing_type** | Type of hearing |
| **committee_id** | Id of committee responsible for the hearing |
| **hearing_input_date** | Date of receiving input |
| **hearing_input_id** | Hearing input id |
| **hearing_input_organization** | Organization giving input |
| **hearing_input_text** | Full text of the hearing input |
| **hearing_input_title** | Title of the hearing input |

## See also

[get_session_hearings](https://martigso.github.io/stortingscrape/reference/get_session_hearings.md)
[get_hearing_program](https://martigso.github.io/stortingscrape/reference/get_hearing_program.md)
[get_written_hearing_input](https://martigso.github.io/stortingscrape/reference/get_written_hearing_input.md)

## Examples

``` r

if (FALSE) { # \dontrun{
get_hearing_input(hearingid = 10004166)
} # }
```
