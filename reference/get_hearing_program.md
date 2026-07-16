# Retrieve the hearing program for a specified hearing

A function for retrieving the hearing program for a specified hearing.
The earlier periods (around 2005 and back) are less rich with data on
some variables

## Usage

``` r
get_hearing_program(hearingid = NA, good_manners = 0)
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

A data.frame with the following variables:

|                              |                                              |
|------------------------------|----------------------------------------------|
|                              |                                              |
| **response_date**            | Date of data retrieval                       |
| **version**                  | Data version from the API                    |
| **hearing_id**               | Id of the hearing                            |
| **hearing_type**             | Type of hearing                              |
| **committee_id**             | Id of committee responsible for the hearing  |
| **hearing_program_date**     | Date hearing program                         |
| **hearing_program_footnote** | Footnote for hearing program                 |
| **order_number**             | Order number for the hearing program element |
| **text**                     | Description of the hearing participant       |
| **time_indication**          | Time stamp for participant hearing input     |
| **date**                     | Date of participant input in hearing         |

## See also

[get_session_hearings](https://martigso.github.io/stortingscrape/reference/get_session_hearings.md)
[get_hearing_input](https://martigso.github.io/stortingscrape/reference/get_hearing_input.md)
[get_written_hearing_input](https://martigso.github.io/stortingscrape/reference/get_written_hearing_input.md)

## Examples

``` r
if (FALSE) { # \dontrun{
s0910 <- get_session_hearings("2009-2010")
hearing <- get_hearing_program(s0910$hearing$hearing_id[1])
head(hearing)
} # }

```
