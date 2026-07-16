# Retrieve all speech activity from one MP for a given session

A function for retrieving all speech activity from an MP during a
specific parliamentary session. Only available from the 2011-2012
session and onwards.

## Usage

``` r
get_session_mp_speech_activity(sessionid = NA, mp_id = NA, good_manners = 0)
```

## Arguments

- sessionid:

  Character string indicating the session to retrieve speeches from.

- mp_id:

  Character string for the MP to retreive all speeches of in a given
  session.

- good_manners:

  Integer. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

A data.frame with the following variables:

|                        |                                                  |
|------------------------|--------------------------------------------------|
|                        |                                                  |
| **response_date**      | Date of data retrieval                           |
| **version**            | Data version from the API                        |
| **session_id**         | Session id                                       |
| **agenda_case_number** | Number indicating the agenda number for the case |
| **meeting_id**         | Meeting id                                       |
| **speech_start_time**  | Start time of speech                             |
| **speech_type**        | Type of speech                                   |
| **speech_length_secs** | Lenght of speech in seconds                      |

## See also

[get_mp](https://martigso.github.io/stortingscrape/reference/get_mp.md)
[get_mp_bio](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md)
[get_publication](https://martigso.github.io/stortingscrape/reference/get_publication.md)

## Examples

``` r

if (FALSE) { # \dontrun{

activ <- get_session_mp_speech_activity("2012-2013", "ALYS")
head(activ)
} # }

```
