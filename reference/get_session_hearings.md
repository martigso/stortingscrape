# Retrieve hearings in specified session

A function for retrieving all hearings in a specified parliamentary
session.

## Usage

``` r
get_session_hearings(sessionid = NA, good_manners = 0, cores = 1)
```

## Arguments

- sessionid:

  Character string indicating the id of the parliamentary session to
  retrieve.

- good_manners:

  Integer. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

- cores:

  Integer...

## Value

A list with four elements:

1.  **\$root** (hearing meta data)

    |                   |                           |
    |-------------------|---------------------------|
    |                   |                           |
    | **response_date** | Date of data retrieval    |
    | **version**       | Data version from the API |
    | **session_id**    | Session id                |

2.  **\$hearing** (main data on the hearing)

    |  |  |
    |----|----|
    |  |  |
    | **deadline_date** | Deadline date for hearing |
    | **status** | Data version from the API |
    | **hearing_id** | Hearing id |
    | **input_deadline** | Deadline date for input |
    | **written** | Logical indication of whether the input was written |
    | **application_deadline** | Deadline date for application to hearing |
    | **start_date** | Start date for hearing |
    | **status_pub** | Publication status for hearing |
    | **status_info_text** | Status information text |
    | **type** | Type of hearing |
    | **committee_id** | Committee id for committee responsible for hearing |

3.  **\$hearing_case_info** (named list by hearing id with information
    on the case(s) belonging to the hearing)

    |                      |                                        |
    |----------------------|----------------------------------------|
    |                      |                                        |
    | **hearing_id**       | Hearing id                             |
    | **case_reference**   | Text reference for case                |
    | **case_id**          | Case id                                |
    | **case_short_title** | Short title for case                   |
    | **case_publication** | URL for front end web-page publication |
    | **case_title**       | Full title for case                    |

4.  **\$hearing_date** (named list by hearing id with date(s) the
    hearing was held)

    |                |                            |
    |----------------|----------------------------|
    |                |                            |
    | **hearing_id** | Hearing id                 |
    | **date**       | Date of hearing            |
    | **place**      | Where the hearing was held |

## See also

[get_hearing_input](https://martigso.github.io/stortingscrape/reference/get_hearing_input.md)
[get_hearing_program](https://martigso.github.io/stortingscrape/reference/get_hearing_program.md)
[get_written_hearing_input](https://martigso.github.io/stortingscrape/reference/get_written_hearing_input.md)

## Examples

``` r

if (FALSE) { # \dontrun{

hear <- get_session_hearings("2010-2011")
head(hear$hearing)

} # }

```
