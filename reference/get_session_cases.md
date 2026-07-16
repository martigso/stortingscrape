# Cases in specified session

A function for retrieving all cases treated in a specified parliamentary
session.

## Usage

``` r
get_session_cases(sessionid = NA, good_manners = 0, cores = 1)
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

  Integer. Number of cores (1 by default) to use in structuring the
  data. More than 1 will not work on windows

## Value

A data.frame with the following variables:

1.  **\$root** (main data on the MP)

    |                        |                                    |
    |------------------------|------------------------------------|
    |                        |                                    |
    | **response_date**      | Date of data retrieval             |
    | **version**            | Data version from the API          |
    | **treated_session_id** | Session the case was treated in    |
    | **document_group**     | Document group the case belongs to |
    | **reference**          | Document reference                 |
    | **id**                 | Case id                            |
    | **com_req_id**         | Committee recommendation id        |
    | **com_req_code**       | Committee recommendation code      |
    | **title_short**        | Short title of case                |
    | **case_filed_id**      | Id of filed case                   |
    | **last_update_date**   | Date of last update on case        |
    | **status**             | Status of the case                 |
    | **title**              | Full title of the case             |
    | **type**               | Type of case                       |
    | **session_id**         | Session id of the case             |
    | **committee_id**       | Responsible committee id           |

2.  **\$topics** (named list by case id)

    |                   |                                                        |
    |-------------------|--------------------------------------------------------|
    |                   |                                                        |
    | **is_main_topic** | Logical indication whether the topic is the main topic |
    | **main_topic_id** | Id of the main topic for the case                      |
    | **id**            | Topic id                                               |
    | **name**          | Topic name                                             |

3.  **\$proposers** (named list by case id)

    |               |                                                  |
    |---------------|--------------------------------------------------|
    |               |                                                  |
    | **rep_id**    | Proposing MP id                                  |
    | **county_id** | County id of proposing MP                        |
    | **party_id**  | Party id of proposing MP                         |
    | **rep_sub**   | Logical indicator for whether MP is a substitute |

4.  **\$spokespersons** (data frame by case id)

    |               |                                                  |
    |---------------|--------------------------------------------------|
    |               |                                                  |
    | **case_id**   | Case id                                          |
    | **rep_id**    | Spokesperson(s) MP id for the case               |
    | **county_id** | County id of spokesperson MP                     |
    | **party_id**  | Party id of spokesperson MP                      |
    | **rep_sub**   | Logical indicator for whether MP is a substitute |

## See also

[get_case](https://martigso.github.io/stortingscrape/reference/get_case.md)
[get_vote](https://martigso.github.io/stortingscrape/reference/get_vote.md)

## Examples

``` r

if (FALSE) { # \dontrun{
s0506 <- get_session_cases("2005-2006")
head(s0506)
} # }
```
