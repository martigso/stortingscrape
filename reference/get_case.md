# Retreive a parliamentary case

A function for retrieving single parliamentary case by id.

## Usage

``` r
get_case(caseid = NA, good_manners = 0)
```

## Arguments

- caseid:

  Character string, or a vector of strings, indicating the id of the
  case to request

- good_manners:

  Integer. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

A list with seven data frame elements:

1.  **\$root** (main data on the case)

    |                      |                                   |
    |----------------------|-----------------------------------|
    |                      |                                   |
    | **response_date**    | Date of data retrieval            |
    | **version**          | Data version from the API         |
    | **document_group**   | Case document group type          |
    | **finalized**        | Whether the case finalized        |
    | **reference**        | Relevant publication references   |
    | **id**               | Case id                           |
    | **req_text**         | Recommendation (proposal) text    |
    | **committee_id**     | Id of committee handling the case |
    | **title_short**      | Case short title                  |
    | **decision_short**   | Case decision_short               |
    | **parenthesis_text** | Case parenthesis text             |
    | **case_number**      | Case number                       |
    | **session_id**       | Session id                        |
    | **proceedings_id**   | Type of proceeding id             |
    | **proceedings_name** | Type of proceeding name           |
    | **status**           | Status for case                   |
    | **title**            | Case title (long)                 |
    | **type**             | Case type                         |
    | **decision_text**    | Decision text                     |

2.  **\$topic** (the topics related to the case)

    |                   |                               |
    |-------------------|-------------------------------|
    |                   |                               |
    | **is_main_topic** | Is this (row) the main topic? |
    | **main_topic_id** | Id for main topic             |
    | **id**            | Topic id                      |
    | **navn**          | Topic name                    |

3.  **\$publication_references** (references for publications on the
    case)

    |               |                                                         |
    |---------------|---------------------------------------------------------|
    |               |                                                         |
    | **export_id** | Id for export of publication (used in ?get_publication) |
    | **link_text** | Publication title                                       |
    | **link_url**  | URL to publication                                      |
    | **type**      | Publication type                                        |
    | **subtype**   | Publication subtype (chamber)                           |

4.  **\$proposers** (MPs behind case proposal, when relevant)

    |              |                             |
    |--------------|-----------------------------|
    |              |                             |
    | **mp_id**    | MPs id                      |
    | **party_id** | Party id of MPs             |
    | **sub_mp**   | Whether MPs are substitutes |

5.  **\$proceeding_steps** (case proceeding steps)

    |                 |                                   |
    |-----------------|-----------------------------------|
    |                 |                                   |
    | **step_name**   | Name of steps                     |
    | **step_number** | Step order for case               |
    | **outdated**    | Whether the step type is outdated |

6.  **\$spokespersons** (all MPs that are spokespersons for the case)

    |              |                             |
    |--------------|-----------------------------|
    |              |                             |
    | **mp_id**    | MPs id                      |
    | **party_id** | Party id of MPs             |
    | **sub_mp**   | Whether MPs are substitutes |

7.  **\$keywords** (all keywords associated with the case)

    |             |                       |
    |-------------|-----------------------|
    |             |                       |
    | **keyword** | Keywords for the case |

## See also

[get_session_cases](https://martigso.github.io/stortingscrape/reference/get_session_cases.md)

## Examples

``` r

if (FALSE) { # \dontrun{

# Get one case
case <- get_case("30233")
case

# Get multiple cases
cases <- lapply(c("30233", "30362", "30234", "30236"), get_case, good_manners = 2)
cases_root <- lapply(cases, function(x) x$root)
cases_root <- do.call(rbind, cases_root)
cases_root

cases_keywords <- lapply(1:nrow(cases_root), function(x){
  tmp <- cases[[x]]$keywords
  tmp$case_id <- cases_root$id[x]
  return(tmp)
})
cases_keywords <- do.call(rbind, cases_keywords)
cases_keywords

} # }
```
