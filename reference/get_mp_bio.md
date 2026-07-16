# Extract biography of specific MPs

A function for retrieving biography of Norwegian MPs from the parliament
API

## Usage

``` r
get_mp_bio(mpid = NA, good_manners = 0)
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

A list with ten data frames:

1.  **\$root** (main data on the MP)

    |                   |                           |
    |-------------------|---------------------------|
    |                   |                           |
    | **response_date** | Date of data retrieval    |
    | **version**       | Data version from the API |
    | **id**            | Id of the MP              |

2.  **\$literature** (all literature the MP contributed to)

    |                 |                                       |
    |-----------------|---------------------------------------|
    |                 |                                       |
    | **year**        | Year of entry publication             |
    | **description** | Description of the publication        |
    | **last_name**   | MP's last name                        |
    | **more_years**  | **Not described in the API**          |
    | **publisher**   | Publisher                             |
    | **first_name**  | First name of the MP                  |
    | **place**       | Place of publication                  |
    | **title**       | Title of the publication              |
    | **type**        | MP's role in publication (author etc) |

3.  **\$leave_of_absence** (times the MP was on leave)

    |                    |                                           |
    |--------------------|-------------------------------------------|
    |                    |                                           |
    | **from_date**      | Start date of leave                       |
    | **reason**         | Reason for leave                          |
    | **to_date**        | End of leave                              |
    | **type**           | Type of leave                             |
    | **sub_last_name**  | Substitute MP last name (id not recorded) |
    | **sub_first_name** | Substitute MP first name                  |

4.  **\$personalia** (the MP's personalia)

    |                           |                                          |
    |---------------------------|------------------------------------------|
    |                           |                                          |
    | **seniority_aar**         | Number of years in parliament            |
    | **seniority_dager**       | Number of extra days (addition to years) |
    | **county_of_birth**       | Birth county of the MP                   |
    | **municipality_of_birth** | Birth municipality of the MP             |
    | **eulogy_date**           | Eulogy date of the MP, when applicable   |

5.  **\$father** (the MP's father personalia)

    |                |                        |
    |----------------|------------------------|
    |                |                        |
    | **death_year** | Father's year of death |
    | **last_name**  | Father's last name     |
    | **birth_year** | Father's year of birth |
    | **first_name** | Father's first name    |
    | **profession** | Father's profession    |

6.  **\$mother** (the MP's mother personalia)

    |                |                        |
    |----------------|------------------------|
    |                |                        |
    | **death_year** | Mother's year of death |
    | **last_name**  | Mother's last name     |
    | **birth_year** | Mother's year of birth |
    | **first_name** | Mother's first name    |
    | **profession** | Mother's profession    |

7.  **\$parl_periods** (parliamentary periods the MP has held a seat)

    |                    |                                                    |
    |--------------------|----------------------------------------------------|
    |                    |                                                    |
    | **from_date**      | Date MP held seat from                             |
    | **county**         | County the MP represented                          |
    | **party_id**       | Party id for the MP's party                        |
    | **rep_number**     | Representative number (within the whol parliament) |
    | **parl_period_id** | Id of the parliamentary period                     |
    | **to_date**        | Date MP held a seat to                             |
    | **type**           | Type of representation                             |

8.  **\$parl_positions** (parliamentary positions held by the MP)

    |  |  |
    |----|----|
    |  |  |
    | **from_year** | Year MP held position from |
    | **from_date** | Date MP held position from |
    | **committee_id** | Id of the position (in committee, cabinet, delegation, etc) |
    | **committee_name** | Position name |
    | **committee_type** | Position type |
    | **sorting** | **Not described in the API** |
    | **parl_period_id** | Parliamentary period the position was held (cabinet data missing) |
    | **to_year** | Year MP held position to |
    | **to_date** | Date MP held position to |

9.  **\$vocation** (vocation and education of the MP outside of
    parliament)

    |  |  |
    |----|----|
    |  |  |
    | **several_periods_text** | Text description if the vocation was held for several periods |
    | **from_year** | Year MP held vocation from |
    | **from_year_unknown** | Logical indication for whether the start year is unknown |
    | **note** | Note for vocation |
    | **name** | Name of vocation |
    | **to_year** | Year MP held vocation to |
    | **to_year_unknown** | Logical indication for whether the end year is unknown |
    | **type** | Vocation type (10 = education, 20 = work) |

10. **\$other_positions** (other positions held outside parliament)

    |  |  |
    |----|----|
    |  |  |
    | **several_periods_text** | Text description if the vocation was held for several periods (removed from API) |
    | **from_year** | Year MP held vocation from |
    | **from_year_sorting** | **Not described in API** (removed from API) |
    | **from_year_unknown** | Logical indication for whether the start year is unknown |
    | **max_to_year** | The last possible time the MP held the position (removed from API) |
    | **note** | Note for position |
    | **min_to_year** | The earliest possible time the MP held the position (removed from API) |
    | **level** | **Not described in API** |
    | **organization** | Organization holding the position |
    | **place** | Place of the position |
    | **to_year** | Year MP held position to |
    | **to_year_unknown** | Logical indication for whether the end year is unknown |
    | **type** | Position type |
    | **position** | Position name/description |

## See also

[get_mp](https://martigso.github.io/stortingscrape/reference/get_mp.md)
[get_parlperiod_mps](https://martigso.github.io/stortingscrape/reference/get_parlperiod_mps.md)
[get_mp_pic](https://martigso.github.io/stortingscrape/reference/get_mp_pic.md)
[get_session_mp_speech_activity](https://martigso.github.io/stortingscrape/reference/get_session_mp_speech_activity.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# Request one MP by id
get_mp_bio("AAMH")

} # }
```
