# All parliamentary proceedings

A function for retrieving all proceedings in Stortinget, both current
and historical.

## Usage

``` r
get_proceedings()
```

## Value

A list with three dataframes:

1.  **\$root** (only download meta data)

    |                   |                           |
    |-------------------|---------------------------|
    |                   |                           |
    | **response_date** | Date of data retrieval    |
    | **version**       | Data version from the API |

2.  \$proceedings (description of main proceeding categories)

    |          |                    |
    |----------|--------------------|
    |          |                    |
    | **id**   | Id of proceeding   |
    | **name** | Name of proceeding |

3.  \$poceedings_steps (description of proceeding steps within each main
    category)

    |                 |                                            |
    |-----------------|--------------------------------------------|
    |                 |                                            |
    | **id**          | Id of proceeding step                      |
    | **name**        | Name of proceeding step                    |
    | **step_number** | Order of proceeding steps                  |
    | **outdated**    | Whether the step is outdated               |
    | **main_id**     | Id for proceeding type the step belongs to |

## Examples

``` r
if (FALSE) { # \dontrun{

get_proceedings()

} # }
```
