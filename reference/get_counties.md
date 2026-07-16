# Get list of electoral districts

A function for retrieving current and/or historical electoral districts
(counties) for the Norwegian parliament.

## Usage

``` r
get_counties(historical = FALSE)
```

## Arguments

- historical:

  Logical. Whether or not to include historical counties.

## Value

A data frame with the following variables:

|                       |                                                     |
|-----------------------|-----------------------------------------------------|
|                       |                                                     |
| **response_date**     | Date of data retrieval                              |
| **version**           | Data version from the API                           |
| **historical_county** | Whether the county is historical (no longer exists) |
| **id**                | Id of the county                                    |
| **name**              | Name of the county                                  |

## Examples

``` r
if (FALSE) { # \dontrun{ 
# Request one MP by id
get_counties()

# With historical counties
get_counties(historical = TRUE)
} # }
```
