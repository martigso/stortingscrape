# Get Parliamentary Sessions

A function for retrieving dates of the parliamentary sessions after WWII

## Usage

``` r
get_parlsessions()
```

## Value

A data.frame with the following variables:

|                   |                                              |
|-------------------|----------------------------------------------|
|                   |                                              |
| **response_date** | Date of data retrieval                       |
| **version**       | Data version from the API                    |
| **from**          | Date session started                         |
| **id**            | Id of for session (used for other functions) |
| **to**            | Date session ended                           |
| **years**         | From year to year in full format             |

## See also

[get_parlperiods](https://martigso.github.io/stortingscrape/reference/get_parlperiods.md)

## Examples

``` r
if (FALSE) { # \dontrun{

parlses <- get_parlsessions()
parlses

} # }
```
