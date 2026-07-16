# Parliamentary committees over all sessions

Imports data on all committee names and ids over all sessions in the
data.stortinget.no API.

## Usage

``` r
get_all_committees()
```

## Value

A data frame of committees, with the variables:

|                   |                           |
|-------------------|---------------------------|
|                   |                           |
| **response_date** | Date of data retrieval    |
| **version**       | Data version from the API |
| **id**            | Id of the committee       |
| **name**          | Name of the committee     |
| **sessionid**     | Id of session (empty)     |

## See also

[get_session_committees](https://martigso.github.io/stortingscrape/reference/get_session_committees.md)

## Examples

``` r
if (FALSE) { # \dontrun{
coms <- get_all_committees()
head(coms)
} # }
```
