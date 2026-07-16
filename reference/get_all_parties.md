# All parliamentary parties

A function for retrieving all Norwegian parliamentary parties in the
data.stortinget.no API.

## Usage

``` r
get_all_parties()
```

## Value

A data.frame of all parties, with the variables:

|  |  |
|----|----|
|  |  |
| **response_date** | Date of data retrieval |
| **version** | Data version from the API |
| **id** | Id of the party |
| **name** | Name of the party |
| **represented_party** | Whether the party is represented at the time of download |
| **sessionid** | Id of session (empty) |
| **period_id** | Id of parliamentary period (empty) |

## See also

[get_session_parties](https://martigso.github.io/stortingscrape/reference/get_session_parties.md)

## Examples

``` r
if (FALSE) { # \dontrun{
parties <- get_all_parties()
head(parties)
} # }
```
