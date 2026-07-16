# Retrieve publications of a type in a parliamentary session

A function for retrieving one of several publication types within a
parliamentary session.

## Usage

``` r
get_session_publications(sessionid = NA, type = "referat", good_manners = 0)
```

## Arguments

- sessionid:

  Character string, or a vector of strings, indicating the id of the
  session to retrieve publications from.

- type:

  Character specifying type of publication to download. Available types
  are "referat" (minutes), "innstilling" (proposition), "innberetning"
  (report), "lovvedtak" (law decision), "lovanmerkning" (law note),
  "dok8" (MP proposal) "dok12" (Constitutional proposal), and
  "dokumentserie" (document series). Defaults to "referat".

- good_manners:

  Integer. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

A data.frame with the following variables:

|                                |                                         |
|--------------------------------|-----------------------------------------|
|                                |                                         |
| **response_date**              | Date of data retrieval                  |
| **version**                    | Data version from the API               |
| **session_id**                 | Session id                              |
| **publication_date**           | Date of publication                     |
| **publication_id**             | Id of publication                       |
| **publication_format**         | Publication format (XML)                |
| **publication_available_date** | When the publication was made available |
| **publication_title**          | Publication title                       |
| **publication_type**           | Publication type                        |

## See also

[get_publication](https://martigso.github.io/stortingscrape/reference/get_publication.md)

## Examples

``` r

if (FALSE) { # \dontrun{

pub <- get_session_publications("1998-99")
head(pub)

} # }

```
