# Retrieve a specific publication

A function for retrieving a specific publication. Because these are
formatted very differently in the API, the returning object is in a raw
html_document format, best manipulated with html node extraction
functions such as
[`rvest::html_elements()`](https://rvest.tidyverse.org/reference/html_element.html)
.

## Usage

``` r
get_publication(publicationid = NA, good_manners = 0)
```

## Arguments

- publicationid:

  Character string, or a vector of strings, indicating the id of the
  publication to retrieve

- good_manners:

  Integer. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

A raw html_document

## See also

[get_question](https://martigso.github.io/stortingscrape/reference/get_question.md)
[get_question_hour](https://martigso.github.io/stortingscrape/reference/get_question_hour.md)
[get_session_publications](https://martigso.github.io/stortingscrape/reference/get_session_publications.md)

## Examples

``` r

if (FALSE) { # \dontrun{
pub <- get_publication("refs-201819-03-06")
(pub |> html_elements("replikk"))[1] |> html_text()
} # }
 
```
