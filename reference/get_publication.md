# Retrieve a specific publication

A function for retrieving a specific publication. Because these are
formatted very differently in the API, the returning object is the raw
XML document, best manipulated with node extraction functions such as
[`rvest::html_elements()`](https://rvest.tidyverse.org/reference/html_element.html)
or
[`xml2::xml_find_all()`](http://xml2.r-lib.org/reference/xml_find_all.md).

## Usage

``` r
get_publication(publicationid = NA, good_manners = 0)
```

## Arguments

- publicationid:

  Character string, or a vector of strings, indicating the id of the
  publication to retrieve

- good_manners:

  Numeric. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

A raw xml_document

## Details

Note that element and attribute names are case sensitive. Publications
from before the 2016-2017 session use lowercase names (e.g. `innlegg`,
`sakid`), while later publications use capitalized names (e.g.
`Replikk`, `personID`). For debate transcripts,
[get_speeches](https://martigso.github.io/stortingscrape/reference/get_speeches.md)
returns the speeches as a data frame.

## See also

[get_speeches](https://martigso.github.io/stortingscrape/reference/get_speeches.md)
[get_question](https://martigso.github.io/stortingscrape/reference/get_question.md)
[get_question_hour](https://martigso.github.io/stortingscrape/reference/get_question_hour.md)
[get_session_publications](https://martigso.github.io/stortingscrape/reference/get_session_publications.md)

## Examples

``` r

if (FALSE) { # \dontrun{
pub <- get_publication("refs-201819-03-06")
(pub |> html_elements("Replikk"))[1] |> html_text()
} # }
 
```
