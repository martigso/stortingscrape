# Retrieve picture of specific MPs

A function for retrieving Norwegian MP pictures by id.

## Usage

``` r
get_mp_pic(mpid = NA, size = "middels", 
           destfile = NA, show_plot = FALSE, 
           good_manners = 0)
```

## Arguments

- mpid:

  Character string, or a vector of strings, indicating the id of the MP
  to retrieve.

- size:

  Character string size of the picture. Accepts values "lite" (small),
  "middels" (medium – default), and "stort" (big).

- destfile:

  Character string specifying where to save the picture. With several
  ids, one destfile per id.

- show_plot:

  Logical. FALSE (default) if no plot should be produced and TRUE if
  plot should be produced. Requires the "magick" package.

- good_manners:

  Numeric. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

No return value; called for its side effects (saves the picture to
`destfile` and/or plots it).

## See also

[get_mp](https://martigso.github.io/stortingscrape/reference/get_mp.md)
[get_parlperiod_mps](https://martigso.github.io/stortingscrape/reference/get_parlperiod_mps.md)
[get_mp_bio](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Request one MP by id
get_mp_pic(mpid = "AAMH", destfile = "~/Pictures/AAMH.jpeg", show_plot = TRUE, size = "stort")

# Several MPs, one file each, with good manners
ids <- c("AAMH", "CIH", "TKF")
get_mp_pic(mpid = ids, destfile = paste0("~/Pictures/", ids, ".jpeg"),
           size = "stort", good_manners = 2)
} # }
```
