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

  Character string indicating the id of the MP to retrieve.

- size:

  Character string size of the picture. Accepts values "lite" (small),
  "middels" (medium – default), and "stort" (big).

- destfile:

  Character string specifying where to save the picture

- show_plot:

  Logical. FALSE (default) if no plot should be produced and TRUE if
  plot should be produced. Requires the "imager" package.

- good_manners:

  Integer. Seconds delay between calls when making multiple calls to the
  same function. Note that the Stortinget API is limited to 100 calls
  per minute (see
  <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>).

## Value

Picture of the requested MP in the preferred size.

## See also

[get_mp](https://martigso.github.io/stortingscrape/reference/get_mp.md)
[get_parlperiod_mps](https://martigso.github.io/stortingscrape/reference/get_parlperiod_mps.md)
[get_mp_bio](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Request one MP by id
get_mp_pic(mpid = "AAMH", destfile = "~/Pictures/AAMH.jpeg", show_plot = TRUE, size = "stort")

# With good manners for multiple calls
lapply(c("AAMH", "CIH", "TKF"), function(x){
  get_mp_pic(mpid = x, destfile = paste0("~/Pictures/", x), 
  show_plot = TRUE, size = "stort", good_manners = 2)
  })
} # }
```
