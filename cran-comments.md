## R CMD check results

0 errors | 0 warnings | 1 note

* This is an update (0.4.1 to 0.5.0). See NEWS.md for the changes.
* The maintainer's email address has changed from martin.soyland@stv.uio.no to
  martin.soyland@uis.no (new employer). I will confirm the change from the
  previous address.

The NOTE is from the URL check: https://roedt.no/grafisk-materiell (in the
documentation of `st_party_colors`) returns HTTP 429 (Too Many Requests) to
automated requests. The URL is valid and opens in a browser.

## Test environments

* Local: Arch Linux, R 4.6.1
* GitHub Actions: macOS (release), Windows (release), Ubuntu (release and devel)

## Reverse dependencies

There are no reverse dependencies.

## non-ASCII

Due to Norwegian letters (in the package data, e.g. the names of MPs) being
a core part of the package, I have not fixed the following NOTE, which may
appear on some check flavours:

```
checking data for non-ASCII characters ... NOTE
  Note: found ... marked UTF-8 strings
```
