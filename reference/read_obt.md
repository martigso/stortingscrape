# Read Oslo-Bergen-Tagger processed files into R

A function reading OBT-tagged files

## Usage

``` r
read_obt(file = NA)
```

## Arguments

- file:

  character. Path to OBT-tagged file

## Value

A data frame with the following variables:

|              |                                      |
|--------------|--------------------------------------|
|              |                                      |
| **sentence** | Sentence number                      |
| **index**    | Token number in sentence             |
| **token**    | Raw token, as read by OBT originally |
| **lwr**      | Lowercase raw token                  |
| **lemma**    | Lemmatized token                     |
| **pos**      | Part of Speech                       |
| **morph**    | Morphological tags                   |

## Examples

``` r

if (FALSE) { # \dontrun{
sample_text <- read_obt("./inst/extdata/obt_sample.txt")
head(sample_text)
} # }
```
