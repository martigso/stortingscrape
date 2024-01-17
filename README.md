
# stortingscrape <img src="man/figures/stortingscrape.png" align="right" width="120"/>

<!-- badges: start -->

[![CRAN
Version](http://www.r-pkg.org/badges/version/stortingscrape)](https://cran.r-project.org/package=stortingscrape)
[![Github
Version](https://img.shields.io/github/r-package/v/martigso/stortingscrape?color=yellowgreen)](https://github.com/martigso/stortingscrape)
[![Downloads](http://cranlogs.r-pkg.org/badges/stortingscrape)](https://cran.r-project.org/package=stortingscrape)
[![Total
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/stortingscrape?color=orange)](https://cran.r-project.org/package=stortingscrape)
[![R-CMD-check](https://github.com/martigso/stortingscrape/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/martigso/stortingscrape/actions/workflows/check-standard.yaml)
<!-- badges: end -->

`stortingscrape` is an R package for retrieving data from the Norwegian
parliament (*Stortinget*) through their easily accessible [back-end
API](https://data.stortinget.no). The data requested using the package
require little to no further structuring. The scope of the package
ranges from general data on the parliament itself (rules, session info,
committees, etc) to data on the parties, bibliographies of the MPs,
questions, hearings, debates, votes, and more.

The main goal of `stortingscrape` is to allow researchers to access any
data from the Norwegian parliament easily, but also still be able to
structure the data according to ones need. Most importantly, the package
is facilitated for weaving together different parts of the
data.stortinget.no API.

## Installation

Because I’m not in control of the API itself, the CRAN package might at
times be outdated. Submitting to CRAN is quite time consuming and will
not be done more than once or twice a year.

### CRAN (stable version)

The latest stable version of the `stortingscrape` package can be
installed from by running CRAN:

``` r
install.packages("stortingscrape")
```

### Github (development version)

The development version of the package can be installed either by
cloning this repository and building the package in R or by installing
via the `devtools::install_github()` function:

``` r
devtools::install_github("martigso/stortingscrape")
library(stortingscrape)
```

## Usage examples

Request all interpellations for a parliamentary session:

``` r
library(stortingscrape)

parl_sessions |> # sessions data are built into the package
  head()         # but can also be retrieved with `get_parlsessions()`


qsesh <- get_session_questions(parl_sessions$id[4], q_type = "interpellasjoner")

int1213 <- list()

for(i in qsesh$id) {
  
  message("Getting ", i)
  
  int1213[[i]] <- get_question(i, good_manners = 2)

}

int1213 <- do.call(rbind, int1213)

head(int1213)
```

Get biographies of all MPs for a given parliamentary period (will take
~30min to run):

``` r
parl_periods # parliamentary periods (4 years) are built into the package,
             # but can also be retrieved with `get_parlperiods()`

mps <- get_parlperiod_mps(parl_periods$id[1], substitute = TRUE)

mps_bios <- lapply(mps$mp_id, get_mp_bio, good_manners = 2)

# Expand by all periods the MP has been in parliament
mps_periods <- lapply(mps_bios, function(x){
  
  data.frame(x$root,
             x$parl_periods)

})

mps_periods <- do.call(rbind, mps_periods)

# Expand by all positions held in parliament
mps_positions <- lapply(mps_bios, function(x){
  
  if(nrow(x$parl_positions) < 1) return()
  
  data.frame(x$root,
             x$parl_positions)
  
})

mps_positions <- do.call(rbind, mps_positions)
```

## Data description

The back-end data is described in detail in the [API of
Stortinget](https://data.stortinget.no/dokumentasjon-og-hjelp/).

## Functions

[You can find a list of all functions
here.](https://martigso.github.io/stortingscrape/functions.html)
