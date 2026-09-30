# Storting cases in the 2019-2020 session

A dataset containing all cases of the 2019-2020 parliamentary session in
*Stortinget*

## Usage

``` r
cases
```

## Format

A list with four elements

- \$root:

  main data on the cases

- \$topics:

  named list by case id

- \$proposers:

  named list by case id

- \$spokespersons:

  named list by case id

- Further description::

  [get_session_cases](https://martigso.github.io/stortingscrape/reference/get_session_cases.md)

The dataset was retrieved with an earlier version of the package;
[get_session_cases](https://martigso.github.io/stortingscrape/reference/get_session_cases.md)
now returns `$spokespersons` as a data frame.

## Source

<https://data.stortinget.no/eksport/saker?sesjonid=2019-2020>
