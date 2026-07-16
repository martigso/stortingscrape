# Roll call vote results for vote ids 15404, 15405, and 15406

A dataset containing all personal votes for votes 15404, 15405, and
15406 in *Stortinget*

## Usage

``` r
vote_result
```

## Format

A list with one vote per element

- response_date:

  Date of data retrieval

- version:

  Data version from the API

- vote_id:

  Id of vote

- mp_id:

  MP id

- party_id:

  Party id

- vote:

  Vote: for, mot (against), ikke_tilstede (absent)

- permanent_sub_for:

  Id of the MP originally holding the seat, if the substitute is
  permanent

- sub_for:

  Id of the MP originally holding the seat

## Source

<https://data.stortinget.no/eksport/voteringsresultat?voteringid=15404>,
<https://data.stortinget.no/eksport/voteringsresultat?voteringid=15405>,
<https://data.stortinget.no/eksport/voteringsresultat?voteringid=15406>
