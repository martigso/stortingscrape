# Color palette for parties in the Storting

A color palette for all (current) parties in the Storting

## Usage

``` r
st_party_colors
```

## Format

A vector of party abbreviations and official hex colors

- Arbeiderpartiet (Labour Party):

  <https://www.arbeiderpartiet.no/om/presse/profil/>

- Fremskrittspartiet (Progress Party):

  <https://www.frp.no/files/Grafiske-retningslinjer/FrP-Profilmanual-2023.pdf>

- Høyre (Conservative Party):

  <https://hoyre.no/design/farger/>

- Kristelig Folkeparti (Christian Democratic Party):

  <https://krf.no/ressursbank/logoarkiv/>

- Miljøpartiet De Grønne (Green Party):

  <https://mdg.no/partiet/organisasjon#logo>

- Pasientfokus (Patient Focus):

  <https://no.wikipedia.org/wiki/Mal:Farge/Pasientfokus>

- Rødt (Red Party):

  <https://roedt.no/grafisk-materiell>

- Senterpartiet (Centre Party):

  <https://profil.senterpartiet.no/point/no/senterpartietbc/component/default/24406>

- Sosialistisk Venstreparti (Socialist Left Party):

  <https://www.sv.no/ressursbanken/grafisk/grafisk-profil/>

- Venstre (Liberal Party):

  <https://www.venstre.no/organisasjon/visuell-identitet/>

## Source

See list of links above; there are several color alternatives for most
parties.

## Examples

``` r
if (FALSE) { # \dontrun{

barplot(table(get_parlperiod_mps(parl_periods$id[1])$party_id), col = st_party_colors)

} # }
```
