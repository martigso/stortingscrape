# stortingscrape

Under construction!

## Usage example

```
devtools::install_github("martigso/stortingscrape")

library(stortingscrape)

sessions <- get_parlsessions()
qsesh <- get_session_questions(sessions$id[9], q_type = "interpellasjoner")

library(pbmcapply) # for progress bar. never use paralell on scraping

int1213 <- pbmclapply(qsesh$id, function(x){
  
  get_question(x, good_manners = 2)
  
}, mc.cores = 1)

test <- do.call(rbind, int1213)
```

## To do

- ~~Stortingsperioder~~
- ~~Sesjoner~~
- ~~Fylker~~
- ~~Emner~~
- ~~Partier~~
- ~~Alle partier~~
- ~~Komiteer~~
- ~~Alle komiteer~~
- ~~Delegasjoner~~
- ~~Person~~
- ~~Personbilde~~
- ~~Kodet personbiografi~~
- ~~Representanter~~
- Dagens representanter (not relevant?)
- ~~Presidentskapet~~
- Regjering
- ~~Spørsmål~~
- ~~Enkeltspørsmål~~
- ~~Saksganger~~
- Saker
- Ventede saker
- Sak
- Voteringer
- Voteringsforslag
- Voteringsvedtak
- Voteringsresultat
- Vedtak
- Møter
- Dagsorden
- Spørretimen
- Høringer
- Skriftlige innspill til høringer
- Høringsprogram
- Høringsinnspill
- Talerliste
- Publikasjoner
- Publikasjon
- Publikasjonsfigur (not relevant?)
- Representanttaleaktivitet
