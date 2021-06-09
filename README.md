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

- Stress test all functions
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
- ~~Saker~~
- Ventede saker
- ~~Sak~~
- ~~Voteringer~~
- ~~Voteringsforslag~~
- ~~Voteringsvedtak~~
- ~~Voteringsresultat~~
- ~~Vedtak~~
- ~~Møter~~
- ~~Dagsorden~~
- ~~Spørretimen~~
- ~~Høringer~~
- ~~Skriftlige innspill til høringer~~
- ~~Høringsprogram~~
- ~~Høringsinnspill~~
- Talerliste (dynamic list of upcoming speeches -- might implement in future)
- ~~Publikasjoner~~
- Publikasjon -- unstandardized format across sessions and unsymmetrical xml files. Example (pulicationid = refh-201920-08-10)

```
<Forhandlinger Id="refh-201920-08-10-2" Status="Komplett">
  <Mote Id="refh-201920-08-10-3">
    <Startseksjon Id="refh-201920-08-10-4">
      <Motestart Id="refh-201920-08-10-5">
        <Tittel Id="refh-201920-08-10-6">Åpen høring i finanskomiteen mandag den 10. august 2020 kl. 12 </Tittel>
        <President Id="refh-201920-08-10-7">
          <A Id="refh-201920-08-10-8">
            Møteleder:
              <Uth Type="Sperret" Id="refh-201920-08-10-9">Mudassar Kapur (H)</Uth>
              (komiteens leder)
            </A>
        </President>
      </Motestart>
    </Startseksjon>
  <Hovedseksjon Id="refh-201920-08-10-10">
    <Saker Id="refh-201920-08-10-11">
      <Sak Id="refh-201920-08-10-12">
        <Sakshode Id="refh-201920-08-10-13">
          <Saknr Id="refh-201920-08-10-14">Sak:</Saknr>
          <Saktittel Id="refh-201920-08-10-15">
            <A Id="refh-201920-08-10-16">Dokument 9:1 for 2019–2020 Brev til Stortinget fra Norges Banks representantskap om ansettelse av ny daglig leder av Norges Bank Investment Management (NBIM)</A>
          </Saktittel>
        </Sakshode>
        <Hovedinnlegg Id="refh-201920-08-10-17">
          <A Id="refh-201920-08-10-18">
            <Navn Id="refh-201920-08-10-19">Møtelederen:</Navn>
            Da kan jeg ønske velkommen til åpen høring i Stortingets finanskomité i følgende sak: Brev til Stortinget fra Norges Banks representantskap om ansettelse av ny daglig leder av Norges Bank Investment Management, Dokument 9:1 for 2019–2020.
          </A>
```
- Publikasjonsfigur (not relevant?)
- ~~Representanttaleaktivitet~~
