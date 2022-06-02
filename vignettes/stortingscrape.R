## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(stortingscrape)

## ---- echo=FALSE--------------------------------------------------------------
cat(
  "<person>
  <respons_dato_tid>2021-08-13T14:59:48.2114895+02:00</respons_dato_tid>
  <versjon>1.6</versjon>
  <doedsdato>0001-01-01T00:00:00</doedsdato>
  <etternavn>Aasen</etternavn>
  <foedselsdato>1967-02-21T00:00:00</foedselsdato>
  <fornavn>Marianne</fornavn>
  <id>MAAA</id>
  <kjoenn>kvinne</kjoenn>
</person>
"  )

## ----covid_relief, eval=TRUE--------------------------------------------------
covid_relief <- get_vote("85196")
dim(covid_relief)

## ----sleep1, echo=FALSE, eval=TRUE--------------------------------------------
Sys.sleep(abs(rnorm(1, 5)))

## ----covid_struct, eval=TRUE--------------------------------------------------
head(covid_relief[, c("case_id", "vote_id", "n_for", "n_against", "adopted")])

## ----covid_id, eval=TRUE------------------------------------------------------

covid_relief$vote_id[which(grepl("217", covid_relief$vote_topic))]


## ----covid_relief_result, eval=TRUE-------------------------------------------

covid_relief_result <- get_result_vote("17689")
head(covid_relief_result[, c("vote_id", "mp_id", "party_id", "vote")])


## ----sleep2, echo=FALSE, eval=TRUE--------------------------------------------
Sys.sleep(abs(rnorm(1, 5)))

## ----party_dist, eval=TRUE----------------------------------------------------

party_dist <- table(covid_relief_result$party_id, covid_relief_result$vote)
addmargins(party_dist)


## ----per_sess, eval=TRUE------------------------------------------------------


periods <- get_parlperiods()
sessions <- get_parlsessions()

tail(periods[,c("id", "years")])
tail(sessions[,c("id", "years")])


## ----sleep3, echo=FALSE, eval=TRUE--------------------------------------------
Sys.sleep(abs(rnorm(1, 5)))

## ----period_id, eval=TRUE-----------------------------------------------------

periods$id[nrow(periods)]


## ----mps4549, eval=TRUE-------------------------------------------------------

mps4549 <- get_parlperiod_mps("1945-49")
head(mps4549[, c("mp_id", "county_id", "party_id", "period_id")])

## ----sleep4, echo=FALSE, eval=TRUE--------------------------------------------
Sys.sleep(abs(rnorm(1, 5)))

## ----interp0203, eval=TRUE----------------------------------------------------
interp0203 <- get_session_questions("2002-2003", q_type = "interpellasjoner")
dim(interp0203)

## ----sleep5, echo=FALSE, eval=TRUE--------------------------------------------
Sys.sleep(abs(rnorm(1, 5)))

## ----cases, eval=TRUE---------------------------------------------------------
cases <- get_session_cases("2019-2020")

## ----sleep6, echo=FALSE, eval=TRUE--------------------------------------------
Sys.sleep(abs(rnorm(1, 5)))

## ----get_vote, eval=TRUE------------------------------------------------------

# The case titles are, unfortunately, not translated
cases$root$title_short[48]

vote <- get_vote(cases$root$id[48])

vote[, c("case_id", "vote_id", 
         "alternative_vote", 
         "n_for", "n_absent", "n_against")]

## ----sleep7, echo=FALSE, eval=TRUE--------------------------------------------
Sys.sleep(abs(rnorm(1, 5)))

## ----vote_result, eval=TRUE---------------------------------------------------

result <- lapply(vote$vote_id, get_result_vote, good_manners = 5)
names(result) <- vote$vote_id

result <- do.call(rbind, result)
head(result[, 3:ncol(result)])


## ----votetab, eval=TRUE-------------------------------------------------------
table(result$vote, result$party_id,
      dnn = c("Vote result", "Vote ID")) |>
  prop.table(margin = 2) |>
  round(digits = 2)


