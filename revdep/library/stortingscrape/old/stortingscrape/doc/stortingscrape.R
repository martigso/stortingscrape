## ---- include = FALSE---------------------------------------------------------
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

## ----load_predl_data----------------------------------------------------------
data_files <- data(package = "stortingscrape")$results[,"Item"]
data(list = data_files)

## ----covid_relief, eval=-1----------------------------------------------------
covid_relief <- get_vote("85196")


## ----covid_struct-------------------------------------------------------------
head(covid_relief[, c("case_id", "vote_id", "n_for", "n_against", "adopted")])

## ----covid_id, eval=FALSE-----------------------------------------------------
#  
#  covid_relief$vote_id[which(grepl("217", covid_relief$vote_topic))]
#  

## ----covid_relief_result, eval=-1---------------------------------------------
covid_relief_result <- get_result_vote("17689")

head(covid_relief_result[, c("vote_id", "mp_id", "party_id", "vote")])


## ----party_dist---------------------------------------------------------------

table(covid_relief_result$party_id, 
      covid_relief_result$vote) |>
  addmargins()


## ----per_sess, eval=-(1:2)----------------------------------------------------
parl_periods <- get_parlperiods()
parl_sessions <- get_parlsessions()

tail(parl_periods[,c("id", "years")])
tail(parl_sessions[,c("id", "years")])


## ----period_id----------------------------------------------------------------

parl_periods$id[nrow(parl_periods)]


## ----mps4549, eval=-1---------------------------------------------------------
mps4549 <- get_parlperiod_mps("1945-49")
head(mps4549[, c("mp_id", "county_id", "party_id", "period_id")])

## ----interp0203, eval=-1------------------------------------------------------
interp0203 <- get_session_questions("2002-2003", q_type = "interpellasjoner")
dim(interp0203)

## ----cases, eval=-1-----------------------------------------------------------
cases <- get_session_cases("2019-2020")

## ----get_vote-----------------------------------------------------------------

# The case titles are, unfortunately, not translated
cases$root$title_short[48]

## ----get_vote2, eval=-1-------------------------------------------------------
vote <- get_vote(cases$root$id[48])

vote[, c("case_id", "vote_id", 
         "alternative_vote", 
         "n_for", "n_absent", "n_against")]

## ----vote_result, eval=-1-----------------------------------------------------
vote_result <- lapply(vote$vote_id, get_result_vote, good_manners = 5)
names(vote_result) <- vote$vote_id

vote_result <- do.call(rbind, vote_result)
head(vote_result[, 3:ncol(vote_result)])


## ----votetab, eval=FALSE------------------------------------------------------
#  table(vote_result$vote, vote_result$party_id,
#        dnn = c("Vote result", "Vote ID")) |>
#    prop.table(margin = 2) |>
#    round(digits = 2)
#  

