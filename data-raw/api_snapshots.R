## Example datasets that are snapshots of the API
##
## These datasets are plain API output, used in the vignette and examples. Most
## were retrieved on 2022-06-03 (see the `response_date` variable in each), before
## build scripts were kept in the package; this script records the calls that
## produce them. Rerunning a call downloads the current data, which may differ
## from the stored snapshot, and the output of some functions has changed since:
##
## - `interp0203`: the columns were renamed in version 0.5.0 (e.g. `qustion_*` to
##   `question_*`) without downloading the data again.
## - `cases`: `get_session_cases()` now returns `$spokespersons` as a data frame.
## - `vote` and `covid_relief`: `get_vote()` no longer returns `vote_method`.
##
## The vignette refers to specific rows of `cases` and `vote`, so these should
## only be replaced together with the vignette text.
##
## `parl_periods` and `parl_sessions` were retrieved again on 2026-09-30.
##
## Run from the package root.

pkgload::load_all()

# Retrieved 2022-06-03
cases <- get_session_cases("2019-2020")
covid_relief <- get_vote("85196")
covid_relief_result <- get_result_vote("17689")
interp0203 <- get_session_questions("2002-2003", q_type = "interpellasjoner")
mps4549 <- get_parlperiod_mps("1945-49")
vote <- get_vote("78686")
vote_result <- lapply(vote$vote_id, get_result_vote)

# Retrieved 2026-09-30
parl_periods <- get_parlperiods()
parl_sessions <- get_parlsessions()

usethis::use_data(cases, covid_relief, covid_relief_result, interp0203, mps4549,
                  vote, vote_result, parl_periods, parl_sessions, overwrite = TRUE)
