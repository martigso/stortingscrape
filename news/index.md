# Changelog

## stortingscrape 0.5.0

- Major changes
  - The single-id data-retrieving functions now accept a **vector of
    ids**. Passing several ids (e.g. `get_question(c(id1, id2))`)
    retrieves them all in one call: functions returning a `data.frame`
    bind the rows together (with plain row names, as the ids are in the
    data), while functions returning a list
    (e.g. [`get_mp_bio()`](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md),
    [`get_case()`](https://martigso.github.io/stortingscrape/reference/get_case.md),
    [`get_publication()`](https://martigso.github.io/stortingscrape/reference/get_publication.md))
    return a named list of results keyed by id. Passing a single id
    works as before. Individual ids that fail are turned into warnings
    so a single bad id does not discard the successful ones. In
    interactive sessions, a progress bar (from the `cli` package) shows
    the progress when retrieval takes more than a few seconds.
  - All API calls now respect [Stortinget’s documented rate limit of 100
    calls per
    minute](https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/).
    Requests are throttled automatically and transient
    `429 Too Many Requests` responses are retried (respecting the
    `Retry-After` header). The `good_manners` argument remains available
    for additional polite pacing.
  - New function
    [`get_speeches()`](https://martigso.github.io/stortingscrape/reference/get_speeches.md)
    returns the speeches of a debate transcript (publication type
    “referat”) as a data frame, one row per speech, with the same
    variables for transcripts before and after the 2016-2017 format
    change. The raw speaker string is kept alongside the parsed title,
    name, party, and time stamp. Speech elements holding several
    speakers are split into one row per speaker, and the sitting chair
    (president or meeting leader) is tracked through each transcript. By
    default (`link = TRUE`), it also adds person ids linked from the
    speaker and chair names (see `speaker_links`), including for
    transcripts before 2016-2017, where the API gives no ids. The
    example dataset `speeches140213` holds the output for one
    transcript.
  - New dataset `speaker_links`: person ids for the speakers and chairs
    in all debate transcripts from 1998-99 onward, linked from their
    names by session (see
    [`?speaker_links`](https://martigso.github.io/stortingscrape/reference/speaker_links.md)
    for the rules and `data-raw/speaker_links.R` for the build).
    [`get_speeches()`](https://martigso.github.io/stortingscrape/reference/get_speeches.md)
    adds these links by default.
  - **Breaking:** misspelled variable names are corrected: `qustion_*`
    to `question_*` in
    [`get_question()`](https://martigso.github.io/stortingscrape/reference/get_question.md);
    `answ_on_belhalf_of*` to `answ_on_behalf_of*` and `sendt_date` to
    `sent_date` in
    [`get_question()`](https://martigso.github.io/stortingscrape/reference/get_question.md),
    [`get_session_questions()`](https://martigso.github.io/stortingscrape/reference/get_session_questions.md),
    and the `interp0203` dataset; and `$poceedings_steps` to
    `$proceedings_steps` in
    [`get_proceedings()`](https://martigso.github.io/stortingscrape/reference/get_proceedings.md).
  - The `mp_id` argument of
    [`get_session_mp_speech_activity()`](https://martigso.github.io/stortingscrape/reference/get_session_mp_speech_activity.md)
    is renamed to `mpid`, as in the other functions. `mp_id` still
    works, with a deprecation warning.
  - **Breaking:**
    [`get_publication()`](https://martigso.github.io/stortingscrape/reference/get_publication.md)
    now parses publications as XML rather than HTML. The HTML parser
    lowercased all element and attribute names (e.g. `personID` became
    `personid`) and split up nested paragraphs. Element names are now
    case sensitive: publications from 2016-2017 onward use capitalized
    names, so selectors such as `html_elements(pub, "replikk")` must
    become `html_elements(pub, "Replikk")`.
- Minor changes
  - The shared `httr2` request pipeline was refactored into internal
    helpers (`api_perform()`, `api_get()`), removing roughly a thousand
    lines of duplicated boilerplate across the data-retrieving functions
    with no change to their returned output.
  - Minimum `httr2` version is now 1.1.0.
  - The rate limit is now enforced over any 60-second window. The
    throttle’s token bucket starts full, so the previous setting (a
    bucket of 100 refilling at 100 per minute) allowed up to about 200
    calls in the first minute; requests now come in bursts of at most 10
    and then at 90 per minute.
  - [`get_session_cases()`](https://martigso.github.io/stortingscrape/reference/get_session_cases.md),
    [`get_session_hearings()`](https://martigso.github.io/stortingscrape/reference/get_session_hearings.md),
    [`get_session_mp_speech_activity()`](https://martigso.github.io/stortingscrape/reference/get_session_mp_speech_activity.md),
    and
    [`get_mp_pic()`](https://martigso.github.io/stortingscrape/reference/get_mp_pic.md)
    now also accept vectors of ids (for
    [`get_mp_pic()`](https://martigso.github.io/stortingscrape/reference/get_mp_pic.md),
    with one `destfile` per id).
  - [`get_session_mp_speech_activity()`](https://martigso.github.io/stortingscrape/reference/get_session_mp_speech_activity.md)
    gains an `mp_id` variable, and
    [`get_parlperiod_presidency()`](https://martigso.github.io/stortingscrape/reference/get_parlperiod_presidency.md)
    a `period_id` variable, so results for several ids can be told
    apart.
  - [`get_written_hearing_input()`](https://martigso.github.io/stortingscrape/reference/get_written_hearing_input.md)
    keeps the hearing id for hearings without written input.
  - Fixed
    [`get_proposal_votes()`](https://martigso.github.io/stortingscrape/reference/get_proposal_votes.md)
    failing for votes where a proposal had no delivering MP; the
    proposal variables are now read per proposal, with `NA` for missing
    values.
  - [`get_speeches()`](https://martigso.github.io/stortingscrape/reference/get_speeches.md)
    records the time of retrieval in `response_date`, as the transcripts
    have none of their own.
  - Fixed
    [`get_session_questions()`](https://martigso.github.io/stortingscrape/reference/get_session_questions.md)
    ignoring `status` when given several sessions, and
    [`get_session_hearings()`](https://martigso.github.io/stortingscrape/reference/get_session_hearings.md)
    ignoring `cores` for the hearing dates.
  - With several ids, a failing id in
    [`get_mp_pic()`](https://martigso.github.io/stortingscrape/reference/get_mp_pic.md)
    gives a warning rather than stopping the rest.
  - The `parl_periods` and `parl_sessions` datasets are updated to
    include the 2025-2029 period and its sessions.
  - Corrected documentation, including the returned variables of several
    functions, the dataset descriptions, and examples using the vector
    of ids; `good_manners` is documented as numeric (seconds, e.g. 0.6).
  - Added offline tests (testthat) for the transcript parser, date
    handling, speaker linking, and the vector-of-ids helper.
  - Fixed getters failing, or misaligning variables, when a hearing has
    no committee
    ([`get_session_hearings()`](https://martigso.github.io/stortingscrape/reference/get_session_hearings.md),
    [`get_hearing_program()`](https://martigso.github.io/stortingscrape/reference/get_hearing_program.md),
    [`get_hearing_input()`](https://martigso.github.io/stortingscrape/reference/get_hearing_input.md),
    [`get_written_hearing_input()`](https://martigso.github.io/stortingscrape/reference/get_written_hearing_input.md)),
    or when a representative has no party or county (the spokespersons
    in
    [`get_session_cases()`](https://martigso.github.io/stortingscrape/reference/get_session_cases.md),
    and
    [`get_vote()`](https://martigso.github.io/stortingscrape/reference/get_vote.md),
    [`get_result_vote()`](https://martigso.github.io/stortingscrape/reference/get_result_vote.md),
    [`get_parlperiod_mps()`](https://martigso.github.io/stortingscrape/reference/get_parlperiod_mps.md),
    [`get_question()`](https://martigso.github.io/stortingscrape/reference/get_question.md)).
    These variables are now read per record.
  - [`get_hearing_input()`](https://martigso.github.io/stortingscrape/reference/get_hearing_input.md)
    returns a row of `NA` for hearings without input (the API answers
    with an error), like
    [`get_written_hearing_input()`](https://martigso.github.io/stortingscrape/reference/get_written_hearing_input.md).
  - [`get_hearing_program()`](https://martigso.github.io/stortingscrape/reference/get_hearing_program.md)
    handles programs with a single element, and
    [`get_proceedings()`](https://martigso.github.io/stortingscrape/reference/get_proceedings.md)
    compares step numbers as numbers.
  - [`get_parlperiod_mps()`](https://martigso.github.io/stortingscrape/reference/get_parlperiod_mps.md)
    no longer prints a message for each period, and
    [`get_session_cases()`](https://martigso.github.io/stortingscrape/reference/get_session_cases.md)
    and
    [`get_session_hearings()`](https://martigso.github.io/stortingscrape/reference/get_session_hearings.md)
    use one core on Windows, where `mclapply()` cannot use more.
  - Requests with Æ, Ø, or Å in an id (e.g. some person ids) are now
    sent as UTF-8, so they also work on systems whose native encoding is
    not UTF-8, and weekday names with å or ø in meeting headings are
    read correctly on such systems.

## stortingscrape 0.4.1

CRAN release: 2025-04-07

- Minor changes
  - Fixed a CRAN issue with the
    [`get_topics()`](https://martigso.github.io/stortingscrape/reference/get_topics.md)
    function throwing an error because of an example
    - Fixed by adding in a `\dontrun{}` for the example

## stortingscrape 0.4.0

CRAN release: 2025-03-07

- Major changes
  - [**Stortinget’s API updated their ID scheme for all
    questions**](https://data.stortinget.no/nyhetsoversikt/endring-i-id-er/)
    - I cannot guarantee that it will be possible to convert previously
      downloaded data to the new format. The API change did not
      facilitate this. If you need to append your data, I advise
      starting from scratch
    - I am not happy about this, but I can do nothing
    - [`get_question()`](https://martigso.github.io/stortingscrape/reference/get_question.md)
      has been updated to the new scheme, and the `legacy_id` variable
      added
    - [`get_meeting_agenda()`](https://martigso.github.io/stortingscrape/reference/get_meeting_agenda.md)
      is updated with `legacy_question_id` keys

## stortingscrape 0.3.2

- Major changes
  - Changed
    [`get_mp_pic()`](https://martigso.github.io/stortingscrape/reference/get_mp_pic.md)
    to utilize the `magick` package instead of `imager` when
    `show_plot = TRUE`
- Minor changes
  - Added color palette for current political parties in the Storting

## stortingscrape 0.3.1

- Minor changes
  - Fixed call to sleep in
    [`get_publication()`](https://martigso.github.io/stortingscrape/reference/get_publication.md)
    function (was missing)
  - Redirected citation to inst/CITATION file
  - Fixed a minor error in the
    [`get_proposal_votes()`](https://martigso.github.io/stortingscrape/reference/get_proposal_votes.md)
    function for the `proposal_delivered_by_mp` variable

## stortingscrape 0.3.0

CRAN release: 2024-01-18

- Minor changes
  - Fixed an error in
    [`get_session_cases()`](https://martigso.github.io/stortingscrape/reference/get_session_cases.md),
    where the structuring of case proposers did not run in parallel, as
    was intended.

## stortingscrape 0.2.0

- Major changes:
  - Replaced `magrittr` (`%>%`) pipes with native pipes (`|>`)
  - Converted all get\_\*() functions from `httr` to
    [`httr2`](https://httr2.r-lib.org/)
  - Changed \$spokespersons in
    [`get_session_cases()`](https://martigso.github.io/stortingscrape/reference/get_session_cases.md)
    to data frame. This will break backwards compatibility (sorry!).
  - Rewrote the *decision_text* variable
    in[`get_session_decisions()`](https://martigso.github.io/stortingscrape/reference/get_session_decisions.md)
    so that residual html is stripped from the output. This might break
    some text processing applications (sorry!).
  - Rewrote the date and info sections of
    [`get_session_hearings()`](https://martigso.github.io/stortingscrape/reference/get_session_hearings.md)
    to data.frames instead of lists. This might break some text
    processing applications (sorry!).
- Minor changes:
  - Rewrote the *proceedings_steps* variable
    in[`get_proceedings()`](https://martigso.github.io/stortingscrape/reference/get_proceedings.md)
    to be scalable to changes in the API. Should not break backwards
    compatibility.

## stortingscrape 0.1.4

- Major changes
  - Fixed a bug in
    [`get_session_questions()`](https://martigso.github.io/stortingscrape/reference/get_session_questions.md),
    where the presence of unanswered questions returned an error instead
    of `NA`
  - Removed a variable from
    [`get_vote()`](https://martigso.github.io/stortingscrape/reference/get_vote.md)
    because it suddenly disappeared from the API.
  - Fixed an issue where
    [`ifelse()`](https://rdrr.io/r/base/ifelse.html) lines returned only
    one element when it was supposed to return several due to someone
    not realizing the vectorization rules of the function. Affected
    functions were:
    [`get_case()`](https://martigso.github.io/stortingscrape/reference/get_case.md),
    [`get_hearing_program()`](https://martigso.github.io/stortingscrape/reference/get_hearing_program.md),
    [`get_question()`](https://martigso.github.io/stortingscrape/reference/get_question.md),
    [`get_result_vote()`](https://martigso.github.io/stortingscrape/reference/get_result_vote.md),
    [`get_session_cases()`](https://martigso.github.io/stortingscrape/reference/get_session_cases.md),
    and
    [`get_session_questions()`](https://martigso.github.io/stortingscrape/reference/get_session_questions.md).
    Only
    [`get_case()`](https://martigso.github.io/stortingscrape/reference/get_case.md)
    was significant, in that it listed all bill sponsors as being from
    the same party.
- Minor changes
  - Fixed some typos in the readme
  - Reworked the
    [`read_obt()`](https://martigso.github.io/stortingscrape/reference/read_obt.md)
    function for the package not to rely on `dplyr`
  - Added a hex badge logo. Extremely important.

## stortingscrape 0.1.3

CRAN release: 2023-03-23

- Major changes
  - Fixed an issue with
    [`get_mp_bio()`](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md),
    which broke after [an API
    update](https://data.stortinget.no/nyhetsoversikt/endringer-i-biografidata/).
  - Fixed [typo
    issue](https://github.com/martigso/stortingscrape/issues/3) –
    renaming some variables in
    [`get_session_questions()`](https://martigso.github.io/stortingscrape/reference/get_session_questions.md)
- Minor changes
  - Added [pkgdown page](https://martigso.github.io/stortingscrape/) via
    gh-pages
  - Changed color of text in logo
  - Added a `NEWS.md` file to track changes to the package.
