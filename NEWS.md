# stortingscrape 0.3.1

# stortingscrape 0.3.0

# stortingscrape 0.2.0

- Major changes:
    * Replaced `magrittr` (`%>%`) pipes with native pipes (`|>`)
    * Converted all get_*() functions from `httr` to [`httr2`](https://httr2.r-lib.org/)
    * Changed $spokespersons in `get_session_cases()` to data frame. This will break backwards compatibility (sorry!).
    * Rewrote the *decision_text* variable in`get_session_decisions()` so that residual html is stripped from the output. This might break some text processing applications (sorry!).
    * Rewrote the date and info sections of `get_session_hearings()` to data.frames instead of lists. This might break some text processing applications (sorry!).
- Minor changes:
    * Rewrote the *proceedings_steps* variable in`get_proceedings()` to be scalable to changes in the API. Should not break backwards compatibility.

# stortingscrape 0.1.4

- Major changes
    * Fixed a bug in `get_session_questions()`, where the presence of unanswered questions returned an error instead of `NA`
    * Removed a variable from `get_vote()` because it suddenly disappeared from the API.
    * Fixed an issue where `ifelse()` lines returned only one element when it was supposed to return several due to someone not realizing the vectorization rules of the function. Affected functions were: `get_case()`, `get_hearing_program()`, `get_question()`, `get_result_vote()`, `get_session_cases()`, and `get_session_questions()`. Only `get_case()` was significant, in that it listed all bill sponsors as being from the same party.
- Minor changes
    * Fixed some typos in the readme
    * Reworked the `read_obt()` function for the package not to rely on `dplyr`
    * Added a hex badge logo. Extremely important.

# stortingscrape 0.1.3

- Major changes
    # * Fixed an issue with `get_mp_bio()`, which broke after [an API update](https://data.stortinget.no/nyhetsoversikt/endringer-i-biografidata/).
    * Fixed [typo issue](https://github.com/martigso/stortingscrape/issues/3) -- renaming some variables in `get_session_questions()
- Minor changes
    * Added [pkgdown page](https://martigso.github.io/stortingscrape/) via gh-pages 
    * Changed color of text in logo
    * Added a `NEWS.md` file to track changes to the package.
