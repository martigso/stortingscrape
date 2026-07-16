#' Perform the shared Stortinget API request pipeline
#'
#' Internal helper wrapping the \pkg{httr2} request pipeline used by all
#' data-retrieving functions. Enforces the API's documented limit of 100 calls
#' per minute (\url{https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/})
#' and automatically retries on HTTP 429 ("Too Many Requests"), respecting the
#' \code{Retry-After} header. Returns the raw response object with no status
#' check, so callers can implement bespoke status handling.
#'
#' @param url Character. Fully built request URL.
#'
#' @return An \pkg{httr2} response object (any status code).
#'
#' @keywords internal
#' @noRd
api_perform <- function(url) {

  request(url) |>
    req_throttle(capacity = 100, fill_time_s = 60) |>
    req_retry(max_tries = 5, is_transient = function(resp) resp_status(resp) == 429) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

}

#' Perform a single Stortinget API request, erroring on non-200
#'
#' Internal helper building on \code{\link{api_perform}} that adds the standard
#' status-code check. Endpoints with bespoke status handling (e.g. treating a
#' 500 as "no data") should call \code{\link{api_perform}} directly instead.
#'
#' @param url Character. Fully built request URL.
#'
#' @return An \pkg{httr2} response object with status code 200.
#'
#' @keywords internal
#' @noRd
api_request <- function(url) {

  resp <- api_perform(url)

  if(resp$status_code != 200) {
    stop(
      paste0(
        "Response of ",
        url,
        " is '",
        resp |> resp_status_desc(),
        "' (",
        resp$status_code,
        ")."
      ),
      call. = FALSE)
  }

  resp

}

#' Perform a Stortinget API request and return parsed XML
#'
#' Internal helper building on \code{\link{api_request}} for the (large
#' majority of) endpoints that return \code{text/xml}. Adds the content-type
#' check and parses the body as HTML/XML.
#'
#' @param url Character. Fully built request URL.
#'
#' @return A parsed \pkg{rvest}/\pkg{xml2} document.
#'
#' @keywords internal
#' @noRd
api_get <- function(url) {

  resp <- api_request(url)

  if(resp_content_type(resp) != "text/xml") {
    stop(
      paste0(
        "Response of ",
        url,
        " returned as '",
        resp_content_type(resp),
        "'.",
        " Should be 'text/xml'."),
      call. = FALSE)
  }

  resp |>
    resp_body_html(check_type = FALSE, encoding = "utf-8")

}

#' Apply a single-id getter over multiple ids
#'
#' Internal helper that vectorizes the single-id data-retrieving functions. When
#' a getter is called with more than one id, it recurses over the ids one at a
#' time (preserving the per-call \code{good_manners} delay and the shared rate
#' limit) and binds the results. Individual failures are turned into warnings so
#' that a single bad id does not discard the successful ones.
#'
#' @param ids Vector of ids.
#' @param .f The single-id getter to apply (e.g. \code{get_question}).
#' @param good_manners Integer. Seconds delay between calls, passed through to \code{.f}.
#' @param .combine Function used to combine the per-id results, or \code{NULL} to
#'   return the raw list (for getters that return a list of data frames). Defaults
#'   to \code{rbind}, which silently drops \code{NULL} results from failed ids.
#' @param ... Further arguments passed on to \code{.f} on every call (e.g. the
#'   \code{q_type} argument of \code{\link{get_session_questions}}).
#'
#' @return A combined data.frame (default) or a named list of results.
#'
#' @keywords internal
#' @noRd
fetch_multi <- function(ids, .f, good_manners = 0, .combine = rbind, ...) {

  out <- lapply(ids, function(id) {
    tryCatch(
      .f(id, ..., good_manners = good_manners),
      error = function(e) {
        warning("id '", id, "' failed: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  })

  names(out) <- ids

  if(is.null(.combine)) return(out)

  do.call(.combine, out)

}
