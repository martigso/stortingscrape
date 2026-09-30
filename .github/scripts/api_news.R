# Check the news page of the Stortinget API (https://data.stortinget.no/nyhetsoversikt/)
# and open a GitHub issue labelled "api-news" for every news item that is not
# already covered by an issue. The page has no RSS feed, so this script is run
# on a schedule by .github/workflows/api-news.yaml.
#
# Environment variables:
#   SINCE    Skip items published before this date (YYYY-MM-DD). Used to leave
#            out the back catalogue that predates the workflow.
#   DRY_RUN  If "true", print the issues that would be opened instead of opening
#            them. Does not need the gh CLI, so the script can be tested locally:
#            DRY_RUN=true SINCE=2025-01-01 Rscript .github/scripts/api_news.R

library(rvest)

base_url <- "https://data.stortinget.no"
label    <- "api-news"
since    <- as.Date(Sys.getenv("SINCE", "1900-01-01"))
dry_run  <- tolower(Sys.getenv("DRY_RUN")) == "true"

gh <- function(...) {

  out <- system2("gh", shQuote(c(...)), stdout = TRUE)

  if(!is.null(attr(out, "status"))) {
    stop("'gh ", ..1, "' failed with status ", attr(out, "status"), call. = FALSE)
  }

  out

}

# News overview
news <- read_html(paste0(base_url, "/nyhetsoversikt/")) |>
  html_elements(".news-list .news")

# Fail loudly (failed workflow run -> email) rather than silently finding nothing
if(length(news) == 0) {
  stop("No news items found; the page structure has probably changed.", call. = FALSE)
}

items <- data.frame(
  published = news |> html_element(".date") |> html_text2(),
  title     = news |> html_element("h2 a") |> html_text2(),
  url       = paste0(base_url, news |> html_element("h2 a") |> html_attr("href"))
)

items$date <- as.Date(items$published, format = "%d.%m.%Y")

if(anyNA(items$date)) {
  stop("Could not parse all dates on the news page.", call. = FALSE)
}

items <- items[items$date >= since, ]
items <- items[order(items$date), ]

# Issues already opened for news items (open or closed) are recognized by the
# article URL in the issue body
if(dry_run) {
  seen <- character()
} else {
  gh("label", "create", label,
     "--color", "1d76db",
     "--description", "News from data.stortinget.no/nyhetsoversikt",
     "--force")
  seen <- gh("issue", "list", "--label", label, "--state", "all",
             "--limit", "1000", "--json", "body", "--jq", ".[].body")
}

is_seen <- vapply(items$url, function(u) any(grepl(u, seen, fixed = TRUE)), logical(1))

new_items <- items[!is_seen, ]

message(nrow(items), " item(s) since ", since, ", ", nrow(new_items), " new.")

for(i in seq_len(nrow(new_items))) {

  item <- new_items[i, ]

  article <- read_html(item$url) |>
    html_element("main .columns") |>
    html_elements("p:not(.last-updated)") |>
    html_text2()

  article <- unique(article[article != ""])

  title <- paste0("data.stortinget.no: ", item$title)

  body <- paste(c(paste0("Published ", item$published, ": <", item$url, ">"),
                  "",
                  paste(article, collapse = "\n\n"),
                  "",
                  "---",
                  "*Opened automatically by the `api-news` workflow.*"),
                collapse = "\n")

  if(dry_run) {
    cat("\n=== ", title, " ===\n", body, "\n", sep = "")
  } else {
    body_file <- tempfile(fileext = ".md")
    writeLines(body, body_file)
    gh("issue", "create", "--title", title, "--body-file", body_file, "--label", label)
  }

}
