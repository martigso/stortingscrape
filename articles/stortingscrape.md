# stortingscrape: An R package for accessing data from the Norwegian parliament

## Introduction: Retrieval of Storting data in R

A wide variety of parliamentary data have been made available to the
public in several countries over the last decade. Be it through frontend
websites or back-end APIs, researchers on parliaments have never had
easier access to large amounts of data than they do now. However, both
frontend and API scraped data often come in formats (.html, .xml, .json,
etc) that require substantial structuring and pre-processing before they
are ready for subsequent analyses.

In this vignette, I present the `stortingscrape` package for R.
`stortingscrape` makes retrieving data from the Norwegian parliament
(*Stortinget*) through their easily accessible back-end API. The data
requested using the package require little to no further structuring.
The scope of the package, discussed further below, ranges from general
data on the parliament itself (rules, session info, committees, etc) to
data on the parties, bibliographies of the MPs, questions, hearings,
debates, votes, and more.

Although this is the first attempt to make data on *Stortinget* more
easily accessible, `stortingscrape` does not live in a vacuum. A variety
of parliamentary data for different countries are available for
researchers to use freely. For parliamentary debates, Thomas et al.
(2006) were one of the first to gather and make available data. Their
data cover the proceedings of the 2005 House debates. Eggers and
Spirling (2014) structured the UK Hansard speech data, which spans from
1802 to 2010. Beelen et al. (2017) provided continuously updated data
for the Canadian parliament, Rauh and Schwalbach (2020) made available a
collection of speech data from 9 countries, and Turner-Zwinkels et al.
(2021) developed a day-by-day dataset of MPs in Germany, Switzerland,
and the Netherlands, in the period between 1947 and 2017. These examples
are, however, different from `stortingscrape` in that they are finished
datasets ready for download and have limited scope.

The main goal of `stortingscrape` is to allow researchers to access any
data from the Norwegian parliament easily, but also still be able to
structure the data according to one’s need. Most importantly, the
package is facilitated for weaving together different parts of the
data.stortinget.no API.

I will start this vignette by briefly discussing the openly accessible
data.stortinget.no API. Next, I will describe the philosophy, scope and
general usage of the `stortingscrape` package. Finally, I will present
some minimal examples of possible workflows for working with the
package, before I summarize.

## Stortinget’s API

The Norwegian parliament was comparatively early in granting open access
to their data through an API when they launched
[data.stortinget.no](https://data.stortinget.no) in 2012. The general
purpose of the API is to provide transparency in the form of raw data,
mirroring the frontend web-page information from
[stortinget.no](https://stortinget.no). The format of the API has been
fairly consistent over the time of its existence, but there have been
some small style changes over different versions.[^1] `stortingscrape`
was built under version $`1.6`$ of the API.

Except for content that is blocked for the public (e.g. debates behind
closed doors), the API contains all recorded data produced in
Stortinget. These data include data on individual MPs, transcripts from
debates, voting results, hearing input, and much more. For an exhaustive
list of all data sources in the API, see the list of functions.[^2] The
data available in the API can be accessed through XML or JSON
format[^3], both of which are flexible formats for compressing data in
nested lists.

As an example, the raw data input for general information about a single
MP[^4] looks like this:

    #> <person>
    #>   <respons_dato_tid>2021-08-13T14:59:48.2114895+02:00</respons_dato_tid>
    #>   <versjon>1.6</versjon>
    #>   <doedsdato>0001-01-01T00:00:00</doedsdato>
    #>   <etternavn>Aasen</etternavn>
    #>   <foedselsdato>1967-02-21T00:00:00</foedselsdato>
    #>   <fornavn>Marianne</fornavn>
    #>   <id>MAAA</id>
    #>   <kjoenn>kvinne</kjoenn>
    #> </person>

This is the typical XML structure in the API, although other parts of
the data are more complex in that the XML tree can be nested multiple
times. This will be discussed further in the next section.

## Package philosophy, scope, and usage

`stortingscrape` aims to make Norwegian parliamentary data easily
accessible, while also being flexible enough for tailoring the different
underlying data sources to one’s needs. Indeed, contrary to most open
source parliamentary speech data, `stortingscrape` aims at giving the
user as much agency as possible in tailoring data for specific needs. In
addition to user agency, the package is built with a core philosophy of
simplifying data structures, make seamless workflows between different
parts of the *Storting* API, and limit data duplication between
functions.

Because a lot of analysis tools in R requires 2 dimensional data
formats, the `stortingscrape` package prioritize converting the nested
XML format to data frames, when possible. However, some sources of data
from the Storting API are nested in a way which makes retaining all data
in a 2 dimensional space either impossible or too verbose. For example,
the
[`get_mp_bio()`](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md)
function, which extract a specific MP’s biography by id, has data on MP
personalia, parliamentary periods the MP had a seat, vocations,
literature authored by the MP, and more. In order to make all these data
workable, the resulting format from the function call is a list of data
frames for each part of the data. The different list elements are,
however, easily combined for different applications of the data.

One of the core thoughts behind the workflow of the package is to make
it easy to combine different parts of the API and to extract the data
you actually need. To facilitate this, most functions within
`stortingscrape` are built to work seamlessly with the
[`apply()`](https://rdrr.io/r/base/apply.html) family or control flow
constructs in R. Because we do not want to call the API repeatedly,
functions that are expected to often be ran repeatedly have a
`good_manners` argument. This will make R sleep for the set amount of
seconds after calling the API. It is recommended to set this argument to
2 seconds or higher on multiple calls to the API. Generally, the package
is built by the recommendations given by the `httr2` package (Wickham
2023)[^5].

Note that Stortinget limits the API to 100 calls per minute; exceeding
this returns an `HTTP 429 ("Too Many Requests")` response.[^6] Since
version 0.5.0, `stortingscrape` enforces this limit automatically by
throttling all calls and retrying transient `429` responses, so the
100-per-minute ceiling is never crossed. The `good_manners` argument
remains useful as an additional, more conservative pacing on top of this
safety net; a delay of 0.6 seconds already keeps you at the hard limit,
so the recommended 2 seconds stays comfortably below it.

Since version 0.5.0, the single-id data-retrieving functions also accept
a *vector* of ids. Passing several ids at once — for instance
`get_question(c(id1, id2))` — retrieves them all in a single call.
Functions that return a `data.frame` bind the rows together, while
functions that return a list (such as
[`get_mp_bio()`](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md),
[`get_case()`](https://martigso.github.io/stortingscrape/reference/get_case.md),
or
[`get_publication()`](https://martigso.github.io/stortingscrape/reference/get_publication.md))
return a named list of results keyed by id. Passing a single id behaves
exactly as before. An individual id that fails is turned into a warning
rather than an error, so a single bad id does not discard the results of
the successful ones.

Most of the data from Stortinget’s API and frontend web page are
interconnected through ids for the various sources (session id, MP id,
case id, question id, vote id, etc.). `stortingscrape` core extraction
methods are based around these. One of the major benefits of this is
that whether you want to extract, for instance, a single question found
on the frontend web page, or all questions for a parliamentary session,
the package is flexible enough to suit both needs (see the
[workflow](#workflow) section). It will also enable users to quickly
retrieve data from the frontend web-page.[^7]

Because of the interconnectedness of the API’s data, there are some
overlapping sources of data. For instance, MP general information
([`get_mp()`](https://martigso.github.io/stortingscrape/reference/get_mp.md)),
biographies
([`get_mp_bio()`](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md)),
and all MPs for a parliamentary period
([`get_parlperiod_mps()`](https://martigso.github.io/stortingscrape/reference/get_parlperiod_mps.md))
all have the names of the MPs in the API, but
[`get_mp_bio()`](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md)
does not return them in `stortingscrape`, because these data sources are
easily merged by the MP’s id (see the [workflow](#workflow) section).
Note that the same person id is called `mp_id` in most functions,
`person_id` in
[`get_speeches()`](https://martigso.github.io/stortingscrape/reference/get_speeches.md)
and
[`get_parlperiod_presidency()`](https://martigso.github.io/stortingscrape/reference/get_parlperiod_presidency.md),
and `id` in
[`get_mp()`](https://martigso.github.io/stortingscrape/reference/get_mp.md)
and
[`get_mp_bio()`](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md).

The scope of `stortingscrape` is almost the entire API of Stortinget,
with some notable shortcomings. First, there are no functions for
dynamically updated data sources, such as current speaker lists
(<https://data.stortinget.no/dokumentasjon-og-hjelp/talerliste/>).
Second, as mentioned above, duplicated data is avoided whenever
possible. Third, certain unstandardized image sources – such as
publication attachment figures – are not supported in the package. And
finally, publications from the
[`get_publication()`](https://martigso.github.io/stortingscrape/reference/get_publication.md)
function can be retrieved, but are returned as parsed XML (from the
`xml2` package) because these data are not standardized across different
publications. The exception is the debate transcripts, where
[`get_speeches()`](https://martigso.github.io/stortingscrape/reference/get_speeches.md)
returns the speeches as a data frame.

There are three overarching sources of data in `stortingscrape`: 1)
Parliamentary structure data, 2) MP data, and 3) Parliamentary activity
data. These are, in some/most cases, linked by various forms of ID tags.
For example, retrieving all MPs for a given session
([`get_parlperiod_mps()`](https://martigso.github.io/stortingscrape/reference/get_parlperiod_mps.md))
will give access to MP IDs (`mp_id`) for that session, which can be used
to extract biographies, pictures, speech activity, and more for those
MPs. Next, I will showcase some examples of how a typical workflow for
using `stortingscrape` could look like.

## Workflow

In the following section, I will discuss some examples of data
extraction with `stortingscrape`. I start by showing basic extraction of
voting data based on vote IDs from the frontend web-page –
[stortinget.no](https://stortinget.no). Next, I exemplify the large set
of period and session specific data by retrieving all MPs for a specific
parliamentary period and all interpellations for a specified
parliamentary session. Next, I show how the different functions of the
`stortingscrape` package works together – merging data on cases with
their belonging vote results. Finally, I show how to go from a debate
transcript to speeches with person ids for the speakers. Note that the
vignette is built using the examples in the data folder of the
package.[^8]

``` r

data_files <- data(package = "stortingscrape")$results[,"Item"]
data(list = data_files)
```

### Basic extraction

The basic extraction of specific data from Stortinget’s API revolves
around various forms of ID tags. For example, all MPs have a unique ID,
all cases have unique IDs, all votes have unique IDs, and so on. For the
following example, I will highlight going from a case on economic
measures for the Covid pandemic to party distribution on a specific vote
in this case. First, the case was relatively rapidly proposed and
treated in the Storting during the early days of June 2021. The case in
its entirety can be found at
[here](https://stortinget.no/no/Saker-og-publikasjoner/Saker/Sak/?p=85196).
You will see the procedure steps from a government proposal, through
work in the finance committee, to debate and decision. Let’s say a
particular proposal under the case caught our eye – for instance, vote
number 61 from the Labor Party asking the government to propose a plan
for implementing the International Labor Organization’s core conventions
to the Human Rights Act (menneskerettighetsloven).

As can be seen from the link to the case itself, we have an ID within
the URL: “85196”. This is the case ID. We can use the
[`get_vote()`](https://martigso.github.io/stortingscrape/reference/get_vote.md)
function from `stortingscrape` to extract all votes on this case:

``` r

## covid_relief <- get_vote("85196")
```

We now have a data frame with 71 votes over 22 variables. The data
structure for some selected variables, looks like this:

``` r

head(covid_relief[, c("case_id", "vote_id", "n_for", "n_against", "adopted")])
#>   case_id vote_id n_for n_against adopted
#> 1   85196   17631     1        87   false
#> 2   85196   17632     6        81   false
#> 3   85196   17633    14        74   false
#> 4   85196   17634    42        46   false
#> 5   85196   17635    40        48   false
#> 6   85196   17636    15        73   false
```

As we are interested in the result of proposal 217 from the Labor Party,
we can extract the ID of this particular vote from our data:

``` r


covid_relief$vote_id[which(grepl("217", covid_relief$vote_topic))]
```

To get the personal MP vote results for this particular vote, we can use
the
[`get_result_vote()`](https://martigso.github.io/stortingscrape/reference/get_result_vote.md)
function:[^9]

``` r

## covid_relief_result <- get_result_vote("17689")

head(covid_relief_result[, c("vote_id", "mp_id", "party_id", "vote")])
#>   vote_id mp_id party_id          vote
#> 1   17689   SSA        H           mot
#> 2   17689   EAG        H ikke_tilstede
#> 3   17689   PTA      FrP           mot
#> 4   17689   DTA        A ikke_tilstede
#> 5   17689  KAAN       SV           for
#> 6   17689  KAND       Sp           for
```

From looking only at the first six rows of the data, the readers who
know the Norwegian political system will suspect that this vote was an
opposition versus government vote, but we can also easily get the
distribution of votes by party:

``` r


table(covid_relief_result$party_id, 
      covid_relief_result$vote) |>
  addmargins()
#>      
#>       for ikke_tilstede mot Sum
#>   A    27            21   0  48
#>   FrP   0            12  14  26
#>   H     0            22  23  45
#>   KrF   0             5   3   8
#>   MDG   1             0   0   1
#>   R     1             0   0   1
#>   Sp    8            12   0  20
#>   SV    5             6   0  11
#>   Uav   0             0   1   1
#>   V     0             5   3   8
#>   Sum  42            83  44 169
```

As suspected, the vote was divided between the opposition parties A,
MDG, R, Sp, and SV on the one side, and the government parties (H, KrF,
and V) together with FrP on the other, and was not adopted by a thin
margin of 2 votes. Of course, this is a minimal example, but I will
highlight more methods for extracting multiple votes below.

### Sequences of data extraction

Below, I show three examples of sequentially extracting data of
interest.

#### Example 1: From periods to interpellations

Most of the mentioned IDs for Stortinget’s data are not only extractable
from the frontend web-page, but also from the back-end API. These data
can be retrieved by various forms of parliamentary period or session
specific functions in `stortingscrape`. In this section, I will show how
to get all MPs for a specific parliamentary period and all
interpellations for a parliamentary session.

First, however, I note that IDs for periods and sessions are accessed
through two core functions in the package:

``` r

## parl_periods <- get_parlperiods()
## parl_sessions <- get_parlsessions()

tail(parl_periods[,c("id", "years")])
#>         id     years
#> 16 1965-69 1965-1969
#> 17 1961-65 1961-1965
#> 18 1958-61 1958-1961
#> 19 1954-57 1954-1958
#> 20 1950-53 1950-1954
#> 21 1945-49 1945-1950
tail(parl_sessions[,c("id", "years")])
#>         id     years
#> 38 1991-92 1991-1992
#> 39 1990-91 1990-1991
#> 40 1989-90 1989-1990
#> 41 1988-89 1988-1989
#> 42 1987-88 1987-1988
#> 43 1986-87 1986-1987
```

The parliamentary period IDs is mainly used for MP data; Norwegian MPs
are elected for 4 year terms, with no constitutional arrangement for
snap elections. The MP data also stretch way further back in time than
most of the other data in the API:

``` r


parl_periods$id[nrow(parl_periods)]
#> [1] "1945-49"
```

``` r

## mps4549 <- get_parlperiod_mps("1945-49")
head(mps4549[, c("mp_id", "county_id", "party_id", "period_id")])
#>   mp_id county_id party_id period_id
#> 1  AAKU        VA        A   1945-49
#> 2  AARY        AA        A   1945-49
#> 3  ALKJ        He        H   1945-49
#> 4  ALVÅ        Fi      NKP   1945-49
#> 5  AMSK        ST        A   1945-49
#> 6  ANBØ        SF        V   1945-49
```

From these data, the way is short to extracting more rich data on
individual MPs, as will be demonstrated below.

Content data, however, use parliamentary session IDs rather than period
IDs. These functions are standardized to function names as
`get_session_*()`. For example, we can access all interpellations from
the 2002-2003 session with the
[`get_session_questions()`](https://martigso.github.io/stortingscrape/reference/get_session_questions.md)
function:

``` r

## interp0203 <- get_session_questions("2002-2003", q_type = "interpellasjoner")
dim(interp0203)
#> [1] 22 26
```

Here, we have 22 interpellations over 26 different variables.
Unfortunately, the API only gives the question and not the answer for
the different types of question requests. However, interpellations are
debated in the Storting’s meetings, so the minister’s answer can be
found among the speeches in the transcript of the meeting (see Example 3
below).

### Example 2: From cases to MP vote results

Next, I showcase how to go from cases in a session, through extracting a
case of interest and vote results, to vote matrices for that case.

First, I extract all cases in the 2019-2020 session:

``` r

## cases <- get_session_cases("2019-2020")
```

The `cases` object will here contain all cases treated in the 2019-2020
parliamentary session. Do note that `cases` is a list of 4 elements
(`$root`, `$topics`, `$proposers`, and `$spokespersons`). In the
following, I use the case ID in `$root` to access vote information for a
case – in this example the 48th row in the data:[^10]

``` r


# The case titles are, unfortunately, not translated
cases$root$title_short[48]
#> [1] "Representantforslag om å reversere avgiftsøkninger på alkoholfrie drikkevarer og bevare arbeidsplasser i norsk næringsmiddelindustri og varehandel"
```

``` r

## vote <- get_vote(cases$root$id[48])

vote[, c("case_id", "vote_id", 
         "alternative_vote", 
         "n_for", "n_absent", "n_against")]
#>   case_id vote_id alternative_vote n_for n_absent n_against
#> 1   78686   15404               -1     1       82        86
#> 2   78686   15405            15406    46       82        41
#> 3   78686   15406            15405    41       82        46
```

The output gives us a data frame of three votes over 22 variables,
whereof one is the vote ID for each of the votes. We can use this
variable to retrieve rollcall data, using the `get_result_vote`
function:

``` r

## vote_result <- lapply(vote$vote_id, get_result_vote, good_manners = 5)
names(vote_result) <- vote$vote_id

vote_result <- do.call(rbind, vote_result)
head(vote_result[, 3:ncol(vote_result)])
#>         vote_id mp_id party_id          vote permanent_sub_for sub_for
#> 15404.1   15404   SSA        H           mot              <NA>    <NA>
#> 15404.2   15404   EAG        H           mot              <NA>    <NA>
#> 15404.3   15404   PTA      FrP           mot              <NA>    <NA>
#> 15404.4   15404   DTA        A           mot              <NA>    <NA>
#> 15404.5   15404  KAAN       SV           mot              <NA>    <NA>
#> 15404.6   15404   MAA       Sp ikke_tilstede              <NA>    <NA>
```

And make an overall proportion table over party distribution for the
three votes:

``` r

table(vote_result$vote, vote_result$party_id,
      dnn = c("Vote result", "Party")) |>
  prop.table(margin = 2) |>
  round(digits = 2)
```

### Example 3: From debate transcripts to speakers

The debate transcripts (publication type “referat”) contain all speeches
given in the Storting’s meetings, as well as in open hearings and in the
European Committee. The ids of the transcripts in a session can be found
with `get_session_publications(type = "referat")`, and
[`get_speeches()`](https://martigso.github.io/stortingscrape/reference/get_speeches.md)
returns the speeches in a transcript as a data frame with one row per
speech. Here, I use the transcript of the meeting on 13 February 2014:

``` r

## speeches140213 <- get_speeches("s140213")

head(speeches140213[, c("speech_order", "speech_type", "speaker_name",
                        "speaker_party", "chair_name")])
#>   speech_order  speech_type    speaker_name speaker_party        chair_name
#> 1            1  presinnlegg            <NA>          <NA> Olemic Thommessen
#> 2            2 hovedinnlegg   Kjersti Toppe            Sp Olemic Thommessen
#> 3            3  presinnlegg            <NA>          <NA> Olemic Thommessen
#> 4            4  presinnlegg            <NA>          <NA> Olemic Thommessen
#> 5            5  presinnlegg            <NA>          <NA> Olemic Thommessen
#> 6            6 hovedinnlegg Ruth Mari Grung             A Olemic Thommessen
```

Note that the API only identifies the speakers by name in this
transcript. The API gives person ids for the speakers (`person_id`) from
the 2016-2017 session onward, but not before, and the ids it gives are
not always correct:

``` r

table(api_id = !is.na(speeches140213$person_id))
#> api_id
#> FALSE 
#>    48
```

To link the speakers to the rest of the API,
[`get_speeches()`](https://martigso.github.io/stortingscrape/reference/get_speeches.md)
therefore also adds person ids linked from the names of the speakers
(`linked_person_id`) and of the chair (`chair_linked_id`). These come
from the `speaker_links` dataset in the package, which links the names
of speakers and chairs in all transcripts from the 1998-99 session
onward to person ids, by session. The links are made from the names
alone, and names that cannot be linked with certainty are left as `NA`
(see
[`?speaker_links`](https://martigso.github.io/stortingscrape/reference/speaker_links.md)
for the rules):

``` r

head(speeches140213[!is.na(speeches140213$speaker_name),
                    c("speaker_name", "speaker_party", "linked_person_id", "link_method")])
#>          speaker_name speaker_party linked_person_id           link_method
#> 2       Kjersti Toppe            Sp              KJT         full name, mp
#> 6     Ruth Mari Grung             A              RUG first + last name, mp
#> 9  Sveinung Stensland             H             SVES         full name, mp
#> 10   Karianne O. Tung             A              KAT         full name, mp
#> 11   Harald T. Nesvik           FrP              HTN         full name, mp
#> 12      Kjersti Toppe            Sp              KJT         full name, mp
```

All speeches with a named speaker are linked, except the two by Else-May
Botten, whose name at the time differs from the name registered in the
API. Speeches by the president (`presinnlegg`) have no speaker name in
the older transcripts:

``` r

table(speech_type = speeches140213$speech_type,
      linked = !is.na(speeches140213$linked_person_id))
#>               linked
#> speech_type    FALSE TRUE
#>   hovedinnlegg     2   29
#>   presinnlegg     16    0
#>   replikk          0    1
```

However, the sitting chair is given in `chair_name`, and linked in
`chair_linked_id`:

``` r

unique(speeches140213[, c("chair_name", "chair_linked_id")])
#>          chair_name chair_linked_id
#> 1 Olemic Thommessen            OLET
```

The linked ids are made by the package rather than given by the API, and
only cover the sessions in `speaker_links`. Setting `link = FALSE`
returns the speeches without them, and `speaker_links` can then be
merged with the speeches by name and session. With the person ids in
place, the speeches are easily combined with the MP data in the package,
such as
[`get_mp()`](https://martigso.github.io/stortingscrape/reference/get_mp.md),
[`get_mp_bio()`](https://martigso.github.io/stortingscrape/reference/get_mp_bio.md),
and
[`get_parlperiod_mps()`](https://martigso.github.io/stortingscrape/reference/get_parlperiod_mps.md).

## Summary

In this vignette, I have presented the philosophy, scope, usage, and
workflow of the `stortingscrape` package for R. In sum, `stortingscrape`
makes retrieving data from the Norwegian parliament (*Stortinget*) more
accessible through the back-end API
([data.stortinget.no](https://data.stortinget.no)). One core philosophy
of the package is to let the user tailor the data to one’s needs, while
at the same time extracting minimal overlapping data. The scope of the
package ranges from general data on the parliament itself (rules,
session info, committees, etc) to data on the parties, bibliographies of
the MPs, questions, hearings, debates, votes, and more.

## List of functions

A list of all functions and their description can be found in the
package documentation within R or from
[github](https://martigso.github.io/stortingscrape/functions.html).

## Bibliography

Beelen, Kaspar, Timothy Alberdingk Thijm, Christopher Cochrane, et al.
2017. “Digitization of the Canadian Parliamentary Debates.” *Canadian
Journal of Political Science/Revue Canadienne de Science Politique*,
1–16.

Eggers, Andrew C., and Arthur Spirling. 2014. “Electoral Security as a
Determinant of Legislator Activity, 1832–1918: New Data and Methods for
Analyzing British Political Development.” *Legislative Studies
Quarterly* 39 (4): 593–620.

Rauh, Christian, and Jan Schwalbach. 2020. “The ParlSpeech V2 data set:
Full-text corpora of 6.3 million parliamentary speeches in the key
legislative chambers of nine representative democracies.” Version V1.
Harvard Dataverse. <https://doi.org/10.7910/DVN/L4OAKN>.

Thomas, Matt, Bo Pang, and Lillian Lee. 2006. “Get Out the Vote:
Determining Support or Opposition from Congressional Floor-Debate
Transcripts.” *Proceedings of the 2006 Conference on Empirical Methods
in Natural Language Processing*, 327–35.

Turner-Zwinkels, Tomas, Oliver Huwyler, Elena Frech, et al. 2021.
“Parliaments Day-by-Day: A New Open Source Database to Answer the
Question of Who Was in What Parliament, Party, and Party-Group, and
When.” *Legislative Studies Quarterly*.

Wickham, Hadley. 2023. *Httr2: Perform HTTP Requests and Process the
Responses*. <https://httr2.r-lib.org>.

[^1]: See
    [`stortingscrape::get_publication()`](https://martigso.github.io/stortingscrape/reference/get_publication.md)
    for instance

[^2]: See <https://martigso.github.io/stortingscrape/functions.html>

[^3]: `stortingscrape` exclusively works with XML.

[^4]: `stortingscrape::get_mp("MAAA")`

[^5]: Especially, see
    <https://httr2.r-lib.org/articles/wrapping-apis.html>

[^6]: See
    <https://data.stortinget.no/nyhetsoversikt/begrensning-pa-api-kall/>.
    Users with special needs may request an exemption from Stortinget’s
    web team.

[^7]: [stortinget.no](https://stortinget.no) as the ids are embedded in
    the urls.

[^8]: This is done in order to not call the API each time the vignette
    is built.

[^9]: I have not decided if data values should be translated or not. In
    this case, “for” is “for”, “mot” is “against”, and “ikke_tilstede”
    is “absent”.

[^10]: I will note that it is possible to extract vote information on
    all cases by either using the
    [`apply()`](https://rdrr.io/r/base/apply.html) family or control
    flow constructs available in R. However, in this case, calling the
    API 614 (`nrow(cases[["root"]])`) times will require pausing between
    calls (with the `good_manners` argument). This will increase running
    time substantially.
