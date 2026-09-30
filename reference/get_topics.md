# Get list of topics and sub-topics for the Norwegian parliament

A function for retrieving topic keys used to label various data from the
Norwegian parliament.

## Usage

``` r
get_topics(keep_sub_topics = TRUE)
```

## Arguments

- keep_sub_topics:

  Logical. Whether to keep sub-topics (default) for all main topics or
  not.

## Value

With `keep_sub_topics = TRUE` (default), a list with two elements. With
`keep_sub_topics = FALSE`, only the main topics, as a data.frame with
the variables of `$main_topics` below.

1.  **\$topics** (sub-topics, with the id of their main topic)

    |                   |                                                         |
    |-------------------|---------------------------------------------------------|
    |                   |                                                         |
    | **response_date** | Date of data retrieval                                  |
    | **version**       | Data version from the API                               |
    | **is_main_topic** | Logical indicator for whether the topic is a main topic |
    | **main_topic_id** | Id of main topic                                        |
    | **id**            | Id of topic                                             |
    | **name**          | Name of topic                                           |

2.  **\$main_topics** (main topics)

    |                   |                                                         |
    |-------------------|---------------------------------------------------------|
    |                   |                                                         |
    | **response_date** | Date of data retrieval                                  |
    | **version**       | Data version from the API                               |
    | **is_main_topic** | Logical indicator for whether the topic is a main topic |
    | **main_topic_id** | Id of main topic                                        |
    | **id**            | Id of topic                                             |
    | **name**          | Name of topic                                           |

## Examples

``` r
if (FALSE) { # \dontrun{
# Request the data
tops <- get_topics()

# Look at the first main topic
tops$main_topics[1, ]

# Extract all sub-topics for the first main topic
tops$topics[which(tops$topics$main_topic_id == 5), ]
} # }
```
