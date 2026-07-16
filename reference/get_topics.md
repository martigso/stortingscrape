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

A list with two elements:

1.  **\$topics** (All topics)

    |                   |                                                         |
    |-------------------|---------------------------------------------------------|
    |                   |                                                         |
    | **response_date** | Date of data retrieval                                  |
    | **version**       | Data version from the API                               |
    | **is_main_topic** | Logical indicator for whether the topic is a main topic |
    | **main_topic_id** | Id of main topic                                        |
    | **id**            | Id of topic                                             |
    | **name**          | Name of topic                                           |

2.  **\$main_topics** (exclusively main topics, if keep_sub_topics =
    TRUE)

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
