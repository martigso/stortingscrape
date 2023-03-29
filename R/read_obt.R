#' Read Oslo-Bergen-Tagger processed files into R
#' 
#' @description A function reading OBT-tagged files 
#' 
#' @usage read_obt(file = NA)
#' 
#' @param file character. Path to OBT-tagged file
#' 
#' 
#' @return A data frame with the following variables:
#' 
#' 
#'    |              |                                      |
#'    |:-------------|:-------------------------------------|
#'    | **sentence** | Sentence number                      |
#'    | **index**    | Token number in sentence             |
#'    | **token**    | Raw token, as read by OBT originally |
#'    | **lwr**      | Lowercase raw token                  |
#'    | **lemma**    | Lemmatized token                     |
#'    | **pos**      | Part of Speech                       |
#'    | **morph**    | Morphological tags                   |
#'    
#' @md
#' 
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' sample_text <- read_obt("./inst/extdata/obt_sample.txt")
#' head(sample_text)
#' }

#' @import stringr
#' 
#' @export

# Reading raw OBT file
read_obt <- function(file = NA){
  
  # Reading the file
  obt_raw <- readLines(file)
  
  # Setting up start and end index based on the format:
  # <word>lever</word>
  # "<lever>"
  #     "leve" verb pres
  seq_a <- seq(1, length(obt_raw)-2, 3)
  seq_b <- seq(3, length(obt_raw), 3)
  
  # Splitting raw OBT into a list of single instances
  # and their tags
  obt_split <- lapply(1:length(seq_a), function(x){
    obt_raw[seq_a[x]:seq_b[x]]
  })
  
  # Converting raw OBT word list to data frame
  obt_struct <- lapply(obt_split, function(x){
    
    # Extracting "as read" token
    token <- x[1] |> 
      str_remove("\\<word\\>") |>
      str_remove("\\<\\/word\\>")
    
    # Extracting lowercase "as read" token
    lwr <- x[2] |>
      str_remove_all('\\"|\\>|\\<')
    
    # Extracting lemmatized token
    lemma <- x[3] |>
      str_trim() |> 
      str_extract_all('\\"(.*)\\"') |>
      str_remove_all('\\"')
    
    # Extracting morphological tags
    morph <- x[3] |>
      str_extract_all('\\"\\s(.*)$') |>
      unlist() |>
      str_remove('\\"') |> 
      str_trim()

    # Extracting parts of speech
    pos <- morph |>
      str_extract("([a-z]+\\-*)+")
    
    # Excluding PoS from morph
    morph <- morph |>
      str_remove("([a-z]+\\-*)+") |> 
      str_trim()
    
    tmp <- data.frame(token,
                      lwr,
                      lemma,
                      pos,
                      morph)
    
    return(tmp)
  })
  
  # Binding all tokens to a data frame
  obt_struct <- do.call(rbind, obt_struct)
  
  # Constructing sentence indication
  obt_struct$sentence <- 1
  
  # Looping over all rows, giving a new sentence when the "$." 
  # pattern is found in the lemma variable.
  for(i in 2:nrow(obt_struct)){
    obt_struct$sentence[i] <- ifelse(obt_struct$lemma[i-1] == "$.",
                                     obt_struct$sentence[i-1] + 1, 
                                     obt_struct$sentence[i-1])
  }
  
  

  # Making a token index variable for each sentence
  obt_struct$index <- by(obt_struct, list(obt_struct$sentence), function(x){
    x$sentence |>
      length() |>
      seq(from = 1, to = _, by = 1)
  }) |> unlist()
  
  # Reordering variables
  obt_struct <- obt_struct[, c("sentence", 
                               "index", 
                               "token", 
                               "lwr", 
                               "lemma",
                               "pos",
                               "morph")]
  return(obt_struct)
}