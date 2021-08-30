rm(list = ls())
library(tidyverse)
library(stortingscrape)
library(glue)
library(DT)

package_names <- "stortingscrape"

stortingscrape_functions <- map_dfr(package_names, function(package_name) {
  tibble(
    Package = package_name,
    Function = as.character(lsf.str(glue("package:{Package}"))),
    Arguments = map_chr(Function, function(function_name) {
      capture.output(str(get(function_name)))[1] %>% 
        str_squish() %>% 
        str_c(collapse = " ") %>% 
        str_remove("^function ")
    })
  ) 
})

Rd_get_metadata <- tools:::.Rd_get_metadata
Rd_contents <- tools:::Rd_contents
Rd_get_example_code <- tools:::.Rd_get_example_code
Rd_get_section <- tools:::.Rd_get_section
Rd_get_text <- tools:::.Rd_get_text
tools:::.Rd_get_value
# Extracts the text of the named section from the rd_doc
Rd_get_section_text <- function(rd_doc, section) {
  Rd_get_section(rd_doc, section) %>% 
    Rd_get_text() %>% 
    discard(~ .x == "")
}

rd_info <- map_dfr(package_names, function(package_name) {
  # A list with the parsed package documentation
  rd_list <- tools::Rd_db(package_name)
  rd_list %>% 
    # Turn the documentation contents into a data frame, one doc page per row.
    Rd_contents() %>% 
    as_tibble() %>% 
    # Remove all documentation of datasets and "internal" functions. 
    # We need to use map_lgl here as Keywords is a list of character vectors.
    filter(map_lgl(Keywords, 
                   ~ length(.x) == 0 || ! .x %in% c("datasets", "internal")
    )) %>%
    select(File, Name, Title, Aliases, Keywords) %>% 
    mutate(Package = package_name) %>% 
    # For each row/doc page we're going to extract some information
    rowwise() %>% 
    mutate(
      rd_doc = list(rd_list[[File]]),
      # The function examples. We're using the paste0("", x) trick as x might
      # be a 0-length vector, but we still want to get back an empty (1-length)
      # character vector.
      Examples = str_trim(paste0("", Rd_get_example_code(rd_doc))),
      Description = paste(Rd_get_section_text(rd_doc, "description"), collapse = " "),
      # A single doc page can document many function we here make a list of
      #  all functions documented by the doc page.
      names_and_aliases = list(unique(c(Name, Aliases)))
    ) %>%
    # A page can document many functions and this unnest will get us one row
    # per function instead of one row per doc page. All row values, except
    # those in names_and_aliases, will be duplicated.
    unnest(names_and_aliases)
})

stortingscrape_functions_info <- inner_join(
  stortingscrape_functions, rd_info,
  by = c("Package" = "Package", "Function" = "names_and_aliases")
)

stortingscrape_functions_info %>% 
  select(Package, Function, Arguments, Title, Description) %>% 
  # Show a datatable with five visible rows
  datatable(options = list(pageLength = 5))

stortingscrape_functions_table <- stortingscrape_functions_info %>% 
  select(Function, Arguments, Title, Description) %>% 
  # Show a datatable with five visible rows
  datatable(options = list(pageLength = 100))

saveWidget(stortingscrape_functions_table, 
           file = "./functions.html")
