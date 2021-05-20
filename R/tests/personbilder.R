suppressMessages(library(rvest))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(readr))

# dir.create("./data/personbilde/jpeg/lite", recursive = TRUE, showWarnings = FALSE)
dir.create("./data/personbilde/jpeg/middels", recursive = TRUE, showWarnings = FALSE)
#dir.create("./data/personbilde/jpeg/stort", recursive = TRUE, showWarnings = FALSE)

# You would want to check whether the "./data/stortingsperioder/csv/stortingsperioder.csv" data is outdated before running this script
representanter <- read_csv("./data/representanter/csv/representanter.csv", col_types = cols()) %>% 
  filter(id %in% c("HAOF", "LEST") == FALSE)


dls <- gsub(".jpeg", "", list.files("./data/personbilde/jpeg/middels"))
dls <- dls[which(dls %in% c("HAOF", "LEST") == FALSE)]

new_dls <- (representanter$id %in% dls) == FALSE


if(identical(character(), representanter$id[new_dls]) == FALSE){
  
  lapply(unique(representanter$id[new_dls]), function(y){
    
    download.file(paste0("http://data.stortinget.no/eksport/personbilde?personid=", y, "&storrelse=middels"), 
                  destfile = paste0("./data/personbilde/jpeg/middels/", y, ".jpeg"), quiet = TRUE, )
    
    Sys.sleep(2)
    
  })
  
} else {
  
  message("No new data to download.")
  
}

