suppressMessages(library(rvest))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(readr))

dir.create("./data/komiteer/xml/", recursive = TRUE, showWarnings = FALSE)
dir.create("./data/komiteer/csv/", recursive = TRUE, showWarnings = FALSE)

# You would want to check whether the "./data/sesjoner/csv/sesjoner.csv" data is outdated before running this script
sesjoner <- read_csv("./data/sesjoner/csv/sesjoner.csv", col_types = cols())

dls <- gsub(".xml", "", list.files("./data/komiteer/xml/"))

new_dls <- (sesjoner$id %in% dls) == FALSE

if(identical(character(), sesjoner$id[new_dls]) == FALSE){
  lapply(sesjoner$id[new_dls], function(y){
    
    
    download.file(paste0("http://data.stortinget.no/eksport/komiteer?sesjonid=", y), 
                  destfile = paste0("./data/komiteer/xml/", y, ".xml"))
    Sys.sleep(2)  
    
  })
  
} else {
  
  message("No new data to download.")
  
}

x <- lapply(list.files("./data/komiteer/xml", full.names = TRUE), function(y){
  
  tmp <- read_html(y)
  
  tmp <- tibble(
    respons_dato_tid = tmp %>% html_elements("komite > respons_dato_tid") %>% html_text(),
    versjon = tmp %>% html_elements("komite > versjon") %>% html_text(),
    id = tmp %>% html_elements("komite > id") %>% html_text(),
    navn = tmp %>% html_elements("komite > navn") %>% html_text(),
    sesjon_id = tmp %>% html_elements("sesjon_id") %>% html_text())
  
  return(tmp)
  
})

x <- bind_rows(x)

write_csv(x, file = "./data/komiteer/csv/komiteer.csv")



message("\nPreview:\n==========")
head(x)

