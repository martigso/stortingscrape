suppressMessages(library(rvest))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(readr))

dir.create("./data/representanter/xml/", recursive = TRUE, showWarnings = FALSE)
dir.create("./data/representanter/csv/", recursive = TRUE, showWarnings = FALSE)

# You would want to check whether the "./data/stortingsperioder/csv/stortingsperioder.csv" data is outdated before running this script
stortingsperioder <- read_csv("./data/stortingsperioder/csv/stortingsperioder.csv", col_types = cols())

dls <- gsub(".xml", "", list.files("./data/representanter/xml/"))

new_dls <- (stortingsperioder$id %in% dls) == FALSE

if(identical(character(), stortingsperioder$id[new_dls]) == FALSE){
  lapply(stortingsperioder$id[new_dls], function(y){
    
    download.file(paste0("http://data.stortinget.no/eksport/representanter?stortingsperiodeid=", y), 
                  destfile = paste0("./data/representanter/xml/", y, ".xml"))
    Sys.sleep(2)  
    
  })
  
} else {
  
  message("No new data to download.")
  
}

x <- lapply(list.files("./data/representanter/xml", full.names = TRUE), function(y){
  
  tmp <- read_html(y)
  
  tmp <- tibble(
    respons_dato_tid = tmp %>% html_elements("representant > respons_dato_tid") %>% html_text(),
    versjon = tmp %>% html_elements("representant > versjon") %>% html_text(),
    doedsdato = tmp %>% html_elements("representant > doedsdato") %>% html_text(),
    etternavn = tmp %>% html_elements("representant > etternavn") %>% html_text(),
    foedselsdato = tmp %>% html_elements("representant > foedselsdato") %>% html_text(),
    fornavn = tmp %>% html_elements("representant > fornavn") %>% html_text(),
    id = tmp %>% html_elements("representant > id") %>% html_text(),
    kjoenn = tmp %>% html_elements("representant > kjoenn") %>% html_text(),
    stortingsperiode_id = tmp %>% html_elements("stortingsperiode_id") %>% html_text())
  
  return(tmp)
  
})

x <- bind_rows(x)

write_csv(x, file = "./data/representanter/csv/representanter.csv")

message("\nPreview:\n==========")
head(x)

