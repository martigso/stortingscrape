suppressMessages(library(rvest))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(readr))

dir.create("./data/partier/xml/", recursive = TRUE, showWarnings = FALSE)
dir.create("./data/partier/csv/", recursive = TRUE, showWarnings = FALSE)

sesjoner <- read_csv("./data/sesjoner/csv/sesjoner.csv", col_types = cols())

dls <- gsub(".xml", "", list.files("./data/partier/xml/"))

new_dls <- (sesjoner$id %in% dls) == FALSE

if(identical(character(), sesjoner$id[new_dls]) == FALSE){
  lapply(sesjoner$id[new_dls], function(y){
    
    
    download.file(paste0("http://data.stortinget.no/eksport/partier?sesjonid=", y), 
                  destfile = paste0("./data/partier/xml/", y, ".xml"))
    Sys.sleep(2)  
    
  })

} else {
  
  message("No new data to download.")

}

x <- lapply(list.files("./data/partier/xml", full.names = TRUE), function(y){
  
  tmp <- read_html(y)
  
  tmp <- tibble(
    respons_dato_tid = tmp %>% html_elements("parti > respons_dato_tid") %>% html_text(),
    versjon = tmp %>% html_elements("parti > versjon") %>% html_text(),
    id = tmp %>% html_elements("parti > id") %>% html_text(),
    navn = tmp %>% html_elements("parti > navn") %>% html_text(),
    representert_parti = tmp %>% html_elements("parti > representert_parti") %>% html_text(),
    sesjon_id = tmp %>% html_elements("sesjon_id") %>% html_text(),
    stortingsperiode_id = tmp %>% html_elements("stortingsperiode_id") %>% html_text())
  
return(tmp)
  
})

x <- bind_rows(x)

write_csv(x, file = "./data/partier/csv/partier.csv")



message("\nPreview:\n==========")
head(x)
