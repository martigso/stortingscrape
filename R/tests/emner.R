suppressMessages(library(rvest))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(readr))



if(file.exists(c("./data/emner/xml/emner.xml")) == FALSE){
  
  download.file("http://data.stortinget.no/eksport/emner", destfile = "./data/emner/xml/emner.xml")
  
  x <- read_html("./data/emner/xml/emner.xml")
  
  emne <- tibble(
    respons_dato_tid = x %>% html_elements("underemne_liste > emne > respons_dato_tid") %>% html_text(),
    versjon = x %>% html_elements("underemne_liste > emne > versjon") %>% html_text(),
    er_hovedemne = x %>% html_elements("underemne_liste > emne > er_hovedemne") %>% html_text(),
    hovedemne_id = x %>% html_elements("underemne_liste > emne > hovedemne_id") %>% html_text(),
    id = x %>% html_elements("underemne_liste > emne > id") %>% html_text(),
    navn = x %>% html_elements("underemne_liste > emne > navn") %>% html_text())
  
  hovedemne <- tibble(
    respons_dato_tid = x %>% html_elements("emne_liste > emne > respons_dato_tid") %>% html_text(),
    versjon = x %>% html_elements("emne_liste > emne > versjon") %>% html_text(),
    er_hovedemne = x %>% html_elements("emne_liste > emne > er_hovedemne") %>% html_text(),
    hovedemne_id = x %>% html_elements("emne_liste > emne > hovedemne_id") %>% html_text(),
    id = x %>% html_elements("emne_liste > emne > id") %>% html_text(),
    navn = x %>% html_elements("emne_liste > emne > navn") %>% html_text())
  
  
  write_csv(emne, file = "./data/emner/csv/emner.csv")
  write_csv(hovedemne, file = "./data/emner/csv/hovedemner.csv")
  
} else if(file.exists("./data/sesjoner/csv/sesjoner.csv") == FALSE){
  
  message("Data already downloaded but not structured. Structuring..")
  
  x <- read_html("./data/emner/xml/emner.xml")
  
  emne <- tibble(
    respons_dato_tid = x %>% html_elements("underemne_liste > emne > respons_dato_tid") %>% html_text(),
    versjon = x %>% html_elements("underemne_liste > emne > versjon") %>% html_text(),
    er_hovedemne = x %>% html_elements("underemne_liste > emne > er_hovedemne") %>% html_text(),
    hovedemne_id = x %>% html_elements("underemne_liste > emne > hovedemne_id") %>% html_text(),
    id = x %>% html_elements("underemne_liste > emne > id") %>% html_text(),
    navn = x %>% html_elements("underemne_liste > emne > navn") %>% html_text())
  
  hovedemne <- tibble(
    respons_dato_tid = x %>% html_elements("emne_liste > emne > respons_dato_tid") %>% html_text(),
    versjon = x %>% html_elements("emne_liste > emne > versjon") %>% html_text(),
    er_hovedemne = x %>% html_elements("emne_liste > emne > er_hovedemne") %>% html_text(),
    hovedemne_id = x %>% html_elements("emne_liste > emne > hovedemne_id") %>% html_text(),
    id = x %>% html_elements("emne_liste > emne > id") %>% html_text(),
    navn = x %>% html_elements("emne_liste > emne > navn") %>% html_text())
  
  write_csv(emne, file = "./data/emner/csv/emner.csv")
  write_csv(hovedemne, file = "./data/emner/csv/hovedemner.csv")
  
  message("Done.")
  
} else {
  
  message("Data already downloaded and structured. Checking download date...")
  
  emne <- read_csv("./data/emner/csv/emner.csv", col_types = cols())
  
  date <- unique(as.Date(emne$respons_dato_tid))
  
  message(paste("Downloaded:", date))
  
  days <- as.Date(Sys.time()) - date
  
  message(paste("Which is", as.Date(Sys.time()) - date, "days ago."))
  
  if(date > as.Date(paste(as.numeric(format(date, "%Y")) + 1, "10-01", sep = "-")) | days > 365){
    
    message("Data is outdated. Downloading and structuring new version...")
    
    download.file("http://data.stortinget.no/eksport/emner", destfile = "./data/emner/xml/emner.xml")
    
    x <- read_html("./data/emner/xml/emner.xml")
    
    emne <- tibble(
      respons_dato_tid = x %>% html_elements("underemne_liste > emne > respons_dato_tid") %>% html_text(),
      versjon = x %>% html_elements("underemne_liste > emne > versjon") %>% html_text(),
      er_hovedemne = x %>% html_elements("underemne_liste > emne > er_hovedemne") %>% html_text(),
      hovedemne_id = x %>% html_elements("underemne_liste > emne > hovedemne_id") %>% html_text(),
      id = x %>% html_elements("underemne_liste > emne > id") %>% html_text(),
      navn = x %>% html_elements("underemne_liste > emne > navn") %>% html_text())
    
    hovedemne <- tibble(
      respons_dato_tid = x %>% html_elements("emne_liste > emne > respons_dato_tid") %>% html_text(),
      versjon = x %>% html_elements("emne_liste > emne > versjon") %>% html_text(),
      er_hovedemne = x %>% html_elements("emne_liste > emne > er_hovedemne") %>% html_text(),
      hovedemne_id = x %>% html_elements("emne_liste > emne > hovedemne_id") %>% html_text(),
      id = x %>% html_elements("emne_liste > emne > id") %>% html_text(),
      navn = x %>% html_elements("emne_liste > emne > navn") %>% html_text())
    
    write_csv(emne, file = "./data/emner/csv/emner.csv")
    write_csv(hovedemne, file = "./data/emner/csv/hovedemner.csv")
    
  }
}

message("\nPreview:\n==========")
head(emne)