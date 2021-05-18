suppressMessages(library(rvest))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(readr))



if(file.exists(c("./data/fylker/xml/fylker.xml")) == FALSE){
  
  download.file("http://data.stortinget.no/eksport/fylker", destfile = "./data/fylker/xml/fylker.xml")
  
  x <- read_html("./data/fylker/xml/fylker.xml")
  
  x <- tibble(
    respons_dato_tid = x %>% html_elements("fylke > respons_dato_tid") %>% html_text(),
    versjon = x %>% html_elements("fylke > versjon") %>% html_text(),
    historisk_fylke = x %>% html_elements("fylke > historisk_fylke") %>% html_text(),
    id = x %>% html_elements("fylke > id") %>% html_text(),
    navn = x %>% html_elements("fylke > navn") %>% html_text())
  
  
  write_csv(x, file = "./data/fylker/csv/fylker.csv")

} else if(file.exists("./data/sesjoner/csv/sesjoner.csv") == FALSE){
  
  message("Data already downloaded but not structured. Structuring..")
  
  x <- read_html("./data/fylker/xml/fylker.xml")
  
  x <- tibble(
    respons_dato_tid = x %>% html_elements("fylke > respons_dato_tid") %>% html_text(),
    versjon = x %>% html_elements("fylke > versjon") %>% html_text(),
    historisk_fylke = x %>% html_elements("fylke > historisk_fylke") %>% html_text(),
    id = x %>% html_elements("fylke > id") %>% html_text(),
    navn = x %>% html_elements("fylke > navn") %>% html_text())
  
  write_csv(x, file = "./data/fylker/csv/fylker.csv")
  
  message("Done.")
  
} else {
  
  message("Data already downloaded and structured. Checking download date...")
  
  x <- read_csv("./data/fylker/csv/fylker.csv", col_types = cols())
  
  date <- unique(as.Date(x$respons_dato_tid))
  
  message(paste("Downloaded:", date))
  
  days <- as.Date(Sys.time()) - date
  
  message(paste("Which is", as.Date(Sys.time()) - date, "days ago."))
  
  if(date > as.Date(paste(as.numeric(format(date, "%Y")) + 1, "10-01", sep = "-")) | days > 365){
    
    message("Data is outdated. Downloading and structuring new version...")
    
    download.file("http://data.stortinget.no/eksport/fylker", destfile = "./data/fylker/xml/fylker.xml")
    
    x <- read_html("./data/fylker/xml/fylker.xml")
    
    x <- tibble(
      respons_dato_tid = x %>% html_elements("fylke > respons_dato_tid") %>% html_text(),
      versjon = x %>% html_elements("fylke > versjon") %>% html_text(),
      historisk_fylke = x %>% html_elements("fylke > historisk_fylke") %>% html_text(),
      id = x %>% html_elements("fylke > id") %>% html_text(),
      navn = x %>% html_elements("fylke > navn") %>% html_text())
    
    write_csv(x, file = "./data/fylker/csv/fylker.csv")

  }
}

message("\nPreview:\n==========")
head(x)