suppressMessages(library(rvest))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(readr))

if(file.exists(c("./data/sesjoner/xml/sesjoner.xml")) == FALSE){
  
  download.file("http://data.stortinget.no/eksport/sesjoner", destfile = "./data/sesjoner/xml/sesjoner.xml")
  
  x <- read_html("./data/sesjoner/xml/sesjoner.xml")
  
  x <- tibble(id = x %>% html_elements("sesjon > id") %>% html_text(),
              fra = x %>% html_elements("sesjon > fra") %>% html_text() %>% as.Date(),
              til = x %>% html_elements("sesjon > til") %>% html_text() %>% as.Date(),
              versjon = x %>% html_elements("sesjon > versjon") %>% html_text(),
              respons_dato_tid = x %>% html_elements("sesjon > respons_dato_tid") %>% html_text())
  
  x$id2 <- x$id
  x$id2[which(nchar(x$id) == 7)] <- str_replace_all(x$id[which(nchar(x$id) == 7)], "\\-", "\\-19")
  
  x <- x %>% select(id, id2, fra, til, versjon, respons_dato_tid)
  
  write_csv(x, file = "./data/sesjoner/csv/sesjoner.csv")
  
} else if(file.exists("./data/sesjoner/csv/sesjoner.csv") == FALSE){
  
  message("Data already downloaded but not structured. Structuring..")
  
  x <- read_html("./data/sesjoner/xml/sesjoner.xml")
  
  x <- tibble(id = x %>% html_elements("sesjon > id") %>% html_text(),
              fra = x %>% html_elements("sesjon > fra") %>% html_text() %>% as.Date(),
              til = x %>% html_elements("sesjon > til") %>% html_text() %>% as.Date(),
              versjon = x %>% html_elements("sesjon > versjon") %>% html_text(),
              respons_dato_tid = x %>% html_elements("sesjon > respons_dato_tid") %>% html_text())
  
  x$id2 <- x$id
  x$id2[which(nchar(x$id) == 7)] <- str_replace_all(x$id[which(nchar(x$id) == 7)], "\\-", "\\-19")
  
  x <- x %>% select(id, id2, fra, til, versjon, respons_dato_tid)
  
  write_csv(x, file = "./data/sesjoner/csv/sesjoner.csv")
  message("Done.")

} else {
  message("Data already downloaded and structured. Checking download date...")
  
  x <- read_csv("./data/sesjoner/csv/sesjoner.csv", col_types = cols())
  
  date <- unique(as.Date(x$respons_dato_tid))
  
  message(paste("Downloaded:", date))
  
  days <- as.Date(Sys.time()) - date
  message(paste("Which is", as.Date(Sys.time()) - date, "days ago."))
  
  if(date > as.Date(paste(as.numeric(format(date, "%Y")) + 1, "10-01", sep = "-")) | days > 365){
    message("Data is outdated. Downloading and structuring new version...")
    download.file("http://data.stortinget.no/eksport/sesjoner", destfile = "./data/sesjoner/xml/sesjoner.xml")
    
    x <- read_html("./data/sesjoner/xml/sesjoner.xml")
    
    x <- tibble(id = x %>% html_elements("sesjon > id") %>% html_text(),
                fra = x %>% html_elements("sesjon > fra") %>% html_text() %>% as.Date(),
                til = x %>% html_elements("sesjon > til") %>% html_text() %>% as.Date(),
                versjon = x %>% html_elements("sesjon > versjon") %>% html_text(),
                respons_dato_tid = x %>% html_elements("sesjon > respons_dato_tid") %>% html_text())
    
    x$id2 <- x$id
    x$id2[which(nchar(x$id) == 7)] <- str_replace_all(x$id[which(nchar(x$id) == 7)], "\\-", "\\-19")
    
    x <- x %>% select(id, id2, fra, til, versjon, respons_dato_tid)
    
    write_csv(x, file = "./data/sesjoner/csv/sesjoner.csv")
  }
}

message("\nPreview:\n==========")
head(x)