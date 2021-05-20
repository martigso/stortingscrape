# 845636337385-kb9gan02mljap0s58h5b2tg10bpomib4.apps.googleusercontent.com
# fpPtT-WuhNm-Ad75EevLgB6L

library(RoogleVision)

options("googleAuthR.client_id" = "845636337385-kb9gan02mljap0s58h5b2tg10bpomib4.apps.googleusercontent.com")
options("googleAuthR.client_secret" = "fpPtT-WuhNm-Ad75EevLgB6L")

options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))

googleAuthR::gar_auth(email = "martin.g.soyland@gmail.com")


files <- list.files("./data/personbilde/jpeg/middels", full.names = TRUE)

set.seed(329401)

o <- list()

for(i in sample(1:length(files), 20)){
  o[[i]] <- getGoogleVisionResponse(files[i], feature = "FACE_DETECTION")
  Sys.sleep(2)
}

library(dplyr)

test <- bind_rows(o)
%>% %>% %>% %>% %>% %>% %>% 