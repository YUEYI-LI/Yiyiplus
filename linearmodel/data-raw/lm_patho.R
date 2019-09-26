## code to prepare `lm_patho` dataset goes here
### Add a data.frame called lm_patho
library(usethis)
library(RCurl)
lm_patho <- utils::read.csv(text=RCurl::getURL("https://raw.githubusercontent.com/BIS557/homework-1-starter-code/master/lm_patho.csv"), header = T)
write.csv(lm_patho, "data-raw/lm_patho.csv")
usethis::use_data("lm_patho")
