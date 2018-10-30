library(rvest)
library(tidyr)
library(selectr)
library(tibble)
library(stringr)
library(dplyr)
library(googledrive)
library(rvest)
library(XML)
library(RCurl)

#### Create one folder with two subfolders: one for the pdfs, one for the cleaned data

#### Connect to Google drive folder through R using library(googledrive)

#### Write an R script that will:
#### 1. Identify all of the Docket pdfs on https://www.ok.gov/ppb/Dockets_and_Results/index.html

pdfs <- read_html("https://www.ok.gov/ppb/Dockets_and_Results/index.html") %>%
  html_nodes("a") %>%
  html_attr("href") %>%

download.file(d[i,1] %>% as.character, destfile = paste0(d[i,2], ".pdf"))

##placing new PDFs in Google Drive
for (i in 1:nrow(new_pdfs)) {
  drive_upload(new_pdfs[i,1] %>% as.character, path = "Parole Data/Docket and Results PDF's/", name = new_pdfs_names[i], overwrite = TRUE, type = "pdf")
}

##### Loop through list of PDFs: read, separate pages of each PDF, and join them into a blank dataframe ('blot'), 
##### rename single column to "text" ##
blotters <- list.files("OKCO Blotters")

blot <- tibble()
for (i in 1:length(blotters)) {
  text <- pdf_text(paste0("OKCO Blotters/", blotters[i]))
  t <- data.frame(matrix(unlist(text)))
  print(paste0("File ", i, " of ", length(blotters), " read"))
  blot <- bind_rows(blot, t)
}



