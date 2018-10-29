library(rvest)
library(tidyverse)

#### Create one folder with two subfolders: one for the pdfs, one for the cleaned data

#### Connect to Google drive folder through R using library(googledrive)


#### Write an R script that will:
#### 1. Identify all of the Docket pdfs on https://www.ok.gov/ppb/Dockets_and_Results/index.html
#### 2. Identify the new dockets 
#### 3. Download the new pdfs 
#### 4. Clean the pdf
#### 5. Export the new data into a .csv in the cleaned folder

d <- read_html("https://www.ok.gov/ppb/Dockets_and_Results/index.html") %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  as.tibble %>%
  filter(str_detect(value, "Parole Docket")) %>%
  mutate(link = )

##### Get a list of all filenames of blotter PDFs ####
pdflist <- drive_ls("Jail blotters")
pdflist <- pdflist$name
blotters <- list.files("OKCO Blotters")

new_pdfs <- pdflist[!pdflist %in% blotters]

for (i in 1:length(new_pdfs)) {
  drive_download(new_pdfs[i], path = paste0("OKCO Blotters/", new_pdfs[i]), overwrite = TRUE)
}

##### Loop through list of PDFs: read, separate pages of each PDF, and join them into a blank dataframe ('blot'), 
##### rename single column to "text" ####
blotters <- list.files("OKCO Blotters")

blot <- tibble()
for (i in 1:length(blotters)) {
  text <- pdf_text(paste0("OKCO Blotters/", blotters[i]))
  t <- data.frame(matrix(unlist(text)))
  print(paste0("File ", i, " of ", length(blotters), " read"))
  blot <- bind_rows(blot, t)
}