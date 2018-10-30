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
#### 2. Identify the new dockets 
#### 3. Download the new pdfs 
#### 4. Clean the pdf
#### 5. Export the new data into a .csv in the cleaned folder

d <- read_html("https://www.ok.gov/ppb/Dockets_and_Results/index.html") %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  as.tibble() %>%
  filter(str_detect(value, "January|February|March|April|May|June|July|August|September|October|November|December|Parole Docket")) %>%
  filter(!str_detect(value, "Commutation|Pardon"))

d$value <- paste0("https://www.ok.gov", d$value)

d$name <- str_extract(d$value, "(?<=documents/).*?(?=.pdf)") %>%
  str_remove("Parole")

d$value <- str_replace_all(d$value, " ", "%20")

##### Get a list of all filenames of blotter PDFs ####
pdflist <- str_squish(d$name) ##new
blotters <- drive_ls("Parole Data/Docket and Results PDF's")
blotters$name <- str_remove(blotters$name, ".pdf") ##old

'%!in%' <- function(x,y)!('%in%'(x,y))

new_pdfs <- as.tibble(d$value[pdflist %!in% blotters$name])
new_pdfs_name <- d$name[pdflist %!in% blotters$name]
new_pdfs_name

##placing new PDFs in Google Drive
for (i in 1:length(new_pdfs)) {
  drive_upload(new_pdfs[i], path = "Parole Data/Docket and Results PDF's/", name = new_pdfs_names[i], overwrite = TRUE, type = "pdf")
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