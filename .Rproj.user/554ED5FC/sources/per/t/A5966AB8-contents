library(utils)
library(pdftools)
library(stringr)
library(stringi)
library(batman)
library(tidyr)
library(dplyr)
library(lubridate)

##Downloading files - docket and results
##Docket
parole_docket <- pdftools::pdf_text("~/Desktop/Oklahoma Policy Institute/Research Projects/Probation and Parole/Docket and Results PDFs/July 2018 Docket.pdf")

parole_docket <- parole_docket %>%
  trimws() %>%
  as.data.frame()
colnames(parole_docket)[colnames(parole_docket)=="."] <- "info"

##Results
 
parole_results <- pdftools::pdf_text("~/Desktop/Oklahoma Policy Institute/Research Projects/Probation and Parole/Docket and Results PDFs/July 2018 Results.pdf")

##Splitting rows by case
separated_rows <- separate(parole_docket, info, into = c("info2", "info3"), sep = "\n\\(\\d{1,3}\\)\n(?=\\D*,)", 
                           convert = FALSE, extra = "merge")

separated_rows <- separated_rows %>%
  gather(ctno, info) %>%
  select(-ctno) %>%
  filter(!is.na(info))

parole_docket <- data.frame(info=separated_rows)

parole_docket$info <- str_squish(parole_docket$info)

##Name
parole_docket$name <- str_extract(parole_docket$info, "^.*?(?=Docket)") %>%
  str_to_upper() %>%
  trimws()

parole_docket <- parole_docket %>%
  arrange(name)

##Month - add month
parole_docket$month <- as.Date("2018-07-01")

##Personal Appearance
parole_docket$personal_appearance <- str_extract(parole_docket$info, "(?<=Appearance: )\\D|\\d{2,3}?(?=\\d{4,12})") %>%
  to_logical(parole_docket$personal_appearance, language="en")

##DOC Num
parole_docket$docnum <- as.character(str_extract(parole_docket$info, "\\d{5,6}"))

##Type
parole_docket$type <- str_extract(parole_docket$info, "(?<=Type: ).*?(?=OFFENSE)") %>%
  str_to_upper() %>%
  trimws()

parole_docket$type <- str_replace(parole_docket$type, "SER", "SIR")

##County
parole_docket$county <- str_extract(parole_docket$info, "(?<=OFFENSE).*?(?=County|Authority)") %>%
  str_to_upper() %>%
  str_remove("#") %>%
  trimws()

##Authority
parole_docket$info[1]
parole_docket$authority <- str_extract(parole_docket$info, "(?<=Authority).*?(?=Count )") %>%
  str_remove("Last Board") %>%
  str_remove("Condsideration:") %>%
  str_remove("Consideration:") %>%
  str_remove(":") %>%
  str_to_upper() %>%
  str_squish()

parole_docket <- parole_docket %>%
  mutate(authority = ifelse(str_detect(authority, "1/3"), "1/3", 
                            ifelse(str_detect(authority, "THREE YEARS"), "3 YEARS",
                                   ifelse(str_detect(authority, "ANNUAL"), "ANNUAL",
                                          authority))))

##Main Charge
parole_docket$charge_1 <- str_extract(parole_docket$info, "(?<=Count \\d{1,2},).*?(?=Jail)") %>%
  str_remove("(?<=CF: )\\d{2,4}-\\d{1,4}") %>%
  str_remove("CF:") %>%
  str_remove("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>%
  str_remove("None") %>%
  str_remove("Denied") %>%
  str_remove("Waived") %>%
  str_remove("Recommended/Withdrawn") %>%
  str_remove("Stricken") %>%
  str_remove_all(",") %>%
  str_remove("\\(\\d{1,3}\\)") %>%
  str_to_upper() %>%
  str_squish()


##Casenum
parole_docket$casenum <- str_extract(parole_docket$info, "(?<=CF: )\\d{2,4}-\\d{1,4}")
parole_docket$casenum <- paste("CF", parole_docket$casenum, sep="-")

##Jail Time
parole_docket$jail_time <- str_extract(parole_docket$info, "(?<=Next Board Consideration: ).*?(?=days)") %>%
  as.double()

##Dates
parole_docket$dates <- str_extract(parole_docket$info, "(?<=days).*?(?=,)") %>%
  trimws()

parole_docket <- parole_docket %>%
  separate(dates, into = c("reception", "projected_release", "next_consideration"), sep = " ", remove = TRUE, convert = FALSE, fill = "right")

parole_docket$reception <- parole_docket$reception %>%
  str_remove(",") %>%
  mdy()

parole_docket$projected_release <- parole_docket$projected_release %>%
  str_remove(",") %>%
  mdy()

parole_docket$next_consideration <- parole_docket$next_consideration %>%
  str_remove(",") %>%
  mdy()


##Sentence
parole_docket$sentence <- str_extract(parole_docket$info, "(?<=Sentence: ).*(?=Concurrent)") %>%
  trimws() %>%
  str_to_upper()

##Concurrent Charges
parole_docket$concurrent_charges <- str_extract(parole_docket$info, "(?<=Cases:).*?(?=Consecutive)") %>%
  trimws() %>%
  str_remove_all("County")

parole_docket <- parole_docket %>%
  separate(concurrent_charges, into = paste("concurrent_charge", 1:30, sep = "_"), sep = " Count", remove = TRUE, convert = FALSE, fill = "right")

##Consecutive Charges
parole_docket$consecutive_charges <- str_extract(parole_docket$info, "(?<=Consecutive Cases: ).*?(?=Detainer:)") 

parole_docket$consecutive_charges <- str_remove_all(parole_docket$consecutive_charges, "County")  

parole_docket <- parole_docket %>%
  separate(consecutive_charges, into = paste("consecutive_charge", 1:30, sep= "_"), sep = " Count", remove = TRUE, convert = FALSE, fill = "right") 

##Detainer
parole_docket$detainer <- str_extract(parole_docket$info, "(?<=Detainer: ).*?(?=$)") %>%
  str_to_upper() %>%
  str_remove("\\(\\d{1,3}\\)")

##Results
parole_results <- paste(parole_results, sep = ",", collapse = ",")
parole_results <- strsplit(parole_results, "Detainer:") 
parole_results <- unlist(parole_results)
parole_results <- parole_results[-c(1)]
parole_results <- str_replace_all(parole_results, "\n", "%")
parole_results <- str_remove(parole_results, "(?<=Docket).*")
parole_results <- str_extract(parole_results, "(?<=\\(\\d{1,3}\\)|%Not Applicable%).*?(?=%,|$)")
parole_results <- str_squish(parole_results)
parole_results <- ifelse(str_detect(parole_results, "% "), str_remove(parole_results, "%"), parole_results)
parole_results <- str_remove(parole_results, "(?<=%).*")
parole_results <- str_remove(parole_results, "%")
parole_results <- str_remove(parole_results, "\\(\\d{1,3}\\)")
parole_results <- trimws(parole_results)

parole_docket$results <- parole_results
parole_docket$outcome <- parole_results

parole_docket <- parole_docket %>%
  mutate(outcome = ifelse(str_detect(outcome, "RECOMMENDED TO THE GOVERNOR|RECOMMEND TO THE GOVERNOR"), "PAROLE RECOMMENDED TO THE GOVERNOR",
                          ifelse(str_detect(outcome, "SUBSTANCE ABUSE|WORK RELEASE|TREATMENT|PROGRAM|MENTAL HEALTH|SOBER LIVING|COUNSELING|SAT|UAS|UA'S|SUBSTANCE"), "GRANTED WITH PROGRAM CONDITION",
                                 ifelse(str_detect(outcome, "CONTINUED"), "CONTINUED",
                                        ifelse(str_detect(outcome, "PAROLE DENIED|DENIED &|PAROLLE DENIED|PAROE DENIED|PAROLE DENEID|PAROLE DEINED|PAROLED DENIED"), "PAROLE DENIED",
                                               ifelse(str_detect(outcome, "CONSECUTIVE"), "PAROLE GRANTED TO CONSECUTIVE CHARGE",
                                                      ifelse(str_detect(outcome, "RECOMMENDED TO THE DETAINER"), "PAROLE RECOMMENDED TO DETAINER",
                                                             ifelse(str_detect(outcome, "DETAINER"), "PAROLE GRANTED TO DETAINER",
                                                                    ifelse(str_detect(outcome, "PASS"), "PASSED TO STAGE II",
                                                                           ifelse(str_detect(outcome, "WAIVED"), "WAIVED",
                                                                                  ifelse(str_detect(outcome, "STRICKEN"), "STRICKEN",
                                                                                         ifelse(str_detect(outcome, "DECEASED"), "DECEASED",
                                                                                                ifelse(str_detect(outcome, "RECOMMEND|PAROLE RECOMMENED"), "PAROLE RECOMMENDED",
                                                                                                       ifelse(str_detect(outcome, "DISCHARGE|DISCHARGED|DISHCARGED"), "DISCHARGED",
                                                                                                              ifelse(str_detect(outcome, "RECOMMENDED TO COMMUTE|COMMUTATION RECOMMENDED|RECOMMEND COMMUTE|RECOMMENDATION TO COMMUTE|RECOMMENDED COMMUTE|RECOMMENED COMMUTE|RECOMMEND TO COMMUTE"), "RECOMMENDED COMMUTATION",
                                                                                                                     ifelse(str_detect(outcome, "COMMUATATION DENIED"), "COMMUTATION DENIED",
                                                                                                                            ifelse(str_detect(outcome, "RECOMMEND MEDICAL|RECOMMENDED MEDICAL|RECOMMNED MEDICAL|RECOMMENDED FOR MEDICAL|RECOMMENED MEDICAL"), "MEDICAL RECOMMENDED",
                                                                                                                                   ifelse(str_detect(outcome, "STAGE II"), "PASSED TO STAGE II",
                                                                                                                                          ifelse(str_detect(outcome, "DUPLICATE"), "DUPLICATE",
                                                                                                                                   outcome)))))))))))))))))))


##Cleaning
parole_docket <- data.frame(lapply(parole_docket, trimws), stringsAsFactors = FALSE)
parole_docket <- data.frame(lapply(parole_docket, str_to_upper), stringsAsFactors = FALSE)

##Concurrent charges
concurrent_charges <- parole_docket %>%
  select(name, docnum, type, charge_1, casenum, contains("concurrent"), month) %>%
  gather(ctno, concurrent_charges, contains("concurrent")) %>%
  select(-ctno) %>%
  filter(!is.na(concurrent_charges)) %>%
  filter(concurrent_charges != "")

##Consecutive charges
consecutive_charges <- parole_docket %>%
  select(name, docnum, type, charge_1, casenum, contains("consecutive"), month) %>%
  gather(ctno, consecutive_charges, contains("consecutive")) %>%
  select(-ctno) %>%
  filter(!is.na(consecutive_charges)) %>%
  filter(consecutive_charges != "")

##Removing consecutive and concurrent
parole_docket <- parole_docket %>%
  select(-contains("concurrent"), -contains("consecutive"))

##Program type 
programs <- which(parole_docket$outcome == "GRANTED WITH PROGRAM CONDITION")
parole_docket$results[programs]

parole_docket$program_type <- NA

parole_docket$program_type[programs] <- str_extract(parole_docket$results[programs], "(?<=WITH).*$") %>%
  trimws()

parole_docket$results <- NULL

##write csv
write.csv(parole_docket, "/Users/annarouw/Desktop/Oklahoma Policy Institute/Research Projects/Probation and Parole/Actually Cleaned CSV's/July 2018.csv")

write.csv(concurrent_charges, "/Users/annarouw/Desktop/Oklahoma Policy Institute/Research Projects/Probation and Parole/Concurrent/July 2018.csv")

write.csv(consecutive_charges, "/Users/annarouw/Desktop/Oklahoma Policy Institute/Research Projects/Probation and Parole/Consecutive/July 2018.csv")

##Adding files together

setwd("~/Desktop/Oklahoma Policy Institute/Research Projects/Probation and Parole/Actually Cleaned CSV's")
files <- list.files(pattern = ".csv")

datalist <- list()

for (i in 1:length(files)) {
  data <- read.csv(files[i])
  datalist[[i]] <- data
}

parole <- do.call(bind_rows, datalist)

##Concurrent

setwd("~/Desktop/Oklahoma Policy Institute/Research Projects/Probation and Parole/Concurrent")
conc_files <- list.files(pattern = ".csv")

conc_datalist <- list()

for (i in 1:length(conc_files)) {
  data <- read.csv(conc_files[i])
  conc_datalist[[i]] <- data
}

concurrent <- do.call(bind_rows, conc_datalist)

##Consecutive

setwd("~/Desktop/Oklahoma Policy Institute/Research Projects/Probation and Parole/Consecutive")
cons_files <- list.files(pattern = ".csv")

cons_datalist <- list()

for (i in 1:length(cons_files)) {
  data <- read.csv(cons_files[i])
  cons_datalist[[i]] <- data
}

consecutive <- do.call(bind_rows, cons_datalist)

rm(conc_datalist, conc_files, cons_datalist, cons_files, files, i, datalist, data)

##Detainer
parole$detainer <- ifelse(parole$detainer == "NOT APPLICABLE", FALSE, TRUE) 

##Year
parole$year <- as.numeric(str_extract(parole$month, "^\\d{4}"))

##Violent v. Non-violent

parole$violent <- grepl("MANSLAUGHTER|BATTERY|ARSON|ASSAULT|A&B|KIDNAPPING|MURDER|SHOOTING|ROBBERY|ABUSE|FLUID|RAPE|HIT AND|SODOMY|MOLEST|PORN", parole$charge_1)

##Adding consecutive and concurrent charges

concurrent <- concurrent %>%
  group_by(docnum, month) %>%
  mutate(n = n())

no_concurrent <- which(con$concurrent_charges == "NOT APPLICABLE")

concurrent$n[no_concurrent] <- 0

n_concurrent <- concurrent %>%
  select(month, docnum, n)

consecutive <- consecutive %>%
  group_by(docnum, month) %>%
  mutate(n = n())

no_consecutive <- which(consecutive$consecutive_charges == "NOT APPLICABLE")

consecutive$n[no_consecutive] <- 0

n_consecutive <- consecutive %>%
  select(month, docnum, n)

parole1 <- full_join(parole, n_concurrent, by=c("docnum","month"))
parole1 <- unique(parole1)
names(parole1)[names(parole1) == 'n'] <- 'n_concurrent'

parole1 <- full_join(parole1, n_consecutive, by=c("docnum","month"))
parole1 <- unique(parole1)
names(parole1)[names(parole1) == 'n'] <- 'n_consecutive'

parole1$n_concurrent[which(is.na(parole1$n_concurrent))] <- 0
parole1$n_consecutive[which(is.na(parole1$n_consecutive))] <- 0

##Making sentence numeric
sentence <- str_extract(parole1$sentence, ".*?(?=YEAR|YEARS)") %>%
  trimws() %>%
  as.numeric()

max(sentence, na.rm = TRUE) ##max = 1750, life should be > 

life <- which(parole1$sentence == "LIFE")

parole1$sentence_numeric <- sentence
parole1$sentence_numeric[life] <- 2000

names(parole1)[names(parole1) == 'sentence_numeric'] <- 'sentence_in_years'

##Joining DOC demographic data - Race, Gender, DOB

OffenderDemographics <- Offender %>%
  select(DocNum, Race, Gender, DOB)

names(OffenderDemographics)[names(OffenderDemographics) == 'DocNum'] <- 'docnum'

parole1 <- full_join(parole1, OffenderDemographics, by = "docnum")

parole1 <- parole1 %>%
  filter(!is.na(outcome))

count(parole1,Race)
count(parole1, Gender)

parole1$Race <- recode(parole1$Race, "American Indian" = "I", Asian = "A", Black = "B", Hispanic = "H", Other = "O", "Pacific Islander" = "A", Unknown = "U", White = "W")
parole1$Gender <- recode(parole1$Gender, Female = "F", Male = "M")

##Cleaning charges - needs standardization
no_charge <- which(parole1$charge_1 == "")

parole1$charge_1[no_charge] <- "UNKNOWN"
parole1$charge_1 <- str_remove(parole1$charge_1, "#") %>%
  trimws()


##Finding duplicates
parole1 <- unique(parole1) ##only 1

##Outcome - cleaning
parole1 <- parole1 %>%
  mutate(outcome = ifelse(str_detect(outcome, "PAROLE DENIED"), "DENIED",
         ifelse(str_detect(outcome, "TO THE STREET"), "PAROLE GRANTED",
         ifelse(str_detect(outcome, "PAROLE RECOMMENDED"), "PASSED TO STAGE II",
         outcome))))

##More cleaning - program type
parole1$program_type <- str_remove(parole1$program_type, "^\\(\\d{1,3}\\)")
which(parole1$program_type == ", 12 MONTH MANDATORY UA'S, SAT, PSY. COUNSELING, & 52 WEEK CERTIFIED OR TRIBAL BATTER'S PROGRAM")
parole1$program_type[4653] <- "12 MONTH MANDATORY UA'S, SAT, PSY. COUNSELING, & 52 WEEK CERTIFIED OR TRIBAL BATTER'S PROGRAM"

View(count(parole, program_type))
##Writing final csvs
write.csv(parole1, "/Users/annarouw/Desktop/Oklahoma Policy Institute/Research Projects/Probation and Parole/parole1.csv")

write.csv(concurrent, "/Users/annarouw/Desktop/Oklahoma Policy Institute/Research Projects/Probation and Parole/concurrent.csv")

write.csv(consecutive, "/Users/annarouw/Desktop/Oklahoma Policy Institute/Research Projects/Probation and Parole/consecutive.csv")

##Duplicates

duplicates <- parole %>%
  select(docnum, month)

##7,276 - 7,273 means that there are 3 duplicates... 

which(duplicated(duplicates))

##2355, 2990, 3067

duplicates[3067,]

##699355 (Feb 2018), 701373 (Jan 2018), 667038 (Jan 2018) -- duplicate cases

which(parole$docnum == "667038" & parole$month == "2018-01-01")

View(parole[c(3066, 3067),])

parole <- parole[-c(2355, 2990, 3066), ]
parole$X1 <- NULL
