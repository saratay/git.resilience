library(tidyverse)
library(dplyr)
library(gtsummary)
library(tidyr)

setwd("~/Sheets & Scripts/Resilience")

# Read in sheets ----------------------------------------------------------

ASPEinput <- read.csv("ASPEres_6.16.21.csv")
FBdf <- read.csv("FBres_6.16.21.csv")
MTurkdf <- read.csv("MTurkres_6.16.21.csv")


# Clean sheets ------------------------------------------------------------

#collapse ASPE rows by participant id
ASPEinput <- ASPEinput[ , -which(names(ASPEinput) %in% c("redcap_event_name",
                                                         "part_raceoth_v2"))]
ASPEdf <- ASPEinput[, colSums(is.na(ASPEinput)) != nrow(ASPEinput)]
ASPEdf$mht_1 <- as.integer(ASPEdf$mht_1)
ASPEdf <- ASPEdf %>% 
  group_by(study_id) %>% 
  fill(colnames(ASPEdf), .direction = "downup") %>% 
  distinct(.keep_all = FALSE)

#incorporate Middle Eastern race column into Other race column for ASPE
ASPEdf <- within(ASPEdf, part_race_v2___6[part_race_v2___6 == 0 & part_race_v2___8 == 1] <- 1)

#incorporate "I do not know" race column into Other for FB and MTurk
FBdf$demo_race___7 <- 0 #need to manually add this column for FB sample as 
FBdf <- within(FBdf, demo_race___7[demo_race___7 == 0 & demo_race___6 == 1] <- 1)
MTurkdf <- within(MTurkdf, demo_race___7[demo_race___7 == 0 & demo_race___6 == 1] <- 1)

#rename age and sex variables to match FB and MTurk

ASPEdf <- ASPEdf %>% rename(demo_age = part_age_screen, 
                            demo_sex = part_sex,
                            demo_gender = part_gender_id,
                            demo_race___0 = part_race_v2___4,
                            demo_race___1 = part_race_v2___3,
                            demo_race___2 = part_race_v2___2,
                            demo_race___3 = part_race_v2___5,
                            demo_race___4 = part_race_v2___1,
                            demo_race___5 = part_race_v2___7,
                            demo_race___7 = part_race_v2___6)

#remove duplicate MTurk entries
MTurkdf <- MTurkdf[!duplicated(MTurkdf$outro_workerid), ]

# Add sample column for each ----------------------------------------------

ASPEdf$sample <- "ASPE"
FBdf$sample <- "FB"
MTurkdf$sample <- "MTurk"


# Merge all data together -------------------------------------------------

common_cols <- intersect(colnames(ASPEdf), colnames(MTurkdf))

DF <- rbind(
  subset(ASPEdf, select = common_cols), 
  subset(FBdf, select = common_cols),
  subset(MTurkdf, select = common_cols)
)


# Rename variables and levels to make table prettier ----------------------

colnames(DF)

##NOTE: AT THIS STEP NEED TO VERIFY COLUMNS IN SAME ORDER AS BELOW

colnames(DF) <- c("Age",
                        "Race: White",
                        "Race: African American / Black",
                        "Race: Asian",
                        "Race: American Indian / Alaska Native",
                        "Race: Native Hawaiian / Pacific Islander",
                        "Race: Other",
                        "Race: Prefer not to say",
                        "Sex","Gender Identity",
                        "History of Mental Health","ADHD",
                        "Anorexia","Anxiety","ASD","Bipolar",
                        "Depression","Schizophrenia","ID",
                        "Language Delay","OCD",
                        "Personality Disorder","Other Psyc",
                        "Sample")

##change level names of variables - recruit type, sex, gender id

DF[,c("Sex","Gender Identity",
            "History of Mental Health")] <- 
  lapply(DF[,c("Sex","Gender Identity",
                     "History of Mental Health")] , factor)
levels(DF$Sex) <- c("Female","Male","I prefer not to say")
levels(DF$`Gender Identity`) <- c("Cis-Female","Cis-Male",
                                        "Trans-female","Trans-male",
                                        "Non-binary","Other", "Prefer not to say",
                                        "I do not know")
levels(DF$`History of Mental Health`) <- c("Prefer Not to Say",
                                           "No","Yes","Do Not Know",
                                           "Prefer Not to Say")


# Make table --------------------------------------------------------------

tbl_summary(DF, by = Sample,
            statistic = list(all_continuous() ~ "{mean} ({sd})", 
                             all_categorical() ~ "{n} ({p}%)"))

