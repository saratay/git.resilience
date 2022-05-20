library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(scales)
library(RColorBrewer)

setwd("~/Sheets & Scripts/Resilience")

# Read in sheets ----------------------------------------------------------
ASPEinput1 <- read.csv("ASPEres_6.29.21.csv")
FBdf1 <- read.csv("FBres_6.29.21.csv")
MTurkdf1 <- read.csv("MTurkres_6.29.21.csv")

ASPEinput2 <- read.csv("ASPE_COVIDExper_7.6.21.csv")
FBdf2 <- read.csv("FB_COVIDExper_7.6.21.csv")
MTurkdf2 <- read.csv("MTurk_COVIDExper_7.6.21.csv")

ASPEinput <- merge (ASPEinput1,ASPEinput2, by = "study_id")
FBdf <- merge (FBdf1,FBdf2, by = "record_id")
MTurkdf <- merge (MTurkdf1,MTurkdf2, by = "outro_workerid")


# Clean sheets ------------------------------------------------------------
#collapse ASPE rows by participant id
ASPEinput <- ASPEinput[ , -which(names(ASPEinput) %in% c("redcap_event_name.x",
                                                         "redcap_event_name.y"))]
ASPEdf <- ASPEinput[, colSums(is.na(ASPEinput)) != nrow(ASPEinput)]
ASPEdf <- ASPEdf %>% 
  group_by(study_id) %>% 
  fill(colnames(ASPEdf), .direction = "downup") %>% 
  distinct(.keep_all = FALSE)
ASPEdf <- ASPEdf %>% 
  group_by(study_id) %>% 
  fill(colnames(ASPEdf), .direction = "downup") %>% 
  distinct(.keep_all = FALSE)

#remove duplicate MTurk entries
MTurkdf <- MTurkdf[!duplicated(MTurkdf$outro_workerid), ]

# Merge all data together -------------------------------------------------

common_cols <- intersect(colnames(ASPEdf), colnames(MTurkdf))

df <- rbind(
  subset(ASPEdf, select = common_cols), 
  subset(FBdf, select = common_cols),
  subset(MTurkdf, select = common_cols)
)

df %>% select(cv19sq_12,cv19sq_13,cv19sq_14,cv19sq_15,cv19sq_16,cv19sq_17) %>% rowSums(na.rm=TRUE) -> df$TotalConcern



# Graph total concerns ----------------------------------------------------


df$ts_covid <- as.Date(df$ts_covid) #convert date column into date object
df$srs2a_sr_total_t <- as.factor(df$srs2a_sr_total_t) #set SRS as factor so can use as grouping variable in graph
#df$srs2a_sr_total_t <- reorder(df$srs2a_sr_total_t, 
#                               df$TotalConcern, 
#                               function(x) -max(x) ) #this put SRS values in order of concern level, makes graph prettier but less interpretable

theme_set(theme_minimal()) #set theme for all graphs

mycolors <- colorRampPalette(brewer.pal(11, "Spectral"))(53) #expand color palette so is extensive enough to cover all SRS values (n = 52)


p1 <- ggplot(df, aes(x = ts_covid, y = TotalConcern, 
                     fill = srs2a_sr_total_t, color = srs2a_sr_total_t)) + 
  geom_area(aes(color = srs2a_sr_total_t, fill = srs2a_sr_total_t), 
            alpha = 0.4, position = "identity") +
  scale_x_date(date_labels = "%b %Y") +
  scale_fill_manual(values = mycolors)+
  scale_color_manual(values = mycolors)+
  xlab("Date of COVID-19 Questionnaire Completion")+
  ggtitle("COVID-19 Concern over Time as Relating to ASD Traits")+
  theme(legend.position = "none") 

p1

df$score_anxiety_classification <- as.factor(df$score_anxiety_classification) #set GAD7 as factor so can use as grouping variable in graph

mycolors <- colorRampPalette(brewer.pal(11, "Spectral"))(22) #expand color palette so is extensive enough to cover all GAD values (n = 52)


p2 <- ggplot(df, aes(x = ts_covid, y = TotalConcern, 
                     fill = score_anxiety_classification, 
                     color = score_anxiety_classification)) + 
  geom_area(aes(color = score_anxiety_classification, fill = score_anxiety_classification), 
            alpha = 0.4, position = "identity") +
  scale_x_date(date_labels = "%b %Y") +
  scale_fill_manual(values = mycolors)+
  scale_color_manual(values = mycolors)+
  xlab("Date of COVID-19 Questionnaire Completion")+
  ggtitle("COVID-19 Concern over Time as Relating to Anxiety")+
  theme(legend.position = "none") 

p2

df$score_depression_classification <- as.factor(df$score_depression_classification) #set GAD7 as factor so can use as grouping variable in graph

mycolors <- colorRampPalette(brewer.pal(11, "Spectral"))(7) #reduce color palette to fit PHQ values 


p3 <- ggplot(df, aes(x = ts_covid, y = TotalConcern, 
                     fill = score_depression_classification, 
                     color = score_depression_classification)) + 
  geom_area(aes(color = score_depression_classification, fill = score_depression_classification), 
            alpha = 0.4, position = "identity") +
  scale_x_date(date_labels = "%b %Y") +
  scale_fill_manual(values = mycolors)+
  scale_color_manual(values = mycolors)+
  xlab("Date of COVID-19 Questionnaire Completion")+
  ggtitle("COVID-19 Concern over Time as Relating to Depression")+
  theme(legend.position = "none") 

p3

