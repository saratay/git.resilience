
library(tidyverse)
library(lm.beta)
#library(dplyr)

setwd("~/Sheets & Scripts/Resilience")

# Read in sheets ----------------------------------------------------------

ASPEinput <- read.csv("ASPEres_6.29.21.csv")
FBdf <- read.csv("FBres_6.29.21.csv")
MTurkdf <- read.csv("MTurkres_6.29.21.csv")


# Clean sheets ------------------------------------------------------------
#collapse ASPE rows by participant id
ASPEinput <- ASPEinput[ , -which(names(ASPEinput) %in% c("redcap_event_name"))]
ASPEdf <- ASPEinput[, colSums(is.na(ASPEinput)) != nrow(ASPEinput)]
ASPEdf <- ASPEdf %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))
ASPEdf <- ASPEdf %>% 
  group_by(study_id) %>% 
  fill(colnames(ASPEdf), .direction = "downup") %>% 
  distinct(.keep_all = FALSE)


#create new education variable for ASPE that matches FB and MTurk values
ASPEdf$part_edu <- as.integer(ASPEdf$part_edu)
ASPEdf$demo_edu <- NA
ASPEdf <- ASPEdf %>% mutate(demo_edu =
                     case_when(part_edu <= 11 ~ 0, 
                               part_edu == 12 ~ 1,
                               part_edu <= 15 ~ 2,
                               part_edu <= 17 ~ 3,
                               part_edu <= 19 ~ 4,
                               part_edu == 20 ~ 5)
)

# rename variable in ASPE to match FB and MTurk

ASPEdf <- ASPEdf %>% rename_at(c("part_income","part_age_screen",
                                 "part_sex"),~c("demo_income","demo_age",
                                                "demo_sex"))

#remove duplicate MTurk entries
MTurkdf <- MTurkdf[!duplicated(MTurkdf$outro_workerid), ]

# Merge all data together -------------------------------------------------

common_cols <- intersect(colnames(ASPEdf), colnames(MTurkdf))

DF <- rbind(
  subset(ASPEdf, select = common_cols), 
  subset(FBdf, select = common_cols),
  subset(MTurkdf, select = common_cols)
)

DF$ts_resiliency <- as.Date(DF$ts_resiliency, '%Y-%m-%d')

DF <- DF %>%
  mutate_at(c(colnames(DF)[-which(names(DF) %in% c("ts_resiliency","cd_date"))]),
            function(x) as.numeric(as.character(x)))


# Add in ACE and SES scores -----------------------------------------------

DF$ace_score <- rowSums(DF[ , c("ace_1","ace_2","ace_3","ace_4",
                      "ace_5","ace_6","ace_7","ace_8","ace_9",
                      "ace_10")],na.rm = FALSE)
DF$ses_score <- rowSums(DF[,c("demo_edu","demo_income")])


# Trimmed down DF for analysis --------------------------------------------

DF.na <- na.omit(DF[,c("srs2a_sr_total_t",
                       "score_overall_resilience_mean",
                       "cd_score",
                       "score_anxiety_classification",
                       "score_depression_classification",
                       "demo_age",
                       "demo_sex",
                       "ace_score",
                       "ses_score",
                       "ts_resiliency")])

# Bivariable relationships Anxiety------------------------------------------------

##Anxiety + Dem
model1 <- lm(DF.na$score_anxiety_classification ~ 
               DF.na$demo_age + DF.na$demo_sex + 
               DF.na$ts_resiliency +
               DF.na$ace_score + DF.na$ses_score)
summary(model1)
lm.beta(model1)

par(mfrow = c(2, 2))
plot(model1) #diagnostic plots

##Anxiety + COVID 19 Res
model2 <- lm(DF.na$score_anxiety_classification ~ DF.na$score_overall_resilience_mean + 
               DF.na$demo_age + DF.na$demo_sex + 
               DF.na$ts_resiliency +
               DF.na$ace_score + DF.na$ses_score)
summary(model2)
lm.beta(model2)
plot(model2) #diagnostic plots

##Anxiety + CD RISC
model3 <- lm(DF.na$score_anxiety_classification ~ DF.na$cd_score + 
               DF.na$demo_age + DF.na$demo_sex + 
               DF.na$ts_resiliency +
               DF.na$ace_score + DF.na$ses_score)
summary(model3)
lm.beta(model3)
plot(model3)  #diagnostic plots


##Anx + SRS
model4 <- lm(DF.na$score_anxiety_classification ~ DF.na$srs2a_sr_total_t + 
               DF.na$demo_age + DF.na$demo_sex + 
               DF.na$ts_resiliency +
               DF.na$ace_score + DF.na$ses_score)
summary(model4)
lm.beta(model4)  
plot(model4) #diagnostic plots

## calculate sum of squares to get R2 change
model0 <- lm(DF.na$score_anxiety_classification ~ 1)

anova(model0)
anova(model1,model2)
anova(model1,model3)
anova(model1,model4)

# Bivariable relationships Depression -------------------------------------

##Depression + Dem
model1 <- lm(DF.na$score_depression_classification ~ 
               DF.na$demo_age + DF.na$demo_sex + 
               DF.na$ts_resiliency +
               DF.na$ace_score + DF.na$ses_score)
summary(model1)
lm.beta(model1)
plot(model1) #diagnostic plots


##Depression + COVID 19 Res
model2 <- lm(DF.na$score_depression_classification ~ DF.na$score_overall_resilience_mean + 
               DF.na$demo_age + DF.na$demo_sex + 
               DF.na$ts_resiliency +
               DF.na$ace_score + DF.na$ses_score)
summary(model2)
lm.beta(model2)
plot(model2) #diagnostic plots


##Depression + CD RISC
model3 <- lm(DF.na$score_depression_classification ~ DF.na$cd_score + 
               DF.na$demo_age + DF.na$demo_sex + 
               DF.na$ts_resiliency +
               DF.na$ace_score + DF.na$ses_score)
summary(model3)
lm.beta(model3)
plot(model3) #diagnostic plots



##Depression + SRS
model4 <- lm(DF.na$score_depression_classification ~ DF.na$srs2a_sr_total_t + 
               DF.na$demo_age + DF.na$demo_sex + 
               DF.na$ts_resiliency +
               DF.na$ace_score + DF.na$ses_score)
summary(model4)
lm.beta(model4)
plot(model4) #diagnostic plots


## calculate sum of squares to get R2 change
model0 <- lm(DF.na$score_depression_classification ~ 1)

anova(model0)
anova(model1,model2)
anova(model1,model3)
anova(model1,model4)

# Bivariable relationships SRS -------------------------------------

##SRS + Dem
model1 <- lm(DF.na$srs2a_sr_total_t ~ 
               DF.na$demo_age + DF.na$demo_sex + 
               DF.na$ts_resiliency +
               DF.na$ace_score + DF.na$ses_score)
summary(model1)
lm.beta(model1)
plot(model1) #diagnostic plots


##SRS + COVID 19 Res
model2 <- lm(DF.na$srs2a_sr_total_t ~ DF.na$score_overall_resilience_mean + 
               DF.na$demo_age + DF.na$demo_sex + 
               DF.na$ts_resiliency +
               DF.na$ace_score + DF.na$ses_score)
summary(model2)
lm.beta(model2)
plot(model2) #diagnostic plots


##SRS + CD RISC
model3 <- lm(DF.na$srs2a_sr_total_t ~ DF.na$cd_score + 
               DF.na$demo_age + DF.na$demo_sex + 
               DF.na$ts_resiliency +
               DF.na$ace_score + DF.na$ses_score)
summary(model3)
lm.beta(model3)
plot(model3) #diagnostic plots



## calculate sum of squares to get R2 change
model0 <- lm(DF.na$srs2a_sr_total_t ~ 1)

anova(model0)
anova(model1,model2)
anova(model1,model3)