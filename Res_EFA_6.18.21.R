
library(tidyverse)
library(psych)
library(stringr)
library(GPArotation)

setwd("~/Sheets & Scripts/Resilience")

# Read in sheets ----------------------------------------------------------

ASPEinput <- read.csv("ASPEres_questions_6.17.21.csv", stringsAsFactors = FALSE)
FBdf <- read.csv("FBres_questions_6.17.21.csv", stringsAsFactors = FALSE)
MTurkdf <- read.csv("MTurkres_questions_6.17.21.csv", stringsAsFactors = FALSE)


# Clean sheets ------------------------------------------------------------
#collapse ASPE rows by participant id
ASPEinput <- ASPEinput[ , -which(names(ASPEinput) %in% c("redcap_event_name", #non numerical, do not need
                                                         "rr_covid19_nri_adult_closest", #categorical variable not included in calculation
                                                         "rr_covid19_nri_adult_closest_other",
                                                         "rr_covid19_nri_closest",
                                                         "rr_covid19_nri_closest_other"))] #categorical variable not included in calculation
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

DF <- rbind(
  subset(ASPEdf, select = common_cols), 
  subset(FBdf, select = common_cols),
  subset(MTurkdf, select = common_cols)
)


# Determine number of factors ---------------------------------------------


#generate scree plot to determine number of factors for EFA
parallel <- fa.parallel(DF, fm = 'minres', fa = 'fa')

#EFA fit statistics for 2 to 7 factors

fit.DF <- data.frame("RMSEA" = 2:7 ,"TLI" = 2:7,
                     "Chi.Stat" = 2:7,"p.value" = 2:7)
for (i in 2:7){
  bloop <- fa(DF, nfactors = i, rotate = "oblimin",fm = "minres")
  fit.DF[i-1,"RMSEA"] <- bloop$RMSEA[1]
  fit.DF[i-1,"TLI"] <- bloop$TLI
  fit.DF[i-1,"Chi.Stat"] <- bloop$STATISTIC
  fit.DF[i-1,"p.value"] <- bloop$PVAL
}


# EFA ---------------------------------------------------------------------



# Maximum Likelihood Factor Analysis
# 7 factors
sevenfactor <- fa(DF,nfactors = 7,rotate = "oblimin",fm="minres")
print(sevenfactor)
#set loading cutoff of 0.3
print(sevenfactor$loadings,cutoff = 0.3)
#save loading in dataframe
loadDF <- as.data.frame(unclass(sevenfactor$loadings))

#write speadsheet
write.csv(loadDF,"loading7Fac.csv")


# Visualize ---------------------------------------------------------------

plot(density(sevenfactor$scores, na.rm = TRUE),main = "Factor Scores")
plot(density(sevenfactor$loadings, na.rm = TRUE),main = "Factor Loadings")

library (ggplot2)
library(ggpubr)
library(gtsummary)
loadDF$questionnaire <- c(rep("SRS",times = 65), #SRS questions 
                  rep("COVID19", times = 22), #COVID 19 Resilience questions
                  rep("CDRISC",times = 25)) #CD-RISC questions

load1 <- loadDF %>%
  group_by(questionnaire) %>%
  summarise(mean_loading = mean(MR1, na.rm = TRUE))
load2 <- loadDF %>%
  group_by(questionnaire) %>%
  summarise(mean_loading = mean(MR2, na.rm = TRUE))
load3 <- loadDF %>%
  group_by(questionnaire) %>%
  summarise(mean_loading = mean(MR3, na.rm = TRUE))
load4 <- loadDF %>%
  group_by(questionnaire) %>%
  summarise(mean_loading = mean(MR4, na.rm = TRUE))
load5 <- loadDF %>%
  group_by(questionnaire) %>%
  summarise(mean_loading = mean(MR5, na.rm = TRUE))
load6 <- loadDF %>%
  group_by(questionnaire) %>%
  summarise(mean_loading = mean(MR6, na.rm = TRUE))
load7 <- loadDF %>%
  group_by(questionnaire) %>%
  summarise(mean_loading = mean(MR7, na.rm = TRUE))

load.sum.DF <- rbind(load1,load2,load3,load4,load5,load6,load7)
load.sum.DF$Factor <- c(rep("Factor 1",times = 3),  
                        rep("Factor 2", times = 3),
                        rep("Factor 3",times = 3),
                        rep("Factor 4", times = 3),
                        rep("Factor 5", times = 3),
                        rep("Factor 6", times = 3),
                        rep("Factor 7", times = 3))
load.sum.DF$Combined <- as.factor(paste(load.sum.DF$questionnaire,load.sum.DF$Factor))


ggbarplot(load.sum.DF, x = "Combined", 
           y = "mean_loading",
           fill = "questionnaire",                      # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           rotate = TRUE,
           sort.val = "desc",
           ggtheme = theme_minimal()                       # ggplot2 theme
)

ggbarplot(load.sum.DF, x = "Combined", 
          y = "mean_loading",
          fill = "questionnaire",                      # Color by groups
          palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
          rotate = TRUE,
          ggtheme = theme_minimal()                       # ggplot2 theme
)



