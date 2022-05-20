library(tidyverse)
library(dplyr)
library(tidyr)
library(corrplot)
library(Hmisc)
library(ggpubr)

setwd("~/Sheets & Scripts/Resilience")

# Read in sheets ----------------------------------------------------------

ASPEinput <- read.csv("ASPEres_data_6.16.21.csv")
FBdf <- read.csv("FBres_data_6.17.21.csv")
MTurkdf <- read.csv("MTurkres_data_6.17.21.csv")


# Clean sheets ------------------------------------------------------------
#collapse ASPE rows by participant id
ASPEinput <- ASPEinput[ , -which(names(ASPEinput) %in% c("redcap_event_name"))]
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

# Correlation matrix ------------------------------------------------------

df <- na.omit(df)
cormat = as.matrix(as.data.frame(lapply(df, as.numeric)))

colnames(cormat) <- c("SRS SR \n Total","Res Survey",
                             "CD-RISC","PHQ2", "GAD7") #rename colunms for graph

stat <- rcorr(cormat, type = "spearman")
p <- formatC(stat$P, format = "e", digits = 2)

M = cor(cormat, method = "spearman")

col <- colorRampPalette(c("red3","white","turquoise3"))

pdf("Figure2.pdf", width = 6, height = 4)
corrplot(M, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=60, #Text label color and rotation
         # Combine with significance
         p.mat = stat$P, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)
dev.off()
