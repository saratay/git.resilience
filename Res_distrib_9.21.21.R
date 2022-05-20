library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(ggdist)

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
wqas0000-  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))
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

ASPEdf$Sample <- "ASPE"
MTurkdf$Sample <- "MTurk"
FBdf$Sample <- "FB"

common_cols <- intersect(colnames(ASPEdf), colnames(MTurkdf))

df <- rbind(
  subset(ASPEdf, select = common_cols), 
  subset(FBdf, select = common_cols),
  subset(MTurkdf, select = common_cols)
)

# Add in ACE and SES scores -----------------------------------------------
df <- df %>%
  mutate_at(c(colnames(df)[-which(names(df) %in% c("Sample"))]),
            function(x) as.numeric(as.character(x)))
df$ace_score <- rowSums(df[ , c("ace_1","ace_2","ace_3","ace_4",
                                "ace_5","ace_6","ace_7","ace_8","ace_9",
                                "ace_10")],na.rm = FALSE)
df$ses_score <- rowSums(df[,c("demo_edu","demo_income")])

# Raincloud ---------------------------------------------------------------
my_theme <- function(leg.pos, base_size){
  theme_bw(base_size=base_size)+
    theme(text = element_text(size=10),
          legend.box = "horizontal",
          legend.position=leg.pos,
          #legend.justification=c(1.31,-0.4),
          legend.text = element_text(size=8),
          legend.title = element_text(size=10),
          legend.background = element_rect(colour = "transparent", fill = "transparent"),
          legend.key = element_blank(),
          plot.margin=unit(c(-0.5,0.1,-0.5,0.1), "cm"))
    
}
PositionNudgeJitter <- ggproto("PositionNudgeJitter", Position,
                               x = 0,
                               y = 0,
                               
                               setup_params = function(self, data) {
                                 list(x = self$x, y = self$y, width = self$width)
                               },
                               
                               compute_layer = function(self, data, params, layout) {
                                 # transform only the dimensions for which non-zero nudging is requested
                                 if (any(params$x != 0)) {
                                   if (any(params$y != 0)) {
                                     transform_position(data, function(x) jitter(x, amount=params$width) + params$x, function(y) y + params$y)
                                   } else {
                                     transform_position(data, function(x) jitter(x, amount=params$width) + params$x, NULL)
                                   }
                                 } else if (any(params$y != 0)) {
                                   transform_position(data, NULL, function(y) y + params$y)
                                 } else {
                                   data # if both x and y are 0 we don't need to transform
                                 }
                               }
)

position_nudgejitter <- function(x = 0, y = 0, width = NULL) {
  ggproto(NULL, PositionNudgeJitter,
          x = x,
          y = y,
          width = width
  )
}

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Plots -------------------------------------------------------------------

##raincloud plot function

raincloudplot <- function(dataframe, group, variable, 
                         title,xlabel,ylabel){
  ggplot(data=dataframe, aes(x=group, y = variable, fill = group)) + 
    ggdist::stat_halfeye(
      adjust = .5, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      alpha = 0.35,
      point_colour = NA) + 
    geom_boxplot(
      width = .25,
      outlier.shape = NA) +
    geom_point(
      aes(x = group, y = variable, color = group),
      size = 0.7,
      alpha = .5,
      position = position_nudgejitter(x=-.3, width=.07)
    ) +
    ggtitle(title) +
    xlab(xlabel) + 
    ylab(ylabel)+
    my_theme("none",6)+ 
    scale_fill_manual(values = cbp1)+
    scale_color_manual(values = cbp1)
}


p1 <- raincloudplot(df,"",df$srs2a_sr_total_t,
                    "","","SRS-2A Total")
p2 <- raincloudplot(df,"",df$score_overall_resilience_mean,
                    "","","Penn Res")
p3 <- raincloudplot(df,"",df$cd_score,
                    "","","CD-RISC")
p4 <- raincloudplot(df,"",df$score_depression_classification,
                    "","","PHQ2")
p5 <- raincloudplot(df,"",df$score_anxiety_classification,
                    "","","GAD7")
p6 <- raincloudplot(df,"",df$ace_score,
                    "","","ACE")
p7 <- raincloudplot(df,"",df$ses_score,
                    "","","SES")


pdf("Figure1.pdf", width = 6, height = 4)
ggarrange(p1,p2,p3,p4,p5,p6,p7,
          ncol = 3,
          nrow = 3,
          labels = "AUTO")
dev.off()
    