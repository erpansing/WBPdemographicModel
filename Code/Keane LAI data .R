################################################################################
#                                                                              #
#                     WBP Demographic Model LAI data                           #
#                                                                              #
# AUTHOR: Elizabeth Pansing                                                    #
# DATA SOURCE: R. Keane, USFS RMRS Missoula Fire Lab                           #
# DESCRIPTION: .... The following explores LAI values for whitebark pine       #
#                   forests collected by R. Keane and colleagues at 6 sites:   #
#                   1) Bear Overlook, 2) Musgrove Creek, 3) Beaver Ridge,      #
#                   4) Snowbowl, 5) Coyote Meadows, 6) Smith Creek. All sites  #
#                   are in the Northern Rocky Mtns. Each site experienced      #
#                   prescribed fire of various intensities (although some      #
#                   plots within sites were controls) and some were thinned.   # 
#                   Plots have been monitored at various time points since.    #
################################################################################

rm(list = ls())

options(stringsAsFactors = FALSE)

options(scipen = 999)

################################################################################
#---------------------------------------|---------------------------------------
#                                Load libraries
#---------------------------------------|---------------------------------------
################################################################################

suppressMessages(library(tidyverse))

################################################################################
#---------------------------------------|---------------------------------------
#                             Establish directories
#---------------------------------------|---------------------------------------
################################################################################

proj_dir <- '/Users/elizabethpansing/Box/PhD/Code/WBP Demographic Model Master/WBPdemographicModel/'
rda_dir <- "Rda/"
data_dir <- "Data/"
code_dir <- "Code/"
model_file <- "Pansing et al. MPM Model Final? Gamma fire.R"
figures_dir <- "Figures/Final?/"
export_table_dir <-  "Exported Data Tables/"

################################################################################
#---------------------------------------|---------------------------------------
#                                Load LAI data 
#---------------------------------------|---------------------------------------
################################################################################

KeaneLAI <- read.csv(paste0(proj_dir, "Data/LAI.csv"))

colnames(KeaneLAI) <- c("sa", "date", "plot", "subplot", "subsubplot", "vp", "lai", "sel", "conf", "notes")
KeaneLAI <- KeaneLAI[,1:10]
str(KeaneLAI)

KeaneLAI$date <- lubridate::ymd(as.character(KeaneLAI$date))
#---------------------------------------|---------------------------------------
#                        Explore to make sure data match design
#---------------------------------------|---------------------------------------

nrow(KeaneLAI)


length(unique(KeaneLAI$sa)) # should be 6
unique(KeaneLAI$sa) 

sum(KeaneLAI$sa == "")
KeaneLAI %>% filter(., sa == "")
which(KeaneLAI$sa == "")

KeaneLAI <- KeaneLAI %>% filter(., sa != "")

nrow(KeaneLAI)
length(unique(KeaneLAI$sa)) # should be 6
unique(KeaneLAI$sa) 

# B = Bear Beaver Ridge
# C = Coyote Meadows
# M = Musgrove Creek
# R = 
# S = Snowbowl?

## According to Keane & Parsons 2010, the plot and subplot (my own names) determine
# the treatment. So creating a "treatment" variable that is a combination of plot and subplot

KeaneLAI <- KeaneLAI %>% 
  mutate(., treatment = paste0(plot, subplot))




## Look at data 

KeaneLAI %>% 
  filter(., sa == "B") %>% 
  group_by(., treatment, date) %>% 
  summarise_at(., vars(lai), funs(mean)) %>% 
  ggplot(aes(x = date, y = lai, col = as.factor(treatment)))+
  geom_point()

# Example, Beaver Ridge
# Treatment areas are 1a, 2a, 3a, etc. which corrrespond to control, nutcracker openings no burn, 
# nut
