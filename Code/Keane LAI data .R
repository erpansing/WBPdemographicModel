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
#TODO: WTF is sel?
#TODO: WTF is conf?
#TODO: Why are some values of burn "" whereas others are "unburned"?
#TODO: What are the different fuels treatments (none, nutcracker, enhancement, treatment)
#TODO: What are the different planting treatments?


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

lai <- read.csv(paste0(proj_dir, "Data/LAI.csv"))

colnames(lai) <- c("sa", "date", "burn_date", "plot", "subplot", "subsubplot", "vp", "lai", "sel", "conf", "notes", "burn", "fuels", "planting")
str(lai)

lai$date <- lubridate::ymd(as.character(lai$date))
lai <- lai %>% mutate(year = lubridate::year(date))
#---------------------------------------|---------------------------------------
#                        Explore to make sure data match design
#---------------------------------------|---------------------------------------

nrow(lai)

length(unique(lai$sa)) # should be 6
unique(lai$sa) 
sum(is.na(lai$sa))

sum(lai$sa == "")
lai %>% filter(., sa == "")
which(lai$sa == "")

lai <- lai %>% filter(., sa != "")

nrow(lai)
length(unique(lai$sa)) # should be 6
unique(lai$sa) 

# B = table(lai$subplot, lai$plot, lai$sa)

# C = *Coyote Meadows
# M = *Musgrove Creek
# R = 
# S = *Smith Creek

##

range(lai$year)
sum(is.na(lai$year))

unique(lai$burn_date)
sum(is.na(lai$burn_date))
sum(is.na(lai$burn_date))/nrow(lai)

unique(lai$plot)
sum(is.na(lai$plot))

unique(lai$subplot)
sum(is.na(lai$subplot))

unique(lai$subsubplot)
sum(is.na(lai$subsubplot))

unique(lai$vp)
sum(is.na(lai$vp))

unique(lai$sel)
sum(is.na(lai$sel))

unique(lai$conf)
sum(is.na(lai$conf))

unique(lai$notes)

unique(lai$burn)
sum(lai$burn == "")
lai[which(lai$burn == ""),]
sum(lai$burn == "" & is.na(lai$burn_date))/sum(lai$burn == "") # all entries with "" burn status have NA burn dates
sum(lai$burn == "" & is.na(lai$burn_date))/sum(is.na(lai$burn_date)) # not all entries with NA burn dates have "" burn status
lai[which(is.na(lai$burn_date) & !(lai$burn == "")),] # all have burn status == "unburned"


unique(lai$fuels)
# none, "", nutcracker, enhancement, treatment
sum(is.na(lai$fuels))
sum(lai$fuels == "")


unique(lai$planting)
sum(is.na(lai$planting))
sum(lai$planting == "")

table(lai[, c("burn")], useNA = "always")
table(lai[, c("fuels")], useNA = "always")
table(lai[, c("planting")], useNA = "always")


sum(is.na(lai$burn) & is.na(lai$fuels))

####################################################
lai_first <- lai %>% 
  dplyr::group_by(., sa) %>% 
  dplyr::filter(., year == min(year)) %>% 
  dplyr::ungroup()

R <- lai %>% 
  filter(., sa == "R")

Rfirst <- lai_first %>% 
  filter(., sa == 'R')

table(Rfirst$subplot, Rfirst$plot)
table(R$subplot, R$plot)

table( R$fuels, R$planting, R$burn)

table(lai[,c("burn","fuels", "planting")], useNA = "always")


## According to Keane & Parsons 2010, the plot and subplot (my own names) determine
# the treatment. So creating a "treatment" variable that is a combination of plot and subplot

lai <- lai %>% 
  mutate(., treatment = paste0(plot, subplot))




## Look at data 

lai %>% 
  filter(., sa == "B") %>% 
  group_by(., treatment, date) %>% 
  summarise_at(., vars(lai), funs(mean)) %>% 
  ggplot(aes(x = date, y = lai, col = as.factor(treatment)))+
  geom_point()

# Example, Beaver Ridge
# Treatment areas are 1a, 2a, 3a, etc. which corrrespond to control, nutcracker openings no burn, 
# nut
