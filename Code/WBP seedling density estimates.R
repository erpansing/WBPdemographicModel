################################################################################
#                                                                              #
#                 Starting density estimates for wbp seedlings                 #
#                                                                              #
# AUTHOR: Elizabeth Pansing                                                    #
#                                                                              #
# DESCRIPTION: .... Code that projects wbp metapopulation size forward in time #
#                   Code that compiles model is in "WBP Demographic Model      #
#                   Metapopulation Model.R". This script simply runs the code  #
#                   abd obtains the results                                    #
#                                                                              #
#                   This script deals soley with population sizes and          #
#                   extirpation                                                #
################################################################################
# rm(list = ls())

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

proj_dir_seedling_density <- '/Users/elizabethpansing/Box/Yellowstone/88-Fires-Analysis/'
rda_dir <- "Rda/"
code_dir <- "Code/"

################################################################################
#---------------------------------------|---------------------------------------
#                                Define functions 
#---------------------------------------|---------------------------------------
################################################################################

lower <- function(x){
  quantile(x, prob = 0.025)
}

upper <- function(x){
  quantile(x, prob = 0.975)
}


q1 <- function(x){
  quantile(x, prob = 0.25)
}

q3 <- function(x){
  quantile(x, prob = 0.75)
}

################################################################################
#---------------------------------------|---------------------------------------
#                              Read in YNP pial data 
#---------------------------------------|---------------------------------------
################################################################################

load(file = paste0(proj_dir_seedling_density, "2017 YNP Data.Rda"))


seedling_density_hm <- pial %>% 
  filter(., StudyArea == "HM") %>% 
  filter(., Year != 2005) %>% 
  filter(., Status == "L") %>% 
  filter(., Age != 0 | is.na(Age)) %>% 
  group_by(., Year, StudyArea, BurnStatus, MoistureRegime, PlotNo) %>% 
  tally(.) %>% 
  ungroup(.) %>% 
  mutate(., seedling_density = n/20) %>% 
  complete(., Year = c(1990, 1991, 1992, 1994, 1995, 2001, 2016), PlotNo = 1:150, fill = list(seedling_density = 0)) %>% 
  mutate(., StudyArea = "Henderson") %>% 
  mutate(., BurnStatus = ifelse(PlotNo <= 100, "B", "U")) %>% 
  mutate(., MoistureRegime = ifelse(PlotNo <=50 | PlotNo > 125, "D", "M"))



seedling_density_mw <- pial %>% 
  filter(., StudyArea == "MW") %>% 
  filter(., Year != 2005) %>% 
  filter(., Status == "L") %>% 
  filter(., Age != 0 | is.na(Age)) %>% 
  group_by(., Year, StudyArea, BurnStatus, MoistureRegime, PlotNo) %>% 
  tally(.) %>% 
  ungroup(.) %>% 
  mutate(., seedling_density = n/20) %>% 
  complete(., Year = c(1990, 1991, 1992, 1994, 1995, 2001, 2016), PlotNo = c(200:300, 330:355), fill = list(seedling_density = 0)) %>% 
  mutate(., StudyArea = "Washburn") %>% 
  mutate(., BurnStatus = ifelse(PlotNo <=300 , "B", "MB")) %>% 
  mutate(., MoistureRegime = ifelse(PlotNo <=250, "D", "M"))

seedling_density <- bind_rows(seedling_density_hm, seedling_density_mw) %>% 
  mutate(unique = paste(Year, PlotNo))




cot_density_hm <- pial %>% 
  filter(., StudyArea == "HM") %>% 
  filter(., Year != 2005) %>% 
  filter(., Status == "L") %>% 
  filter(., Age == 0 | is.na(Age)) %>% 
  group_by(., Year, StudyArea, BurnStatus, MoistureRegime, PlotNo) %>% 
  tally(.) %>% 
  ungroup(.) %>% 
  mutate(., cot_density = n/20) %>% 
  complete(., Year = c(1990, 1991, 1992, 1994, 1995, 2001, 2016), PlotNo = 1:150, fill = list(cot_density = 0)) %>% 
  mutate(., StudyArea = "Henderson") %>% 
  mutate(., BurnStatus = ifelse(PlotNo <= 100, "B", "U")) %>% 
  mutate(., MoistureRegime = ifelse(PlotNo <=50 | PlotNo > 125, "D", "M"))




cot_density_mw <- pial%>% 
  filter(., StudyArea == "MW") %>% 
  filter(., Year != 2005) %>% 
  filter(., Status == "L") %>% 
  filter(., Age == 0 | is.na(Age)) %>% 
  group_by(., Year, StudyArea, BurnStatus, MoistureRegime, PlotNo) %>% 
  tally(.) %>% 
  ungroup(.) %>% 
  mutate(., cot_density = n/20) %>% 
  complete(., Year = c(1990, 1991, 1992, 1994, 1995, 2001, 2016), PlotNo = c(200:300, 330:355), fill = list(cot_density = 0)) %>% 
  mutate(., StudyArea = "Washburn") %>% 
  mutate(., BurnStatus = ifelse(PlotNo <=300 , "B", "MB")) %>% 
  mutate(., MoistureRegime = ifelse(PlotNo <=250, "D", "M"))

cot_density <- bind_rows(cot_density_hm, cot_density_mw) %>% 
  mutate(unique = paste(Year, PlotNo))



cot_seedling_density <- merge(cot_density, seedling_density, by = "unique") %>% 
  dplyr::select(Year = Year.x, PlotNo = PlotNo.x, StudyArea = StudyArea.x, BurnStatus = BurnStatus.x, 
                MoistureRegime = MoistureRegime.x, cot_density, seedling_density) %>% 
  dplyr::mutate(cot_count = cot_density * 1000) %>% 
  dplyr::mutate(seedling_count = seedling_density * 1000) %>% 
  filter(., !(cot_density == 0 & seedling_density == 0))



rm(cot_density, cot_density_hm, cot_density_mw, pial, 
            seedling_density, seedling_density_hm, seedling_density_mw, 
            proj_dir_seedling_density)

