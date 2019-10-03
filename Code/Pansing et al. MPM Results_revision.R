################################################################################
#                                                                              #
#                     WBP Demographic Model Projection                         #
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

rm(list = ls())

options(stringsAsFactors = FALSE)

options(scipen = 999)

################################################################################
#---------------------------------------|---------------------------------------
#                                Load libraries
#---------------------------------------|---------------------------------------
################################################################################

suppressMessages(library(tidyverse))
suppressMessages(library(data.table))

################################################################################
#---------------------------------------|---------------------------------------
#                             Establish directories
#---------------------------------------|---------------------------------------
################################################################################

proj_dir <- '/Users/elizabethpansing/Box/PhD/Code/WBP Demographic Model Master/WBPdemographicModel/'
rda_dir <- "Rda/"
data_dir <- "Data/"
code_dir <- "Code/"
model_file <- "Pansing et al. WBP demographic model_revision.R"
figures_dir <- "Figures/"
export_results_dir <-  "Exported Results Tables/"


###############################################################################
#---------------------------------------|---------------------------------------
#                        Define functions and variables
#---------------------------------------|---------------------------------------
################################################################################

################################################################################
#                        FUNCTION TO RESHAPE MODEL OUTPUT                      #

reshapeModelOutput <- function(dataIn        = constant_high_distance_revision, 
                               area          = 2000,
                               fireScenario  = "Historical fire return interval",
                               dispersalProb = "Low dispersal",
                               period        = "Current",
                               result        = "data.frame") {
  
  #-------------------------------------|-------------------------------------
  #                      Establish tSinceFire data.table
  
  tSinceFire <- data.table::as.data.table(dataIn[["tSinceFire_tracker"]])
  
  #-------------------------------------|-------------------------------------
  #                   Rename tSinceFire variables explicitly
  
  data.table::setnames(tSinceFire, old=c("V1","V2","V3","V4"), new=c("Iteration","t","tSinceFire1","tSinceFire2"))
  
  #-------------------------------------|-------------------------------------
  #                    Preprocess tSinceFire data.table
  
  tSinceFire <- tSinceFire[,IterationTime:=paste(Iteration, t)] %>%
    .[,.(tSinceFire1,tSinceFire2,IterationTime)]
  
  #-------------------------------------|-------------------------------------
  #                      Establish popSizes data.table
  
  popSizes   <- data.table::as.data.table(dataIn[["pop_sizes"]])
  
  #-------------------------------------|-------------------------------------
  #                 Create popSizes iteration time variable
  
  popSizes[,IterationTime:=paste(Iteration, t)]
  
  #-------------------------------------|-------------------------------------
  #                   Remove seed popSizes variables
  
  popSizes[,c("SEED2_1","SEED2_2"):=NULL]
  
  #-------------------------------------|-------------------------------------
  #                         Define pop sizes and densities
  # Total tree population sizes
  popSizes[,PopSize:=rowSums(popSizes[, .SD, .SDcols = names(popSizes) %like% "_"])]
  popSizes[,PopSize1:=rowSums(popSizes[, .SD, .SDcols = names(popSizes) %like% "_1"])]
  popSizes[,PopSize2:=rowSums(popSizes[, .SD, .SDcols = names(popSizes) %like% "_2"])]
  
  # Mature tree population sizes
  popSizes[,MaturePopSize:=rowSums(popSizes[, .SD, .SDcols = names(popSizes) %like% "MA_"])]
  popSizes[,MaturePopSize1:=rowSums(popSizes[, .SD, .SDcols = names(popSizes) %like% "MA_1"])]
  popSizes[,MaturePopSize2:=rowSums(popSizes[, .SD, .SDcols = names(popSizes) %like% "MA_2"])]
  
  # Sapling tree population sizes
  popSizes[,SaplingPopSize:=rowSums(popSizes[, .SD, .SDcols = names(popSizes) %like% "SAP_"])]
  popSizes[,SaplingPopSize1:=rowSums(popSizes[, .SD, .SDcols = names(popSizes) %like% "SAP_1"])]
  popSizes[,SaplingPopSize2:=rowSums(popSizes[, .SD, .SDcols = names(popSizes) %like% "SAP_2"])]
  
  # Mature + sapling tree population sizes
  popSizes[,SaplingMaturePopSize:=rowSums(popSizes[, .SD, .SDcols = names(popSizes) %like% "MA_|SAP_"])]
  popSizes[,SaplingMaturePopSize1:=rowSums(popSizes[, .SD, .SDcols = names(popSizes) %like% "MA_1|SAP_1"])]
  popSizes[,SaplingMaturePopSize2:=rowSums(popSizes[, .SD, .SDcols = names(popSizes) %like% "MA_2|SAP_2"])]
  
  # Total tree population densities
  popSizes[,Density:=PopSize/area]
  popSizes[,Density1:=PopSize1/(area/2)]
  popSizes[,Density2:=PopSize2/(area/2)]
  
  # Mature tree population densities
  popSizes[,MA_Density:=MaturePopSize/area]
  popSizes[,MA_Density1:=MaturePopSize1/(area/2)]
  popSizes[,MA_Density2:=MaturePopSize2/(area/2)]

  # Sapling tree population densities
  popSizes[,SAP_Density:=SaplingPopSize/area]
  popSizes[,SAP_Density1:=SaplingPopSize1/(area/2)]
  popSizes[,SAP_Density2:=SaplingPopSize2/(area/2)]
  
  # Mature + sapling tree population densities
  popSizes[,SAP_MA_Density:=SaplingMaturePopSize/area]
  popSizes[,SAP_MA_Density1:=SaplingMaturePopSize1/(area/2)]
  popSizes[,SAP_MA_Density2:=SaplingMaturePopSize2/(area/2)]
  #-------------------------------------|-------------------------------------
  #                                   Label
  popSizes[,Fire_scenario:=fireScenario]
  popSizes[,Dispersal_probability:=dispersalProb]
  popSizes[,Period:=period]
  #-------------------------------------|-------------------------------------
  #                            Define results
  dataOut <- merge(popSizes, tSinceFire, by="IterationTime", all=TRUE) %>%
    .[,t_burnin:=t] %>%
    .[,t:=(t-10)] %>%
    .[t>0,] %>% 
    .[,IterationTime := paste(Iteration, t)]
  #-------------------------------------|-------------------------------------
  #                              Return results
  if(result=="data.frame") { return(as.data.frame(dataOut)) }
  if(result=="data.table") { return(dataOut) }

}



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


projection_time <- 500
reps <- 5000
area <- 2000

### Set plotting theme

theme_set(theme_bw())

axis_label_text_size <- 23
strip_text_size <- 18
axis_text_size <- 18


################################################################################
#---------------------------------------|---------------------------------------
#                            Load Model Output                        
#---------------------------------------|---------------------------------------
################################################################################

## Scenario #1: Constant fire return interval, low distance

load(paste0(proj_dir, rda_dir, "Revision constant high distance.Rda"))

## Scenario #2: Constant fire return interval, high distance

load(paste0(proj_dir, rda_dir, "Revision constant low distance.Rda"))

## Scenario #3: Fire suppression, high distance

load(paste0(proj_dir, rda_dir, "Revision suppression high distance.Rda"))

## Scenario #4: Fire suppression, low distance

load(paste0(proj_dir, rda_dir, "Revision suppression low distance.Rda"))

## Scenario #5: Decreasing fire return interval, high distance

load(paste0(proj_dir, rda_dir, "Revision decrease high distance.Rda"))

## Scenario #6: Decreasing fire return interval, low distance

load(paste0(proj_dir, rda_dir, "Revision decrease low distance.Rda"))

## Scenario #7: Constant fire return interval, low distance

load(paste0(proj_dir, rda_dir, "Revision constant high distance historical.Rda"))

## Scenario #8: Constant fire return interval, high distance

load(paste0(proj_dir, rda_dir, "Revision constant low distance historical.Rda"))

################################################################################
#---------------------------------------|---------------------------------------
#                                Reshape model output  
#---------------------------------------|---------------------------------------
################################################################################
constant_high_distance_pop_size_total <- reshapeModelOutput(dataIn        = constant_high_distance_revision, 
                                                            area          = 2000,
                                                            fireScenario  = "Historical fire return interval",
                                                            dispersalProb = "Low dispersal",
                                                            period        = "Current",
                                                            result        = "data.frame") 

constant_low_distance_pop_size_total <- reshapeModelOutput(dataIn         = constant_low_distance_revision, 
                                                            area          = 2000,
                                                            fireScenario  = "Historical fire return interval",
                                                            dispersalProb = "High dispersal",
                                                            period        = "Current",
                                                            result        = "data.frame") 

suppression_high_distance_pop_size_total <- reshapeModelOutput(dataIn       = suppression_high_distance_revision, 
                                                              area          = 2000,
                                                              fireScenario  = "Suppression",
                                                              dispersalProb = "Low dispersal",
                                                              period        = "Current",
                                                              result        = "data.frame") 

suppression_low_distance_pop_size_total <- reshapeModelOutput(dataIn        = suppression_low_distance_revision, 
                                                              area          = 2000,
                                                              fireScenario  = "Suppression",
                                                              dispersalProb = "High dispersal",
                                                              period        = "Current",
                                                              result        = "data.frame") 

decrease_high_distance_pop_size_total <- reshapeModelOutput(dataIn       = decrease_high_distance_revision, 
                                                           area          = 2000,
                                                           fireScenario  = "Decreasing fire return interval",
                                                           dispersalProb = "Low dispersal",
                                                           period        = "Current",
                                                           result        = "data.frame") 

decrease_low_distance_pop_size_total <- reshapeModelOutput(dataIn        = decrease_low_distance_revision, 
                                                           area          = 2000,
                                                           fireScenario  = "Decreasing fire return interval",
                                                           dispersalProb = "High dispersal",
                                                           period        = "Current",
                                                           result        = "data.frame") 


constant_high_distance_historical_pop_size_total <- reshapeModelOutput(dataIn       = constant_high_distance_historic_revision, 
                                                                      area          = 2000,
                                                                      fireScenario  = "Historical fire return interval",
                                                                      dispersalProb = "Low dispersal",
                                                                      period        = "Historical",
                                                                      result        = "data.frame") 

constant_low_distance_historical_pop_size_total <- reshapeModelOutput(dataIn       = constant_low_distance_historic_revision, 
                                                                     area          = 2000,
                                                                     fireScenario  = "Historical fire return interval",
                                                                     dispersalProb = "High dispersal",
                                                                     period        = "Historical",
                                                                     result        = "data.frame")

rm(constant_high_distance_revision)

rm(constant_low_distance_revision)

rm(suppression_high_distance_revision)

rm(suppression_low_distance_revision)

rm(decrease_high_distance_revision)

rm(decrease_low_distance_revision)

rm(constant_high_distance_historic_revision)

rm(constant_low_distance_historic_revision)

#---------------------------------------|---------------------------------------
#                           Density as a function of 
#                       time since fire. Current demographics
#---------------------------------------|---------------------------------------

# pop1 <- constant_high_distance_pop_size_total %>% 
#   dplyr::select(., Iteration, dplyr::contains("1")) %>% 
#   dplyr::mutate(., Pop = "pop1")  %>% 
#   dplyr::mutate(., Dispersal_probability = "High")
# 
# colnames(pop1) <- c("Iteration","CS", "SD", "SAP", "MA", "PopSize", "MaturePopSize", "SaplingPopSize", "SaplingMaturePopSize",
#                     "Density", "MA_Density", "SAP_Density", "SAP_MA_Density", "tSinceFire", "Pop", "Dispersal_probability")
# 
# pop2 <- constant_high_distance_pop_size_total %>% 
#   dplyr::select(., Iteration, dplyr::contains("2")) %>% 
#   dplyr::mutate(., Pop = "pop2") %>% 
#   dplyr::mutate(., Dispersal_probability = "High")
# 
# colnames(pop2) <- c("Iteration", "CS", "SD", "SAP", "MA", "PopSize", "MaturePopSize", "SaplingPopSize", "SaplingMaturePopSize",
#                     "Density", "MA_Density", "SAP_Density", "SAP_MA_Density", "tSinceFire", "Pop", "Dispersal_probability")
# 
# pops <- dplyr::bind_rows( pop1, pop2)
# 
# rm(pop1, pop2)
# 
# density_tSinceFire <- pops %>% 
#   dplyr::group_by(., tSinceFire, Dispersal_probability) %>% 
#   dplyr::summarise_at(., dplyr::vars(MA_Density, SAP_MA_Density, Density), 
#                list(mean = mean, median = median, min = min, 
#                     lower = lower, q1 = q1, q3 = q3, upper = upper, max = max)) %>% 
#   dplyr::ungroup() 
# 
# tiff(filename = paste0(proj_dir, figures_dir, "Final SAP MA density by tSinceFire.tiff"),
#      width = 12,
#      height = 8.5,
#      units = "in",
#      res = 300,
#      compression = "lzw")
# ggplot2::ggplot(data = density_tSinceFire, 
#                 ggplot2::aes(x = tSinceFire, y = SAP_MA_Density_median)) +
#   ggplot2::geom_ribbon(data = density_tSinceFire, 
#                        ggplot2::aes(x    = tSinceFire,
#                                     ymin = SAP_MA_Density_q1,
#                                     ymax = SAP_MA_Density_q3),
#                        alpha = 0.2)+
#   ggplot2::geom_line(lwd = 1.5) +
#   ggplot2::theme_bw() +
#   ggplot2::labs(x = "Time since fire", y = expression(paste("Median sapling and mature tree density (no. trees/hectare)")))+
#   ggplot2::theme(axis.text  = ggplot2::element_text(size = axis_text_size)) +
#   ggplot2::theme(axis.title = ggplot2::element_text(size = axis_label_text_size))

# dev.off()

################################################################################
#---------------------------------------|---------------------------------------
#                               Create Figures                  
#---------------------------------------|---------------------------------------
################################################################################

## Goal is to create a figure that shows median population size as
## a function of time stratified by the scenarios
## Start by filtering for total density, then aggregate population sizes from each scenario.
## I do not consider seeds as these are not generally counted in
## estimates of tree abundance/density measures.

#---------------------------------------|---------------------------------------
#                           Density as a function of 
#                       time since fire. Historical demographics
#---------------------------------------|---------------------------------------

historical <- dplyr::bind_rows(constant_high_distance_historical_pop_size_total, constant_low_distance_historical_pop_size_total)

pop1 <- historical %>% 
  dplyr::select(., Iteration, Fire_scenario, Dispersal_probability, dplyr::contains("1")) %>% 
  dplyr::mutate(., Pop = "pop1") 

colnames(pop1) <- c("Iteration","Fire_scenario", "Dispersal_probability", "CS", "SD", "SAP", "MA", "PopSize", "MaturePopSize", "SaplingPopSize", "SaplingMaturePopSize",
                    "Density", "MA_Density", "SAP_Density", "SAP_MA_Density", "tSinceFire", "Pop")

pop2 <- historical %>% 
  dplyr::select(., Iteration, Fire_scenario, Dispersal_probability, dplyr::contains("2")) %>% 
  dplyr::mutate(., Pop = "pop2") 

colnames(pop2) <- c("Iteration","Fire_scenario", "Dispersal_probability", "CS", "SD", "SAP", "MA", "PopSize", "MaturePopSize", "SaplingPopSize", "SaplingMaturePopSize",
                    "Density", "MA_Density", "SAP_Density", "SAP_MA_Density", "tSinceFire", "Pop")

pops <- dplyr::bind_rows(pop1, pop2)

rm(pop1, pop2)

density_tSinceFire <-  pops %>% 
  dplyr::group_by(., tSinceFire, Fire_scenario, Dispersal_probability) %>% 
  dplyr::summarise_at(., dplyr::vars(MA_Density, SAP_MA_Density, Density), dplyr::funs(mean, median, min, lower, q1, q3, upper, max)) %>% 
  dplyr::ungroup() 


(year30density <- pops %>% 
  dplyr::filter(., tSinceFire <= 30) %>% 
  dplyr::group_by(., tSinceFire) %>% 
  dplyr::summarise_at(., dplyr::vars(SAP_MA_Density, Density), dplyr::funs(median, mean,lower, upper, min, max, q1, q3)))


# write.csv(year30density, file = paste0(proj_dir, export_results_dir, "30 Year Post Fire Density Historical dispersal averaged in hectares revision.csv"))


year100200300400500 <- pops %>% 
  dplyr::filter(., tSinceFire == 100 |
            tSinceFire == 200 |
           tSinceFire == 300 |
           tSinceFire == 400 |
           tSinceFire == 500) %>% 
  dplyr::group_by(., Dispersal_probability, as.factor(tSinceFire)) %>% 
  dplyr::summarise_at(., dplyr::vars(SAP_MA_Density, Density), dplyr::funs(mean, median, min, lower, q1, q3, upper, max)) %>% 
  dplyr::ungroup()

# write.csv(year100200300400500, file = paste0(proj_dir, export_results_dir, "Many Year Post Fire Density Historic revision.csv"))

# tiff(filename = paste0(proj_dir, figures_dir, "Final historic SAP MA density by tSinceFire revision.tiff"),
#      width = 12,
#      height = 8.5,
#      units = "in",
#      res = 300,
#      compression = "lzw")

density_tSinceFire %>% 
  dplyr::mutate(., Fire_scenario = ifelse(Fire_scenario=='Decreasing fire return interval', 'Decreasing',
                                          ifelse(Fire_scenario=='Historical fire return interval', 'Historical conditions',
                                                 ifelse(Fire_scenario=='Suppression', 'Suppression', NA)))) %>%
  ggplot2::ggplot(data = ., ggplot2::aes(x = tSinceFire, y = SAP_MA_Density_median, fill = Dispersal_probability))+
  
  ggplot2::geom_ribbon(ggplot2::aes(x = tSinceFire, ymin = SAP_MA_Density_min, ymax = SAP_MA_Density_max,
                                             fill = Dispersal_probability), alpha = 0.2)+
  ggplot2::scale_fill_manual(values = c("#ef8a62", "#67a9cf")) +
  ggplot2::geom_ribbon(aes(x = tSinceFire, ymin = SAP_MA_Density_q1, ymax = SAP_MA_Density_q3,
                                             col = Dispersal_probability), alpha = 0.05, lty = 2) +
  ggplot2::geom_line(ggplot2::aes(col = Dispersal_probability), lwd = 1.5) +
  ggplot2::scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "\nTime since fire (years)", y = expression(paste("Median sapling and mature tree density (no. trees/hectare)")))+
  ggplot2::theme(axis.text  = ggplot2::element_text(size = axis_text_size)) +
  ggplot2::theme(axis.title = ggplot2::element_text(size = axis_label_text_size))+
  ggplot2::theme(legend.position = "none") +
  ggplot2::facet_grid(~Fire_scenario) +
  ggplot2::theme(strip.background = ggplot2::element_rect(colour = "black", fill = "white"),
                 strip.text       = ggplot2::element_text(colour = "black",  size = strip_text_size))+
  ggplot2::theme(plot.margin = grid::unit(c(3, 1, 1, 1), "lines")) #+  # top, right, bottom, left

  
legd <- grid::legendGrob(c("High dispersal probability","Low dispersal probability"),
                         nrow          = 1,
                         ncol          = 2,
                         lines.first   = TRUE,
                         hgap          = grid::unit(2, "lines"),
                         vgap          = grid::unit(1, "lines"),
                         default.units = "lines",
                         pch           = 22,
                         gp = grid::gpar(col      = c("black","black"),
                                         fill     = c("#ef8a62", "#67a9cf"),
                                         fontsize = axis_text_size,
                                         fontface = "bold"),
                         vp = grid::viewport(x    = 0,
                                             y    = 0,
                                             w    = 1.05,
                                             h    = 1.94,
                                             just = c("left", "bottom")))
grid::grid.draw(legd)
  

# dev.off()

rm(pops, legd, density_tSinceFire)


#---------------------------------------|---------------------------------------
#                        Bind model outputs for each time period
#---------------------------------------|---------------------------------------

current_models <- dplyr::bind_rows(constant_low_distance_pop_size_total, 
                                   constant_high_distance_pop_size_total,
                                   suppression_low_distance_pop_size_total,
                                   suppression_high_distance_pop_size_total,
                                   decrease_low_distance_pop_size_total,
                                   decrease_high_distance_pop_size_total) %>% 
  dplyr::mutate(., Treatment = paste(Fire_scenario, Dispersal_probability, Period))

unique(current_models$Treatment)

rm(constant_high_distance_pop_size_total, 
   constant_low_distance_pop_size_total,
   suppression_high_distance_pop_size_total, 
   suppression_low_distance_pop_size_total,
   decrease_high_distance_pop_size_total, 
   decrease_low_distance_pop_size_total)


historic_models <- dplyr::bind_rows(constant_low_distance_historical_pop_size_total,
                                    constant_high_distance_historical_pop_size_total) %>% 
  dplyr::mutate(., Treatment = paste(Fire_scenario, Dispersal_probability, Period))

unique(historic_models$Treatment)

rm(constant_low_distance_historical_pop_size_total,
   constant_high_distance_historical_pop_size_total)
   

current_model_summary <- current_models %>% 
  dplyr::group_by(., Fire_scenario, Dispersal_probability, t) %>% 
  dplyr::summarise_at(., dplyr::vars(Density, SAP_Density, MA_Density, SAP_MA_Density), 
               list(mean = mean, median = median, min = min, lower = lower, 
                    q1 = q1, q3 = q3, upper = upper, max = max)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Period   = "Current")


historic_model_summary <- historic_models %>% 
  dplyr::group_by(., Fire_scenario, Dispersal_probability, t) %>% 
  dplyr::summarise_at(., dplyr::vars(Density, SAP_Density, MA_Density, SAP_MA_Density), 
                         dplyr::funs(mean, median, min, lower, q1, q3, upper, max)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Period   = "Historical")


summary_frame <- dplyr::bind_rows(historic_model_summary, current_model_summary)

summary_frame$Fire_scenario <- factor(summary_frame$Fire_scenario, 
                                      levels = c("Historical fire return interval", "Suppression", "Decreasing fire return interval"))

rm(historic_model_summary, current_model_summary)


# #---------------------------------------|---------------------------------------
# #               Total population trajectory (ALL LIFE STAGES BUT SEEDS)                  
# #---------------------------------------|---------------------------------------
# 
# tiff(filename = paste0(proj_dir, figures_dir, "Current SAP MA Population Trajectory Figure revision.tiff"),
#      width = 12,
#      height = 8.5,
#      units = "in",
#      res = 300,
#      compression = "lzw")

(MA_SAP_population_plot <- summary_frame %>%
   # filter(., t < 200) %>%
   dplyr::filter(., Period == "Current") %>%
   ggplot2::ggplot(data = .,
                   ggplot2::aes(x     = t,
                                y     = SAP_MA_Density_median,
                                group = Dispersal_probability))+
   ggplot2::geom_ribbon(ggplot2::aes(x    = t,
                                     ymin = SAP_MA_Density_min,
                                     ymax = SAP_MA_Density_max,
                                     fill = Dispersal_probability),
                        alpha = 0.3)+
   ggplot2::scale_fill_manual(values = c("#ef8a62", "#67a9cf")) +
   ggplot2::geom_ribbon(aes(x    = t, 
                            ymin = SAP_MA_Density_q1, 
                            ymax = SAP_MA_Density_q3,
                            col  = Dispersal_probability), alpha = 0.05, lty = 2) +
   ggplot2::scale_fill_manual(values = c("#ef8a62", "#67a9cf")) +
   ggplot2::geom_line(ggplot2::aes(x   = t,
                                   y   = SAP_MA_Density_median,
                                   col = Dispersal_probability),
                      lwd = 1.5) +
   ggplot2::scale_color_manual(values = c("#ef8a62", "#67a9cf"))+
   ggplot2::facet_grid(~ Fire_scenario) + # free_x removes extra y axis tick labels.... go figure
   ggplot2::theme_bw() +
   ggplot2::theme(strip.background = ggplot2::element_rect(colour = "black", fill = "white"),
                  strip.text       = ggplot2::element_text(colour = "black",  size = strip_text_size))+
   ggplot2::theme(axis.title       = ggplot2::element_text(size = axis_label_text_size))+
   ggplot2::theme(axis.text        = ggplot2::element_text(size = axis_text_size))+
   ggplot2::theme(legend.position  = "none") +
   ggplot2::theme(plot.margin = grid::unit(c(2.5, 1, 1, 1), "lines")) + # top, right, bottom, left
   ggplot2::scale_x_continuous(breaks = seq(0, 500, by = 100))+
   ggplot2::labs(x = "Time (years)",
                 y = expression(paste("Median sapling and mature tree density (no. trees/hectare)")))+
   ggplot2::geom_hline(yintercept = 0, col = "black", lty = 2))


legd <- grid::legendGrob(c("High dispersal probability","Low dispersal probability"),
                         nrow          = 1,
                         ncol          = 2,
                         lines.first   = TRUE,
                         hgap          = grid::unit(2, "lines"),
                         vgap          = grid::unit(1, "lines"),
                         default.units = "lines",
                         pch           = 22,
                         gp            = grid::gpar(col      = c("black","black"),
                                                    fill     = c("#ef8a62", "#67a9cf"),
                                                    fontsize = axis_text_size,
                                                    fontface = "bold"),
                         vp            = grid::viewport(x    = 0,
                                                        y    = 0,
                                                        w    = 1.05,
                                                        h    = 1.92,
                                                        just = c("left", "bottom")))
grid::grid.draw(legd)

# dev.off()
# 
# 
# tiff(filename = paste0(proj_dir, figures_dir, "Final Historic SAP MA Population Trajectory Figure.tiff"),
#      width = 12,
#      height = 8.5,
#      units = "in",
#      res = 300,
#      compression = "lzw")

(MA_SAP_population_plot <- summary_frame %>%
    dplyr::filter(., Period == "Historical") %>%
    ggplot2::ggplot(data = .,
                    ggplot2::aes(x     = t,
                                 y     = SAP_MA_Density_median,
                                 group = Dispersal_probability))+
    ggplot2::geom_ribbon(ggplot2::aes(x    = t,
                                      ymin = SAP_MA_Density_min,
                                      ymax = SAP_MA_Density_max,
                                      fill = Dispersal_probability),
                         alpha = 0.3)+
    ggplot2::scale_fill_manual(values = c("#ef8a62", "#67a9cf")) +
    ggplot2::geom_ribbon(ggplot2::aes(x    = t,
                                      ymin = SAP_MA_Density_q1,
                                      ymax = SAP_MA_Density_q3,
                                      col = Dispersal_probability),
                         alpha = 0.05, lty = 2)+
    ggplot2::scale_fill_manual(values = c("#ef8a62", "#67a9cf")) +
    ggplot2::geom_line(ggplot2::aes(x   = t,
                                    y   = SAP_MA_Density_median,
                                    col = Dispersal_probability),
                       lwd = 1.5) +
    ggplot2::scale_color_manual(values = c("#ef8a62", "#67a9cf"))+
    ggplot2::facet_grid(~Period, scales = "free")+ # free_x removes extra y axis tick labels.... go figure
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour = "black", fill = "white"),
                   strip.text       = ggplot2::element_text(colour = "black",  size = strip_text_size))+
    ggplot2::theme(axis.title       = ggplot2::element_text(size = axis_label_text_size))+
    ggplot2::theme(axis.text        = ggplot2::element_text(size = axis_text_size))+
    ggplot2::theme(legend.position  = "none") +
    ggplot2::theme(plot.margin = grid::unit(c(2.5, 1, 1, 1), "lines")) + # top, right, bottom, left
    ggplot2::scale_x_continuous(breaks = seq(0, 500, by = 125))+
    ggplot2::labs(x = "Time (years)",
                  y = expression(paste("Median sapling and mature tree density (no. trees/hectare)")))+
    ggplot2::geom_hline(yintercept = 0, col = "black", lty = 2))


legd <- grid::legendGrob(c("High dispersal probability","Low dispersal probability"),
                         nrow          = 1,
                         ncol          = 2,
                         lines.first   = TRUE,
                         hgap          = grid::unit(2, "lines"),
                         vgap          = grid::unit(1, "lines"),
                         default.units = "lines",
                         pch           = 22,
                         gp            = grid::gpar(col      = c("black","black"),
                                                    fill     = c("#ef8a62", "#67a9cf"),
                                                    fontsize = axis_text_size,
                                                    fontface = "bold"),
                         vp            = grid::viewport(x    = 0,
                                                        y    = 0,
                                                        w    = 1.05,
                                                        h    = 1.92,
                                                        just = c("left", "bottom")))
grid::grid.draw(legd)

# dev.off()
# 
# 
################################################################################
#---------------------------------------|---------------------------------------
#                           Extirpations by time horizon
#---------------------------------------|---------------------------------------
################################################################################

extinct_current <- data.table::as.data.table(current_models) %>%
  .[SAP_MA_Density < 0.5,] %>%
  #.[Density < 2e-04,] %>%
  .[order(Treatment, Iteration, t)] %>%
  .[ , .SD[which.min(t)], by = .(Treatment, Iteration)] %>% 
  .[,TreatmentItT:=paste(Treatment, Iteration, t)] %>%
  .[,TreatmentIt:=paste(Treatment, Iteration)] %>%
  .[,Extirpation:=1] %>%
  .[,.(Treatment, Iteration, t, Extirpation)] %>% 
  .[,Period := "Current"]


extinct_historical <- data.table::as.data.table(historic_models) %>%
  .[SAP_MA_Density < 0.05,] %>%
  #.[Density < 2e-04,] %>%
  .[order(Treatment, Iteration, t)] %>%
  .[, .SD[which.min(t)], by = .(Treatment, Iteration)] %>% 
  .[,TreatmentItT:=paste(Treatment, Iteration, t)] %>%
  .[,TreatmentIt:=paste(Treatment, Iteration)] %>%
  .[,Extirpation:=1] %>%
  .[,.(Treatment, Iteration, t, Extirpation)] %>% 
  .[,Period := "Historical"]


extinct <- dplyr::bind_rows(extinct_current, extinct_historical) %>% 
  dplyr::mutate(., Fire_scenario=ifelse(grepl(pattern = "Decreasing", x = Treatment), "Decreasing fire return interval", NA)) %>% 
  dplyr::mutate(., Fire_scenario=ifelse(grepl(pattern = "Historical", x = Treatment), "Historical fire return interval", Fire_scenario)) %>% 
  dplyr::mutate(., Fire_scenario=ifelse(grepl(pattern = "Suppression", x = Treatment), "Suppression", Fire_scenario)) %>% 
  dplyr::mutate(., Dispersal_probability=ifelse(grepl(pattern = "High", x = Treatment), "High dispersal", NA)) %>% 
  dplyr::mutate(., Dispersal_probability=ifelse(grepl(pattern = "Low", x = Treatment), "Low dispersal", Dispersal_probability))


# ggplot2::ggplot(data = extinct, ggplot2::aes(x = t))+
#   ggplot2::geom_density(alpha = 0.5)+
#   ggplot2::facet_grid( ~ Treatment )

Cum_extirpation_current <- merge(data.table::as.data.table(current_models), extinct_current, 
                                 by=c("Treatment", "Iteration","t"), all.x=TRUE) %>%
  dplyr::mutate(., Extirpation=ifelse(is.na(Extirpation), 0, Extirpation)) %>%
  dplyr::group_by(., Fire_scenario, Dispersal_probability, t) %>% 
  dplyr::summarise_at(., dplyr::vars(Extirpation), dplyr::funs(sum)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(., Fire_scenario, Dispersal_probability) %>% 
  dplyr::mutate(., CumSum = cumsum(Extirpation)) %>% 
  dplyr::mutate(., PExt = CumSum/5000) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Period = "Current")



Cum_extirpation_historical <-  merge(data.table::as.data.table(historic_models), extinct_historical, 
                                     by=c("Treatment", "Iteration","t"), all.x=TRUE) %>%
  dplyr::mutate(., Extirpation=ifelse(is.na(Extirpation), 0, Extirpation)) %>%
  dplyr::group_by(., Fire_scenario, Dispersal_probability, t) %>% 
  dplyr::summarise_at(., dplyr::vars(Extirpation), dplyr::funs(sum)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(., Fire_scenario, Dispersal_probability) %>% 
  dplyr::mutate(., CumSum = cumsum(Extirpation)) %>% 
  dplyr::mutate(., PExt = CumSum/5000) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Period = "Historical")

Cum_extirpation <- dplyr::bind_rows(Cum_extirpation_current,Cum_extirpation_historical)

Cum_extirpation %>% dplyr::filter(., t == 500)





(median_extinction_time <- extinct %>%
    dplyr::group_by(Period, Fire_scenario, Dispersal_probability) %>%
    dplyr::summarise_at(., vars(t), funs(min, lower, q1, median,mean, q3, upper, max)) %>%
    dplyr::ungroup())

median_extinction_time_export <- median_extinction_time %>% 
  dplyr::select(., Period, Fire_scenario, Dispersal_probability, q1, median, q3) %>% 
  dplyr::mutate(., `Time to extirpation` = paste0(round(median, 3), "(", round(q1,3), ", ",round(q3,3),")" )) %>% 
  dplyr::select(., Period, Fire_scenario, Dispersal_probability, `Time to extirpation`)

# write.csv(median_extinction_time_export, file = paste0(proj_dir, export_results_dir, "Extirpation times.csv"))

pd <- ggplot2::position_dodge(0.4)

ggplot2::ggplot(data = median_extinction_time,
                ggplot2::aes(x   = Fire_scenario,
                             y   = median,
                             col = Dispersal_probability))+
  ggplot2::geom_linerange(data = median_extinction_time,
                          ggplot2::aes(x        = Fire_scenario,
                                       y        = median,
                                       colour   = Dispersal_probability,
                                       ymin     = q1,
                                       ymax     = q3),
                          lwd      = 1.25,
                          position = pd) +
  ggplot2::geom_point(position = pd, pch = 21,fill = "white", size = 3, stroke = 2)+
  ggplot2::facet_wrap(~Period) +
  ggplot2::theme_bw() +
  ggplot2::theme(strip.background = ggplot2::element_rect(colour = "black", fill = "white"),
                 strip.text       = ggplot2::element_text(colour = "black",  size = strip_text_size))+
  ggplot2::theme(axis.title       = ggplot2::element_text(size = axis_label_text_size))+
  ggplot2::theme(axis.text        = ggplot2::element_text(size = axis_text_size))+
  ggplot2::theme(strip.text.x     = ggplot2::element_text(size = strip_text_size))+
  ggplot2::theme(legend.text      = ggplot2::element_text(size= axis_text_size)) +
  ggplot2::theme(legend.title     = ggplot2::element_text(size= axis_label_text_size))+
  ggplot2::scale_color_discrete("Dispersal\nprobability")+
  ggplot2::scale_fill_discrete("Dispersal\nprobability")+
  ggplot2::labs(x = "Fire scenario", y = "Median time to extirpation")


################################################################################
#---------------------------------------|---------------------------------------
#                       Cumulative probability of extirpation
#---------------------------------------|---------------------------------------
################################################################################

# tiff(filename = paste0(proj_dir, figures_dir, "Cumulative extirpation revision.tiff"),
#      width = 12,
#      height = 8.5,
#      units = "in",
#      res = 300,
#      compression = "lzw")

Cum_extirpation_current %>% 
  ggplot2::ggplot(data = ., ggplot2::aes(x = t, y = PExt, col = Fire_scenario))+
  ggplot2::geom_line(data = Cum_extirpation_current, ggplot2::aes(x = t, y = PExt, lty = Dispersal_probability), lwd = 1.5)+
  # scale_colour_manual(
  #   "CI horizontal line", values = c("#d73027", "#1a9850"),
  #   guide=guide_legend(override.aes = list(colour=c("#d73027", "#1a9850"))),
  #   labels=c("Decreasing fire return interval", "Historical fire return interval")
  # ) +
  ggplot2::scale_colour_manual(values = c("#d73027", "#1a9850", "#fee08b")) +
  # facet_grid(~ Dispersal_probability) +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "\nTime (years)",
                y = "Cumulative extirpation probability \n") +
  ggplot2::theme(strip.background = ggplot2::element_rect(colour = "black", fill = "white"),
                 strip.text       = ggplot2::element_text(colour = "black",  size = strip_text_size)) +
  # theme(legend.position = "none") +
  ggplot2::theme(legend.position = "top", legend.box = "vertical")+
  ggplot2::guides(linetype    = ggplot2::guide_legend(keywidth = 3, keyheight = 1)) +
  ggplot2::theme(legend.text  = ggplot2::element_text(size = axis_text_size))+
  ggplot2::theme(legend.title = ggplot2::element_blank())+
  ggplot2::theme(axis.title   = ggplot2::element_text(size = axis_label_text_size - 1)) +
  ggplot2::theme(axis.text    = ggplot2::element_text(size = axis_text_size)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.1))+
  ggplot2::theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines"))# top, right, bottom, left
# dev.off()

################################################################################
#---------------------------------------|---------------------------------------
#                                 Extirpations
#---------------------------------------|---------------------------------------
################################################################################

#---------------------------------------|---------------------------------------
#                                  Time Period 
# Do demographics alone in absence of changes to fire return interval suggest
# an increase in odds of extirpation?
#---------------------------------------|---------------------------------------
extirpation_historic <- extinct %>% 
  dplyr::filter(Fire_scenario == "Historical fire return interval") %>%
  dplyr::group_by(.,Period) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup()

# extirpation_150_historic[2,] <- c("Historic", 0) # only if time horizon is 100

historic_table <- extirpation_historic %>%
  dplyr::mutate(., Extirpations = as.numeric(n)) %>%
  dplyr::mutate(., Surviving = 2*reps - Extirpations) %>% 
  dplyr::select(., -n) 

period <- as.character(historic_table$Period)
outcomes <- c("Etirpation", "Surviving")

(dat <- matrix(c(as.numeric(historic_table$Extirpations), as.numeric(historic_table$Surviving)), byrow = F, nrow = nrow(historic_table)))
dimnames(dat) <- list("Period" = period, "Outcome" = outcomes)

(HistoricFRI_ORs <- fisher.test(dat))

HistoricFRI_ORs <- data.frame(OR = HistoricFRI_ORs$estimate, LL = HistoricFRI_ORs$conf.int[1], UL = HistoricFRI_ORs$conf.int[2]) %>% 
  dplyr::mutate(., Comparison = "Current/Historic") %>% 
  dplyr::mutate(., Period = "All") %>% 
  dplyr::mutate(., FireReturnInterval = "Historic") %>% 
  dplyr::mutate(., DispersalProbability = "All")

#---------------------------------------|--------------------------------------------------
#                                  Fire 
# Do changes to fire frequency, across all dispersal probabilities (fire sizes), increase
# odds of extirpation
#---------------------------------------|---------------------------------------------------

extirpation_fire <- extinct %>% 
  dplyr::filter(., Period == "Current") %>% 
  # dplyr::filter(., t <= 150) %>%
  dplyr::group_by(.,Fire_scenario) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup()

fire_table <- extirpation_fire %>% 
  dplyr::rename(., Extirpations = n) %>% 
  dplyr::mutate(., Surviving = reps * nrow(extirpation_fire) - Extirpations)# %>% 
# dplyr::mutate(., Odds = Extirpations/Surviving) %>% 
# dplyr::select(., -n, -Extirpations, - Surviving)  %>% 
# dplyr::mutate(., DecreasingHighNo  = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal No Restoration"], nrow(test))) %>% 
# dplyr::mutate(., DecreasingHighYes = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal Restoration"], nrow(test))) %>% 
# dplyr::mutate(., )

fire <- as.character(fire_table$Fire_scenario)
outcomes <- c("Etirpation", "Surviving")

(dat <- matrix(c(as.numeric(fire_table$Extirpations), as.numeric(fire_table$Surviving)), byrow = F, nrow = nrow(fire_table)))
dimnames(dat) <- list("Fire" = fire, "Outcome" = outcomes)

(dec_his_fire <- fisher.test(dat[1:2,]))
(his_sup_fire <- fisher.test(dat[2:3,]))
(dec_sup_fire <- fisher.test(dat[c(1,3),]))


fire_ORs <- data.frame(OR = c(dec_his_fire$estimate,   his_sup_fire$estimate,    dec_sup_fire$estimate),
                       LL = c(dec_his_fire$conf.int[1],his_sup_fire$conf.int[1], dec_sup_fire$conf.int[1]), 
                       UL = c(dec_his_fire$conf.int[2],his_sup_fire$conf.int[2], dec_sup_fire$conf.int[2])) %>% 
  dplyr::mutate(., Comparison = c("Decreasing/historical", "Historical/suppression", "Decreasing/suppression"))  %>% 
  dplyr::mutate(., Period = "Current") %>% 
  dplyr::mutate(., FireReturnInterval = "All") %>% 
  dplyr::mutate(., DispersalProbability = "All")
  
#---------------------------------------|---------------------------------------
#                                  Dispersal Decreasing
#  Does dispersal probability, across all fire frequencies (fire sizes) change
# odds of extirpation
#---------------------------------------|---------------------------------------

extirpation_dispersal_decreasing <- extinct %>% 
  dplyr::filter(., Period == "Current" & Fire_scenario == "Decreasing fire return interval") %>% 
  # dplyr::filter(., t <= 150) %>%
  dplyr::group_by(.,Dispersal_probability) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup()


dispersal_table_decreasing <- extirpation_dispersal_decreasing %>% 
  dplyr::select(., Dispersal_probability, n) %>% 
  dplyr::rename(., Extirpations = n) %>% 
  dplyr::mutate(., Surviving = reps - Extirpations)# %>% 
# dplyr::mutate(., Odds = Extirpations/Surviving) %>% 
# dplyr::select(., -n, -Extirpations, - Surviving)  %>% 
# dplyr::mutate(., DecreasingHighNo  = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal No Restoration"], nrow(test))) %>% 
# dplyr::mutate(., DecreasingHighYes = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal Restoration"], nrow(test))) %>% 
# dplyr::mutate(., )

dispersal <- as.character(dispersal_table_decreasing$Dispersal_probability)
outcomes <- c("Extirpation", "Surviving")

(dat <- matrix(c(as.numeric(dispersal_table_decreasing$Extirpations), as.numeric(dispersal_table_decreasing$Surviving)), byrow = F, nrow = nrow(dispersal_table_decreasing)))
dimnames(dat) <- list("Dispersal" = dispersal, "Outcome" = outcomes)

dat <- as.data.frame(dat) %>% 
  dplyr::arrange(., Surviving)

(dispersal_Current_ORs_decreasing <- fisher.test(dat))

dispersal_Current_ORs_decreasing <- data.frame(OR = dispersal_Current_ORs_decreasing$estimate, 
                                               LL = dispersal_Current_ORs_decreasing$conf.int[1], 
                                               UL = dispersal_Current_ORs_decreasing$conf.int[2]) %>% 
  dplyr::mutate(., Comparison = "Low/high") %>% 
  dplyr::mutate(., Period = "Current") %>% 
  dplyr::mutate(., FireReturnInterval = "Decreasing fire return interval") %>% 
  dplyr::mutate(., DispersalProbability = "All")

#---------------------------------------|---------------------------------------
#                                  Dispersal Decreasing
#  Does dispersal probability, across all fire frequencies (fire sizes) change
# odds of extirpation
#---------------------------------------|---------------------------------------

extirpation_dispersal_historic <- extinct %>% 
  dplyr::filter(., Period == "Current" & Fire_scenario == "Historical fire return interval") %>% 
  # filter(., t <= 150) %>%
  dplyr::group_by(.,Dispersal_probability) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup()


dispersal_table_historic <- extirpation_dispersal_historic %>% 
  dplyr::select(., Dispersal_probability, n) %>% 
  dplyr::rename(., Extirpations = n) %>% 
  dplyr::mutate(., Surviving = reps - Extirpations)# %>% 
# dplyr::mutate(., Odds = Extirpations/Surviving) %>% 
# dplyr::select(., -n, -Extirpations, - Surviving)  %>% 
# dplyr::mutate(., DecreasingHighNo  = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal No Restoration"], nrow(test))) %>% 
# dplyr::mutate(., DecreasingHighYes = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal Restoration"], nrow(test))) %>% 
# dplyr::mutate(., )

dispersal <- as.character(dispersal_table_historic$Dispersal_probability)
outcomes <- c("Etirpation", "Surviving")

(dat <- matrix(c(as.numeric(dispersal_table_historic$Extirpations), as.numeric(dispersal_table_historic$Surviving)), byrow = F, nrow = nrow(dispersal_table_historic)))
dimnames(dat) <- list("Dispersal" = dispersal, "Outcome" = outcomes)

dat <- as.data.frame(dat) %>% 
  dplyr::arrange(., Surviving)

(dispersal_Current_ORs_historic <- fisher.test(dat))



dispersal_Current_ORs_historic <- data.frame(OR = dispersal_Current_ORs_historic$estimate, 
                                             LL = dispersal_Current_ORs_historic$conf.int[1], 
                                             UL = dispersal_Current_ORs_historic$conf.int[2]) %>% 
  dplyr::mutate(., Comparison = "Low/high") %>% 
  dplyr::mutate(., Period = "Current") %>% 
  dplyr::mutate(., FireReturnInterval = "Historical fire return interval") %>% 
  dplyr::mutate(., DispersalProbability = "All")


#---------------------------------------|---------------------------------------
#                                  High dispersal
#---------------------------------------|---------------------------------------

extirpation_highDispersal_fire <- extinct %>% 
  dplyr::filter(., Period == "Current" & Dispersal_probability == "High dispersal") %>% 
  # dplyr::filter(., t <= 150) %>%
  dplyr::group_by(.,Fire_scenario) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup()


fire_high_dispersal_table <- extirpation_highDispersal_fire %>% 
  dplyr::select(., Fire_scenario, n) %>% 
  dplyr::rename(., Extirpations = n) %>% 
  dplyr::mutate(., Surviving = reps - Extirpations)# %>% 
# dplyr::mutate(., Odds = Extirpations/Surviving) %>% 
# dplyr::select(., -n, -Extirpations, - Surviving)  %>% 
# dplyr::mutate(., DecreasingHighNo  = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal No Restoration"], nrow(test))) %>% 
# dplyr::mutate(., DecreasingHighYes = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal Restoration"], nrow(test))) %>% 
# dplyr::mutate(., )

fire <- as.character(fire_high_dispersal_table$Fire_scenario)
outcomes <- c("Etirpation", "Surviving")

(dat <- matrix(c(as.numeric(fire_high_dispersal_table$Extirpations), as.numeric(fire_high_dispersal_table$Surviving)), byrow = F, nrow = nrow(fire_high_dispersal_table)))
dimnames(dat) <- list("Fire" = fire, "Outcome" = outcomes)

(dec_his_fire_high_dispersal <- fisher.test(dat[c(1,2),]))
(dec_sup_fire_high_dispersal <- fisher.test(dat[c(1,3),]))
(his_sup_fire_high_dispersal <- fisher.test(dat[c(2,3),]))


fire_high_dispersal_ORs <- data.frame(OR = c(dec_his_fire_high_dispersal$estimate   , dec_sup_fire_high_dispersal$estimate,    his_sup_fire_high_dispersal$estimate), 
                                      LL = c(dec_his_fire_high_dispersal$conf.int[1], dec_sup_fire_high_dispersal$conf.int[1], his_sup_fire_high_dispersal$conf.int[1]), 
                                      UL = c(dec_his_fire_high_dispersal$conf.int[2], dec_sup_fire_high_dispersal$conf.int[2], his_sup_fire_high_dispersal$conf.int[2])) %>% 
  dplyr::mutate(., Comparison = c("Decreasing/historical", "Decreasing/suppression", "Historical/suppression")) %>% 
  dplyr::mutate(., Period = "Current") %>% 
  dplyr::mutate(., FireReturnInterval = "All") %>% 
  dplyr::mutate(., DispersalProbability = "High dispersal")


#---------------------------------------|---------------------------------------
#                                  Low dispersal
#---------------------------------------|---------------------------------------

extirpation_lowDispersal_fire <- extinct %>% 
  dplyr::filter(., Period == "Current" & Dispersal_probability == "Low dispersal") %>% 
  # filter(., t <= 150) %>%
  dplyr::group_by(.,Fire_scenario) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup()


fire_low_dispersal_table <- extirpation_lowDispersal_fire %>% 
  dplyr::select(., Fire_scenario, n) %>% 
  dplyr::rename(., Extirpations = n) %>% 
  dplyr::mutate(., Surviving = reps - Extirpations)# %>% 
# dplyr::mutate(., Odds = Extirpations/Surviving) %>% 
# dplyr::select(., -n, -Extirpations, - Surviving)  %>% 
# dplyr::mutate(., DecreasingHighNo  = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal No Restoration"], nrow(test))) %>% 
# dplyr::mutate(., DecreasingHighYes = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal Restoration"], nrow(test))) %>% 
# dplyr::mutate(., )

fire <- as.character(fire_low_dispersal_table$Fire_scenario)
outcomes <- c("Etirpation", "Surviving")

(dat <- matrix(c(as.numeric(fire_low_dispersal_table$Extirpations), as.numeric(fire_low_dispersal_table$Surviving)), byrow = F, nrow = nrow(fire_low_dispersal_table)))
dimnames(dat) <- list("Fire" = fire, "Outcome" = outcomes)

(dec_his_fire_low_dispersal <- fisher.test(dat[c(1,2),]))
(dec_sup_fire_low_dispersal <- fisher.test(dat[c(1,3),]))
(his_sup_fire_low_dispersal <- fisher.test(dat[c(2,3),]))



fire_low_dispersal_ORs <- data.frame(OR = c(dec_his_fire_low_dispersal$estimate,   dec_sup_fire_low_dispersal$estimate, his_sup_fire_low_dispersal$estimate), 
                                     LL = c(dec_his_fire_low_dispersal$conf.int[1],dec_sup_fire_low_dispersal$conf.int[1], his_sup_fire_low_dispersal$conf.int[1]),  
                                     UL = c(dec_his_fire_low_dispersal$conf.int[2],dec_sup_fire_low_dispersal$conf.int[2], his_sup_fire_low_dispersal$conf.int[2])) %>% 
  dplyr::mutate(., Comparison = "Decreasing/Historic") %>% 
  dplyr::mutate(., Period = "Current") %>% 
  dplyr::mutate(., FireReturnInterval = "All") %>% 
  dplyr::mutate(., DispersalProbability = "Low dispersal")


# #---------------------------------------|---------------------------------------
# #                           Current Decreaseing dispersal
# #---------------------------------------|---------------------------------------
# 
# extirpation_150_decreasing_dispersal <- extinct %>% 
#   dplyr::filter(., Period == "Current" & Fire_scenario == "Decreasing fire return interval") %>% 
#   dplyr::filter(., t <= 150) %>% 
#   dplyr::group_by(.,Dispersal_probability) %>% 
#   dplyr::tally() %>% 
#   dplyr::ungroup()
# 
# 
# dispersal_decreasing <- extirpation_150_decreasing_dispersal %>% 
#   dplyr::select(., Dispersal_probability, n) %>% 
#   dplyr::rename(., Extirpations = n) %>% 
#   dplyr::mutate(., Surviving = reps - Extirpations)# %>% 
# # dplyr::mutate(., Odds = Extirpations/Surviving) %>% 
# # dplyr::select(., -n, -Extirpations, - Surviving)  %>% 
# # dplyr::mutate(., DecreasingHighNo  = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal No Restoration"], nrow(test))) %>% 
# # dplyr::mutate(., DecreasingHighYes = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal Restoration"], nrow(test))) %>% 
# # dplyr::mutate(., )
# 
# dispersal <- as.character(dispersal_decreasing$Dispersal_probability)
# outcomes <- c("Etirpation", "Surviving")
# 
# (dat <- matrix(c(as.numeric(dispersal_decreasing$Extirpations), as.numeric(dispersal_decreasing$Surviving)), byrow = F, nrow = nrow(dispersal_decreasing)))
# dimnames(dat) <- list("Dispersal" = dispersal, "Outcome" = outcomes)
# 
# (dispersal_decreasing_ORs <- fisher.test(dat))
# 
# 
# dispersal_decreasing_ORs <- data.frame(OR = dispersal_decreasing_ORs$estimate, 
#                                       LL = dispersal_decreasing_ORs$conf.int[1], 
#                                       UL = dispersal_decreasing_ORs$conf.int[2]) %>% 
#   dplyr::mutate(., Comparison = "High/Low") %>% 
#   dplyr::mutate(., FireReturnInterval = "Decreasing") %>% 
#   dplyr::mutate(., DispersalProbability = "All")

#---------------------------------------|---------------------------------------
#                                 Create OR Frame
#---------------------------------------|---------------------------------------

ORPattern <- grep("_OR",names(.GlobalEnv),value=TRUE)
ORs <- rbindlist(mget((ORPattern)))

ORs <- ORs %>% 
  dplyr::mutate(., OddsRatio = ifelse(OR <1, 1/OR, OR)) %>% 
  dplyr::mutate(., LL_ = ifelse(OR <1, 1/UL, LL)) %>% 
  dplyr::mutate(., UL_ = ifelse(OR <1, 1/LL, UL))%>% 
  dplyr::mutate(., Comparison = ifelse(OR < 1, paste0(Comparison, "*"), Comparison)) %>% 
  dplyr::select(., Comparison, Period, FireReturnInterval, DispersalProbability, OddsRatio, LL_, UL_) %>% 
  dplyr::rename(., UL = UL_, LL = LL_)



OR_table <- ORs %>%
  dplyr::mutate(., OR = paste0(round(OddsRatio, 3), "(", round(LL,3), ", ",round(UL,3),")" )) %>% 
  dplyr::select(., -LL, -UL, -OddsRatio) %>% 
  dplyr::arrange(., FireReturnInterval, DispersalProbability, Comparison)




write.csv(OR_table, file = paste0(proj_dir, export_results_dir, "OR table.csv"))

# tiff(filename = paste0(proj_dir, figures_dir, "Final? OR by category fire dispersal restoration.tiff"),
#      width = 12,
#      height = 8.5,
#      units = "in",
#      res = 300,
#      compression = "lzw")

ggplot2::ggplot(data = ORs, ggplot2::aes(x = DispersalProbability, y = OddsRatio))+
  ggplot2::geom_hline(yintercept = 1, col = "black", lty = 2) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = LL, ymax = UL), lwd = 1.5,
                 position = pd) +
  ggplot2::geom_point(aes(x = DispersalProbability,
                      y = OddsRatio),
                      size = 4,
                      stroke = 2,
                  # colour=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'),
                  position = pd ,
                  shape=21, 
                  fill= "white") +
                 # scale_color_manual(values = c( "#e41a1c","#377eb8","#4daf4a" ))+
  ggplot2::theme_bw() +
  ggplot2::facet_grid(Period~ Comparison)+
  ggplot2::theme(strip.background = ggplot2::element_rect(colour = "black", fill = "white"),
                 strip.text       = ggplot2::element_text(colour = "black",  size = strip_text_size)) +
  ggplot2::labs(x = "Time Horizon",
                y = "Probability of Extirpation") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::labs(x = "",
                y = "Odds Ratio") +
  ggplot2::theme(axis.title  = ggplot2::element_text(size = axis_label_text_size)) +
  ggplot2::theme(axis.text   = ggplot2::element_text(size = axis_text_size)) +
  ggplot2::theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines")) # top, right, bottom, left

# dev.off()


################################################################################
#---------------------------------------|---------------------------------------
#                           Logistic regression
#---------------------------------------|---------------------------------------
################################################################################

extirpation_lr <- extinct %>% 
  ggplot2::mutate(., Extirpated = 1) %>% 
  ggplot2::select(., Iteration, t, Fire_scenario, Dispersal_probability, Period, Extirpated) %>% 
  tidyr::complete(.,Iteration = unique(extinct$Iteration),
           t = 1:500,
           Fire_scenario         = levels(extinct$Fire_scenario), 
           Dispersal_probability = levels(extinct$Dispersal_probability),
           Period                = levels(extinct$Period), 
           fill = list(Extinct = 0))


################################################################################
#---------------------------------------|---------------------------------------
#                           Density time since fire
#---------------------------------------|---------------------------------------
################################################################################

TSF_suppression <- current_models %>% 
  dplyr::filter(., t == 200 & !Fire_scenario == "Decreasing fire return interval"|
                  t == 300 & !Fire_scenario == "Decreasing fire return interval"|
                  t == 400 & !Fire_scenario == "Decreasing fire return interval"|
                  t == 500 & !Fire_scenario == "Decreasing fire return interval") %>% 
  dplyr::group_by(., t, Fire_scenario, Dispersal_probability) %>% 
  dplyr::summarise_at(., dplyr::vars(Density, SAP_Density, MA_Density, SAP_MA_Density), dplyr::funs(q1, median, q3))

ggplot2::ggplot(TSF_suppression, 
                dplyr::aes(x   = as.character(t), 
                           y   = SAP_MA_Density_median, 
                           col = Fire_scenario))+
  ggplot2::geom_linerange(data = TSF_suppression, 
                          ggplot2::aes(x    = as.character(t), 
                                       ymin = SAP_MA_Density_q1, 
                                       ymax = SAP_MA_Density_q3),
                          position = pd)+
  ggplot2::geom_point(data = TSF_suppression, 
                      ggplot2::aes(x = as.character(t), 
                                   y = SAP_MA_Density_median), 
                      position = pd)+
  ggplot2::facet_grid(~ Dispersal_probability)

################################################################################
#---------------------------------------|---------------------------------------
#                           Density at year 500
#---------------------------------------|---------------------------------------
################################################################################
not <- extinct %>% 
  dplyr::mutate(., Unique = IterationTime, Fire_scenario, Dispersal_probability)

year500densityTable <- current_models %>% 
  dplyr::mutate(., Unique = IterationTime, Fire_scenario, Dispersal_probability) %>% 
  # dplyr::filter(., !(Unique %in% not$Unique)) %>%
  dplyr::filter(., t == 500) %>%
  dplyr::group_by(., Fire_scenario, Dispersal_probability) %>%
  dplyr::summarise_at(., dplyr::vars(Density), dplyr::funs(min, lower, q1, median, mean, q3, upper, max)) %>%
  dplyr::ungroup()

year500density <- current_models %>% 
  dplyr::mutate(., Panel = ifelse(Fire_scenario == "Decreasing fire return interval", "One", "Two")) %>% 
  dplyr::mutate(., Unique = IterationTime, Fire_scenario, Dispersal_probability) %>% 
  # dplyr::filter(., !(Unique %in% not$Unique)) %>%
  dplyr::filter(., t == 500)


tiff(filename = paste0(proj_dir, figures_dir, "Final Metapopulation density 500 years.tiff"),
     width = 12,
     height = 8.5,
     units = "in",
     res = 300,
     compression = "lzw")

year500density %>%
  dplyr::filter(., Density < 0.003) %>%  #& Fire_scenario != "Decreasing fire return interval") %>%
  # dplyr::filter(., Fire_scenario != "Decreasing fire return interval") %>% 
  dplyr::mutate(., Fire_scenario = ifelse(Fire_scenario=='Decreasing fire return interval', 'Decreasing',
                                   ifelse(Fire_scenario=='Historic fire return interval', 'Historical',
                                   ifelse(Fire_scenario=='Suppression', 'Suppression', NA)))) %>%
  ggplot2::ggplot( aes(x = Density, fill = Fire_scenario)) +
  # ggplot2::geom_linerange( ggplot2::aes(x = Dispersal_probability, ymin = min, ymax = max, col = Dispersal_probability),
  #                position = ggplot2::position_dodge(width = 0.4), lwd = 1.5) +
  # ggplot2::scale_colour_manual(values = c("#ef8a62", "#67a9cf")) +
  # ggplot2::geom_point(ggplot2::aes(x = Dispersal_probability, y = median, col = Dispersal_probability),
  #            position = ggplot2::position_dodge(width = 0.4), stroke = 2, pch = 21, fill = "white", size = 3)+
  # ggplot2::geom_histogram(ggplot2::aes(fill = Dispersal_probability), alpha = 0.5, binwidth = .001)+
  ggplot2::geom_density(ggplot2::aes(fill = Fire_scenario), alpha = 0.4)+
  ggplot2::scale_fill_manual(values =  c("#d73027", "#1a9850", "#fee08b")) +
  ggplot2::theme_bw() +
  ggplot2::facet_grid(Panel~Dispersal_probability, scales = "free") +
  ggplot2::labs(x = expression(paste("\nTree density (no. trees/hectare) at year 500")), y = "Probability density")+
  ggplot2::theme(axis.text        = ggplot2::element_text(size = axis_text_size - 2)) +
  ggplot2::theme(axis.title       = ggplot2::element_text(size = axis_label_text_size))+
  ggplot2::theme(strip.text       = ggplot2::element_text(size = strip_text_size),
                # strip.text.y = element_blank(),
                 strip.background = ggplot2::element_rect(fill = "white"))+
  # ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
  # ggplot2::theme(axis.text.x  = ggplot2::element_blank()) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::theme(plot.margin = grid::unit(c(2.5, 1, 1, 1), "lines"))+  # top, right, bottom, left
  ggplot2::facet_grid(Fire_scenario~Dispersal_probability, scales = "free_y")

# grid::grid.text("Extirpation threshold",
#             x = grid::unit(0.88, "npc"),
#             y = grid::unit(0.15, "npc"),
#             just = "centre",
#             hjust = 0,
#             vjust = NULL,
#             rot = 0,
#             check.overlap = FALSE,
#             default.units = "npc",
#             name = NULL,
#             gp = grid::gpar(cex=1.2, fontsize=11),
#             draw = TRUE,
#             vp = NULL)

# legd <- grid::legendGrob(c("Decreasing fire return interval","Historical fire return interval", "Suppression"),
#                          nrow          = 1,
#                          ncol          = 3,
#                          lines.first   = TRUE,
#                          hgap          = grid::unit(2, "lines"),
#                          vgap          = grid::unit(1, "lines"),
#                          default.units = "lines",
#                          pch           = 22,
#                          gp = grid::gpar(col      = c("black","black", "black"),
#                                          fill     = c("#d73027", "#1a9850", "#fee08b"),
#                                          alpha =  c(1, 0.4),
#                                          fontsize = axis_text_size,
#                                          fontface = "bold"),
#                          vp = grid::viewport(x    = 0,
#                                              y    = 0,
#                                              w    = 1.05,
#                                              h    = 1.92,
#                                              just = c("left", "bottom")))
# grid::grid.draw(legd)

dev.off()
  

write.csv(year500density, file = paste0(proj_dir, export_table_dir, "500 year density.csv"))

#####################################################################################
#              Comparison to FIA data
################################################################################

load( file = paste0(proj_dir, data_dir, "fia density.Rda"))

fia_density <- fia_density %>% 
  mutate(., SAP_MA_density = (sapling_count + mature_count)/1000)


fia.comparison.plot <- 
  ggplot2::ggplot(data = historic_models, ggplot2::aes(x = SAP_MA_Density))+
  ggplot2::geom_density(fill = "#01665e", col = "black", alpha = 0.75) +
  ggplot2::geom_density(data = fia_density, aes(x = SAP_MA_density),
                        fill = "#8c510a", alpha = 0.75, col = "black") +
  # ggplot2::theme_bw() +
  ggplot2::labs(x = "\nTree density (no. trees/hectare)", 
                y = "Probability density")+
  ggplot2::theme(axis.text        = ggplot2::element_text(size = axis_text_size - 2)) +
  ggplot2::theme(axis.title       = ggplot2::element_text(size = axis_label_text_size))+
  ggplot2::theme(legend.position = "none") +
  ggplot2::theme(plot.margin = grid::unit(c(2.5, 1, 1, 1), "lines"))

legd <- grid::legendGrob(c("Predicted historical time period", "FIA Data"),
                         nrow          = 1,
                         ncol          = 2,
                         lines.first   = TRUE,
                         hgap          = grid::unit(2, "lines"),
                         vgap          = grid::unit(1, "lines"),
                         default.units = "lines",
                         pch           = 22,
                         gp = grid::gpar(col      = rep(c("black"), 2),
                                         fill     = c("#01665e", "#8c510a"),
                                         fontsize = axis_text_size,
                                         fontface = "bold"),
                         vp = grid::viewport(x    = 0,
                                             y    = 0,
                                             w    = 1.05,
                                             h    = 1.92,
                                             just = c("left", "bottom")))
grid::grid.draw(legd)



#####################################################################################
#               Comparison to early succession data 
################################################################################

load("/Users/elizabethpansing/Box/Yellowstone/88-Fires-Analysis/2017 YNP Data.Rda")

hm <- pial %>% 
  dplyr::filter(., StudyArea == "HM") %>% 
  dplyr::mutate(., ifelse(Year == 2016, 2017, Year)) %>% 
  dplyr::select(.,   Year, PlotNo) %>% 
  dplyr::group_by(., Year, PlotNo) %>% 
  dplyr::tally(.) %>% 
  dplyr::ungroup(.) %>% 
  tidyr::complete(.,Year                = c(1990, 1991, 1992, 1994, 1995, 2001, 2017), 
                  PlotNo                = 1:150,
                  fill                  = list(n = 0)) %>% 
  dplyr::mutate(., Density = n/20 * 10000) 


mw <- pial %>% 
  dplyr::filter(., StudyArea == "MW") %>% 
  dplyr::select(., Year, PlotNo) %>% 
  dplyr::group_by(.,Year, PlotNo) %>% 
  dplyr::tally(.) %>% 
  dplyr::ungroup(.) %>% 
  tidyr::complete(.,Year                = c(1990, 1991, 1992, 1994, 1995, 2001, 2017), 
                  PlotNo                = c(200:300, 330:354),
                  fill                  = list(n = 0)) %>% 
  dplyr::mutate(., Density = n/20 * 10000) %>% 
  dplyr::filter(., !(Year == 2017 & PlotNo > 300))


recovery88 <- bind_rows(hm, mw) %>% 
  dplyr::group_by(., Year) %>% 
  dplyr::summarise_at(., vars(Density), dplyr::funs(min, max, median, mean)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., tSinceFire = Year - 1988) 


names(recovery88)<- paste0("observed.", names(recovery88))
names(year30density) <- paste0("predicted.", names(year30density))

comparison_data <- year30density %>% 
  dplyr::filter(., predicted.tSinceFire %in% recovery88$observed.tSinceFire) %>% 
  merge(., recovery88 , by.x = "predicted.tSinceFire", by.y = "observed.tSinceFire")


inset_figure <- comparison_data %>%   
  ggplot2::ggplot(aes(y = predicted.Density_mean, x = observed.mean)) +
  ggplot2::geom_linerange(aes(x = observed.mean, 
                              ymin = predicted.Density_lower,
                              ymax = predicted.Density_upper), lwd = 1.25) +
  ggplot2::geom_point(aes(y = predicted.Density_mean, x = observed.mean), pch = 21, fill = "white", size = 3, stroke = 1.5) +
  scale_y_continuous(limits = c(0, 1100)) +
  scale_x_continuous(limits = c(0, 1100)) +
  geom_abline(slope=1, intercept=0, lty = 2) +
  coord_flip() +
  theme_bw() +
  labs(x = "Predicted tree \ndensity (trees/ha)", y = "Observed tree \ndensity (trees/ha)")+
  ggplot2::theme(axis.text        = ggplot2::element_text(size = 18)) +
  ggplot2::theme(axis.title       = ggplot2::element_text(size = 20))
  

vp <- grid::viewport(width = 0.4, height = 0.4, x = 0.98,
               y = 0.93, just = c("right",
                                                "top"))

full <- function() {
  print(fia.comparison.plot)
  theme_set(theme_bw(base_size = 8))
  print(inset_figure, vp = vp)
  theme_set(theme_bw())
}

tiff(filename = paste0(proj_dir, figures_dir, "Model accuracy check figure.tiff"),
     width = 12,
     height = 8.5,
     units = "in",
     res = 300,
     compression = "lzw")

full()
grid::grid.draw(legd)

dev.off()

