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

reps <- 5000

axis_label_text_size <- 23
strip_text_size <- 18
axis_text_size <- 18

# area <- 2000 # Area of 2000 ha total, 1000ha/population. Multiply by 10e3 to get m2

################################################################################
#---------------------------------------|---------------------------------------
#                                   Load model 
#---------------------------------------|---------------------------------------
################################################################################
# The sourced code is only required for the first model run. Otherwise skip to line 130

# source(paste0(proj_dir, code_dir, model_file))


################################################################################
#---------------------------------------|---------------------------------------
#                             Run Model Scenarios   
#---------------------------------------|---------------------------------------
################################################################################
################################################################################

# The following runs 6 projection scenarios for wbp: 3 fire
# scenarios crossed with 2 dispersal probabilities
#    1) Constant current fire return interval
#       long distance between populations 
#       (i.e., low dispersal probabiliy)
#    2) Constant current fire return interval
#       short distance between populations 
#       (i.e., high dispersal probabiliy)
#    3) Complete fire suppression
#       long distance between populations 
#       (i.e., low dispersal probabiliy)
#    4) Complete fire suppression
#       long distance between populations 
#       (i.e., low dispersal probabiliy)
#    5) Decreasing fire return interval
#       long distance between populations 
#       (i.e., low dispersal probabiliy)
#    6) Decreasing fire return interval
#       long distance between populations 
#       (i.e., low dispersal probabiliy)
#
#
#  ** Note: Lines 95-125 only need to be run initially. After initial run
#           jump to line 1230

projection_time <- 500
reps <- 5000

# # 1)
start.time1 <- Sys.time()
constant_high_distance_revision   <- project(projection_time = projection_time, reps = reps, FRI_decrease = FALSE,
                                     fire = TRUE, dispersal_distance = "High", period = "Current")
end.time1 <- Sys.time()

save(constant_high_distance_revision, file = paste0(proj_dir, rda_dir, "Revision constant high distance.Rda"))

#2)
start.time2 <- Sys.time()
constant_low_distance_revision    <- project(projection_time = projection_time,  reps = reps, FRI_decrease = FALSE,
                                     fire = TRUE, dispersal_distance = "Low", period = "Current")
end.time2 <- Sys.time()
save(constant_low_distance_revision,  file = paste0(proj_dir, rda_dir, "Revision constant low distance.Rda"))

#3)
start.time3 <- Sys.time()
suppression_high_distance_revision <- project(projection_time = projection_time,  reps = reps, FRI_decrease = FALSE,
                                     fire = FALSE, dispersal_distance = "High", period = "Current")
end.time3 <- Sys.time()
save(suppression_high_distance_revision, file = paste0(proj_dir, rda_dir,"Revision suppression high distance.Rda"))

#4)
start.time4 <- Sys.time()
suppression_low_distance_revision    <- project(projection_time = projection_time,  reps = reps, FRI_decrease = FALSE,
                                     fire = FALSE, dispersal_distance = "Low", period = "Current")
end.time4 <- Sys.time()
save(suppression_low_distance_revision,  file = paste0(proj_dir, rda_dir, "Revision suppression low distance.Rda"))

##5)
start.time5 <- Sys.time()
decrease_high_distance_revision      <- project(projection_time = projection_time,  reps = reps, FRI_decrease = TRUE,
                                     fire = TRUE, dispersal_distance = "High", period = "Current")
end.time5 <- Sys.time()
save(decrease_high_distance_revision, file = paste0(proj_dir, rda_dir, "Revision decrease high distance.Rda"))

#6)
start.time6 <- Sys.time()
decrease_low_distance_revision       <- project(projection_time = projection_time,  reps = reps, FRI_decrease = TRUE,
                                     fire = TRUE, dispersal_distance = "Low", period = "Current")
end.time6 <- Sys.time()
save(decrease_low_distance_revision, file = paste0(proj_dir, rda_dir, "Revision decrease low distance.Rda"))

# 7)
start.time7 <- Sys.time()
constant_high_distance_historic_revision  <- project(projection_time = projection_time, reps = reps, FRI_decrease = FALSE,
                                          fire = TRUE, dispersal_distance = "High", period = "Historic")
end.time7 <- Sys.time()
save(constant_high_distance_historic_revision, file = paste0(proj_dir, rda_dir, "Revision constant high distance historical.Rda"))

# 8)
start.time8 <- Sys.time()
constant_low_distance_historic_revision   <- project(projection_time = projection_time ,  reps = reps, FRI_decrease = FALSE,
                                          fire = TRUE, dispersal_distance = "Low", period = "Historic")
end.time8 <- Sys.time()
save(constant_low_distance_historic_revision, file = paste0(proj_dir, rda_dir, "Revision constant low distance historical.Rda"))
total.end <- Sys.time()

cat(paste0("Model run began at ", start.time1, " and concluded at ", end.time1, "\n\n"))
cat(paste0("Model run began at ", start.time2, " and concluded at ", end.time2, "\n\n"))
cat(paste0("Model run began at ", start.time3, " and concluded at ", end.time3, "\n\n"))
cat(paste0("Model run began at ", start.time4, " and concluded at ", end.time4, "\n\n"))
cat(paste0("Model run began at ", start.time5, " and concluded at ", end.time5, "\n\n"))
cat(paste0("Model run began at ", start.time6, " and concluded at ", end.time6, "\n\n"))
cat(paste0("Model run began at ", start.time7, " and concluded at ", end.time7, "\n\n"))
cat(paste0("Model run began at ", start.time8, " and concluded at ", end.time8, "\n\n"))


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


area <- 2000
################################################################################
#---------------------------------------|---------------------------------------
#                                Look at trajectories  
#---------------------------------------|---------------------------------------
################################################################################

# 1) Constant High Distance
#---------------------------------------|---------------------------------------
#                           Constant FRI, high distance 
#---------------------------------------|---------------------------------------

constant_high_distance_pop_size_total <- constant_high_distance_revision$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  dplyr::mutate(., PopSize               = rowSums(dplyr::select(., dplyr::contains("_")))) %>% 
  dplyr::mutate(., PopSize1              = rowSums(dplyr::select(., dplyr::contains("_1")))) %>% 
  dplyr::mutate(., PopSize2              = rowSums(dplyr::select(., dplyr::contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MaturePopSize         = rowSums(dplyr::select(., dplyr::contains("MA_")))) %>% 
  dplyr::mutate(., MaturePopSize1        = rowSums(dplyr::select(., dplyr::contains("MA_1")))) %>% 
  dplyr::mutate(., MaturePopSize2        = rowSums(dplyr::select(., dplyr::contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingPopSize        = rowSums(dplyr::select(., dplyr::contains("SAP_")))) %>% 
  dplyr::mutate(., SaplingPopSize1       = rowSums(dplyr::select(., dplyr::contains("SAP_1")))) %>% 
  dplyr::mutate(., SaplingPopSize2       = rowSums(dplyr::select(., dplyr::contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., dplyr::matches("MA_|SAP_")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., dplyr::matches("MA_1|SAP_1")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., dplyr::matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  dplyr::mutate(., Density = PopSize/area)  %>% 
  dplyr::mutate(., Density1 = PopSize1/(area/2))  %>% 
  dplyr::mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MA_Density = MaturePopSize/area) %>% 
  dplyr::mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_Density = SaplingPopSize/area) %>% 
  dplyr::mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  dplyr::mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  dplyr::mutate(., Fire_scenario = "Historical fire return interval") %>% 
  dplyr::mutate(., Dispersal_probability = "Low dispersal") %>% 
  dplyr::mutate(., Period = "Current") %>%
  dplyr::mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(constant_high_distance_revision$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  dplyr::mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, -t)

constant_high_distance_pop_size_total <- dplyr::full_join(constant_high_distance_pop_size_total, tSinceFire, by = "IterationTime")%>% 
  dplyr::mutate(., t_burnin = t) %>% 
  dplyr::mutate(., t = t - 10) %>% 
  dplyr::filter(., t > 0)

pop1 <- constant_high_distance_pop_size_total %>% 
  dplyr::select(., Iteration, dplyr::contains("1")) %>% 
  dplyr::mutate(., Pop = "pop1")  %>% 
  dplyr::mutate(., Dispersal_probability = "High")

colnames(pop1) <- c("Iteration","CS", "SD", "SAP", "MA", "PopSize", "MaturePopSize", "SaplingPopSize", "SaplingMaturePopSize",
                    "Density", "MA_Density", "SAP_Density", "SAP_MA_Density", "tSinceFire", "Pop", "Dispersal_probability")

pop2 <- constant_high_distance_pop_size_total %>% 
  dplyr::select(., Iteration, dplyr::contains("2")) %>% 
  dplyr::mutate(., Pop = "pop2") %>% 
  dplyr::mutate(., Dispersal_probability = "High")

colnames(pop2) <- c("Iteration", "CS", "SD", "SAP", "MA", "PopSize", "MaturePopSize", "SaplingPopSize", "SaplingMaturePopSize",
                    "Density", "MA_Density", "SAP_Density", "SAP_MA_Density", "tSinceFire", "Pop", "Dispersal_probability")

pops <- dplyr::bind_rows( pop1, pop2)

rm(pop1, pop2)

density_tSinceFire <- pops %>% 
  dplyr::group_by(., tSinceFire, Dispersal_probability) %>% 
  dplyr::summarise_at(., dplyr::vars(MA_Density, SAP_MA_Density, Density), 
               list(mean = mean, median = median, min = min, 
                    lower = lower, q1 = q1, q3 = q3, upper = upper, max = max)) %>% 
  dplyr::ungroup() 


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

rm(constant_high_distance_revision)

density_constant_high_ditance_revision <- constant_high_distance_pop_size_total %>%
  dplyr::select(., Iteration, t, Density, Density1, Density2,
                MA_Density, MA_Density1, MA_Density2,
                SAP_Density, SAP_Density1, SAP_Density2,
                SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2) %>%
  tidyr::gather(., LifeStage_pop, Density, -Iteration, -t)
# 
# 
# density_constant_high_ditance_revision %>%
#   # dplyr::filter(., t < 20) %>%
#   dplyr::filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
#   ggplot2::ggplot(., ggplot2::aes(x = t, y = Density, col = Iteration))+
#   ggplot2::geom_line()+
#   ggplot2::theme(legend.position = "none") +
#   ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# 
# summary_constant_high_distance_density_by_t <- density_constant_high_distance %>%
#   dplyr::group_by(., t, LifeStage_pop) %>%
#   dplyr::summarise_at(., dplyr::vars(Density), dplyr::funs(mean, median, min, max)) %>%
#   dplyr::ungroup()
# 
# summary_constant_high_distance_density_by_t %>%
#   ggplot2::ggplot(., ggplot2::aes(x = t, y = median))+
#   ggplot2::geom_line()+
#   ggplot2::geom_ribbon(ggplot2::aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# 
# (summary_constant_high_distance_density <- summary_constant_high_distance_density_by_t %>%
#     dplyr::group_by(., LifeStage_pop) %>%
#     dplyr::summarise_at(., dplyr::vars(mean, median), dplyr::funs(mean)) %>%
#     dplyr::ungroup())
# 
# write.csv(summary_constant_high_distance_density, 
#           file = paste0(proj_dir, export_table_dir, "constant high no restoration density.csv"))
# 
# rm(summary_constant_high_distance_density_by_t, summary_constant_high_distance_density, density_constant_high_distance)

# constant_high_distance_pop_size_total <- constant_high_distance_pop_size_total %>% 
# dplyr::select(., Iteration, t, Density) #, Density1, Density2, SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2)


# 2) Constant Low Distance
#---------------------------------------|---------------------------------------
#                         Constant FRI, low distance                   
#---------------------------------------|---------------------------------------

constant_low_distance_pop_size_total <- constant_low_distance_revision$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  dplyr::mutate(., PopSize               = rowSums(dplyr::select(., dplyr::contains("_")))) %>% 
  dplyr::mutate(., PopSize1              = rowSums(dplyr::select(., dplyr::contains("_1")))) %>% 
  dplyr::mutate(., PopSize2              = rowSums(dplyr::select(., dplyr::contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MaturePopSize         = rowSums(dplyr::select(., dplyr::contains("MA_")))) %>% 
  dplyr::mutate(., MaturePopSize1        = rowSums(dplyr::select(., dplyr::contains("MA_1")))) %>% 
  dplyr::mutate(., MaturePopSize2        = rowSums(dplyr::select(., dplyr::contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingPopSize        = rowSums(dplyr::select(., dplyr::contains("SAP_")))) %>% 
  dplyr::mutate(., SaplingPopSize1       = rowSums(dplyr::select(., dplyr::contains("SAP_1")))) %>% 
  dplyr::mutate(., SaplingPopSize2       = rowSums(dplyr::select(., dplyr::contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., dplyr::matches("MA_|SAP_")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., dplyr::matches("MA_1|SAP_1")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., dplyr::matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  dplyr::mutate(., Density = PopSize/area)  %>% 
  dplyr::mutate(., Density1 = PopSize1/(area/2))  %>% 
  dplyr::mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MA_Density = MaturePopSize/area) %>% 
  dplyr::mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_Density = SaplingPopSize/area) %>% 
  dplyr::mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  dplyr::mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  dplyr::mutate(., Fire_scenario = "Historical fire return interval") %>% 
  dplyr::mutate(., Dispersal_probability = "High dispersal") %>% 
  dplyr::mutate(., Period = "Current") %>%
  dplyr::mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(constant_low_distance_revision$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  dplyr::mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, - t)

constant_low_distance_pop_size_total <- dplyr::full_join(constant_low_distance_pop_size_total, tSinceFire, by = "IterationTime")


rm(constant_low_distance_revision)

density_constant_low_distance_revision <- constant_low_distance_pop_size_total %>%
  dplyr::select(., Iteration, t, Density, Density1, Density2,
                MA_Density, MA_Density1, MA_Density2,
                SAP_Density, SAP_Density1, SAP_Density2,
                SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2) %>%
  tidyr::gather(., LifeStage_pop, Density, -Iteration, -t)


# density_constant_low_distance_revision %>%
#   # dplyr::filter(., t < 20) %>%
#   dplyr::filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
#   ggplot2::ggplot(., ggplot2::aes(x = t, y = Density, col = Iteration))+
#   ggplot2::geom_line()+
#   ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)+
#   ggplot2::theme(legend.position = "none")
# 
# 
# summary_constant_low_distance_density_by_t <- density_constant_low_distance %>%
#   dplyr::group_by(., t, LifeStage_pop) %>%
#   dplyr::summarise_at(., vars(Density), vars(mean, median, min, max)) %>%
#   dplyr::ungroup()
# 
# summary_constant_low_distance_density_by_t %>%
#   ggplot2::ggplot(., ggplot2::aes(x = t, y = median))+
#   ggplot2::geom_line()+
#   ggplot2::geom_ribbon(ggplot2::aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# (summary_constant_low_distance_density <- summary_constant_low_distance_density_by_t %>%
#     dplyr::group_by(., LifeStage_pop) %>%
#     dplyr::summarise_at(., dplyr::vars(mean, median), dplyr::funs(mean)) %>%
#     dplyr::ungroup())
# 
# write.csv(summary_constant_low_distance_density, 
#           file = paste0(proj_dir, export_table_dir, "constant low no restoration density.csv"))
# 
# rm(summary_constant_low_distance_density_by_t, summary_constant_low_distance_density, density_constant_low_distance)

# constant_low_distance_pop_size_total <- constant_low_distance_pop_size_total %>% 
#   dplyr::select(., Iteration, t, Density) #, Density1, Density2, SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2)

# 3) Suppression High Distance
#---------------------------------------|---------------------------------------
#                          Suppression FRI, high distance                   
#---------------------------------------|---------------------------------------

suppression_high_distance_pop_size_total <- suppression_high_distance_revision$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  dplyr::mutate(., PopSize               = rowSums(dplyr::select(., dplyr::contains("_")))) %>% 
  dplyr::mutate(., PopSize1              = rowSums(dplyr::select(., dplyr::contains("_1")))) %>% 
  dplyr::mutate(., PopSize2              = rowSums(dplyr::select(., dplyr::contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MaturePopSize         = rowSums(dplyr::select(., dplyr::contains("MA_")))) %>% 
  dplyr::mutate(., MaturePopSize1        = rowSums(dplyr::select(., dplyr::contains("MA_1")))) %>% 
  dplyr::mutate(., MaturePopSize2        = rowSums(dplyr::select(., dplyr::contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingPopSize        = rowSums(dplyr::select(., dplyr::contains("SAP_")))) %>% 
  dplyr::mutate(., SaplingPopSize1       = rowSums(dplyr::select(., dplyr::contains("SAP_1")))) %>% 
  dplyr::mutate(., SaplingPopSize2       = rowSums(dplyr::select(., dplyr::contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., dplyr::matches("MA_|SAP_")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., dplyr::matches("MA_1|SAP_1")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., dplyr::matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  dplyr::mutate(., Density = PopSize/area)  %>% 
  dplyr::mutate(., Density1 = PopSize1/(area/2))  %>% 
  dplyr::mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MA_Density = MaturePopSize/area) %>% 
  dplyr::mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_Density = SaplingPopSize/area) %>% 
  dplyr::mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  dplyr::mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  dplyr::mutate(., Fire_scenario = "Suppression") %>% 
  dplyr::mutate(., Dispersal_probability = "Low dispersal") %>% 
  dplyr::mutate(., Period = "Current") %>%
  dplyr::mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(suppression_high_distance_revision$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  dplyr::mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, - t)

suppression_high_distance_pop_size_total <- dplyr::full_join(suppression_high_distance_pop_size_total, tSinceFire, by = "IterationTime")


rm(suppression_high_distance_revision)

density_suppression_high_ditance_revision <- suppression_high_distance_pop_size_total %>%
  dplyr::select(., Iteration, t, Density, Density1, Density2,
                MA_Density, MA_Density1, MA_Density2,
                SAP_Density, SAP_Density1, SAP_Density2,
                SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2) %>%
  tidyr::gather(., LifeStage_pop, Density, -Iteration, -t)


# density_suppression_high_ditance_revision %>%
#   # dplyr::filter(., t < 20) %>%
#   dplyr::filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
#   ggplot2::ggplot(., ggplot2::aes(x = t, y = Density, col = Iteration))+
#   ggplot2::geom_line()+
#   ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)+
#   ggplot2::theme(legend.position = "none")
# 
# 
# summary_suppression_high_distance_density_by_t <- density_suppression_high_distance %>%
#   dplyr::group_by(., t, LifeStage_pop) %>%
#   dplyr::summarise_at(., dplyr::vars(Density), dplyr::funs(mean, median, min, max)) %>%
#   dplyr::ungroup()
# 
# summary_suppression_high_distance_density_by_t %>%
#   ggplot2::ggplot(., ggplot2::aes(x = t, y = median))+
#   ggplot2::geom_line()+
#   ggplot2::geom_ribbon(ggplot2::aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# 
# (summary_suppression_high_distance_density <- summary_suppression_high_distance_density_by_t %>%
#     dplyr::group_by(., LifeStage_pop) %>%
#     dplyr::summarise_at(., dplyr::vars(mean, median), dplyr::funs(mean)) %>%
#     dplyr::ungroup())
# 
# write.csv(summary_suppression_high_distance_density, 
#           file = paste0(proj_dir, export_table_dir, "suppression high no restoration density.csv"))
# 
# rm(summary_suppression_high_distance_density_by_t, summary_suppression_high_distance_density, density_suppression_high_distance)

# suppression_high_distance_pop_size_total <- suppression_high_distance_pop_size_total %>% 
#   dplyr::select(., Iteration, t, Density) #, Density1, Density2, SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2)


# 4) Suppression Low Distance
#---------------------------------------|---------------------------------------
#                          Suppression FRI, low distance                   
#---------------------------------------|---------------------------------------

suppression_low_distance_pop_size_total <- suppression_low_distance_revision$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  dplyr::mutate(., PopSize               = rowSums(dplyr::select(., dplyr::contains("_")))) %>% 
  dplyr::mutate(., PopSize1              = rowSums(dplyr::select(., dplyr::contains("_1")))) %>% 
  dplyr::mutate(., PopSize2              = rowSums(dplyr::select(., dplyr::contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MaturePopSize         = rowSums(dplyr::select(., dplyr::contains("MA_")))) %>% 
  dplyr::mutate(., MaturePopSize1        = rowSums(dplyr::select(., dplyr::contains("MA_1")))) %>% 
  dplyr::mutate(., MaturePopSize2        = rowSums(dplyr::select(., dplyr::contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingPopSize        = rowSums(dplyr::select(., dplyr::contains("SAP_")))) %>% 
  dplyr::mutate(., SaplingPopSize1       = rowSums(dplyr::select(., dplyr::contains("SAP_1")))) %>% 
  dplyr::mutate(., SaplingPopSize2       = rowSums(dplyr::select(., dplyr::contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., dplyr::matches("MA_|SAP_")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., dplyr::matches("MA_1|SAP_1")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., dplyr::matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  dplyr::mutate(., Density = PopSize/area)  %>% 
  dplyr::mutate(., Density1 = PopSize1/(area/2))  %>% 
  dplyr::mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MA_Density = MaturePopSize/area) %>% 
  dplyr::mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_Density = SaplingPopSize/area) %>% 
  dplyr::mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  dplyr::mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  dplyr::mutate(., Fire_scenario = "Suppression") %>% 
  dplyr::mutate(., Dispersal_probability = "High dispersal") %>% 
  dplyr::mutate(., Period = "Current") %>%
  dplyr::mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(suppression_low_distance_revision$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  dplyr::mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, - t)

suppression_low_distance_pop_size_total <- dplyr::full_join(suppression_low_distance_pop_size_total, tSinceFire, by = "IterationTime")

rm(suppression_low_distance_revision)

density_suppression_low_distance_revision <- suppression_low_distance_pop_size_total %>%
  dplyr::select(., Iteration, t, Density, Density1, Density2,
                MA_Density, MA_Density1, MA_Density2,
                SAP_Density, SAP_Density1, SAP_Density2,
                SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2) %>%
  tidyr::gather(., LifeStage_pop, Density, -Iteration, -t)


# density_suppression_low_distance_revision %>%
#   # dplyr::filter(., t < 20) %>%
#   dplyr::filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
#   ggplot2::ggplot(., ggplot2::aes(x = t, y = Density, col = Iteration))+
#   ggplot2::geom_line()+
#   ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)+
#   ggplot2::theme(legend.position = "none")
# 
# 
# summary_suppression_low_distance_density_by_t <- density_suppression_low_distance %>%
#   group_by(., t, LifeStage_pop) %>%
#   summarise_at(., vars(Density), vars(mean, median, min, max)) %>%
#   ungroup()
# 
# summary_suppression_low_distance_density_by_t %>%
#   ggplot2::ggplot(., ggplot2::aes(x = t, y = median))+
#   ggplot2::geom_line()+
#   ggplot2::geom_ribbon(ggplot2::aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# 
# (summary_suppression_low_distance_density <- summary_suppression_low_distance_density_by_t %>%
#     dplyr::group_by(., LifeStage_pop) %>%
#     dplyr::summarise_at(., dplyr::vars(mean, median), dplyr::funs(mean)) %>%
#     dplyr::ungroup())
# 
# write.csv(summary_suppression_low_distance_density, 
#           file = paste0(proj_dir, export_table_dir, "suppression low no restoration density.csv"))
# 
# rm(summary_suppression_low_distance_density_by_t, summary_suppression_low_distance_density, density_suppression_low_distance)

# suppression_low_distance_pop_size_total <- suppression_low_distance_pop_size_total %>% 
#   dplyr::select(., Iteration, t, Density) #, Density1, Density2, SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2)


# 5) Decreasing High Distance
#---------------------------------------|---------------------------------------
#                          Decreasing FRI, high distance                   
#---------------------------------------|---------------------------------------

decrease_high_distance_pop_size_total <- decrease_high_distance_revision$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  dplyr::mutate(., PopSize               = rowSums(dplyr::select(., dplyr::contains("_")))) %>% 
  dplyr::mutate(., PopSize1              = rowSums(dplyr::select(., dplyr::contains("_1")))) %>% 
  dplyr::mutate(., PopSize2              = rowSums(dplyr::select(., dplyr::contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MaturePopSize         = rowSums(dplyr::select(., dplyr::contains("MA_")))) %>% 
  dplyr::mutate(., MaturePopSize1        = rowSums(dplyr::select(., dplyr::contains("MA_1")))) %>% 
  dplyr::mutate(., MaturePopSize2        = rowSums(dplyr::select(., dplyr::contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingPopSize        = rowSums(dplyr::select(., dplyr::contains("SAP_")))) %>% 
  dplyr::mutate(., SaplingPopSize1       = rowSums(dplyr::select(., dplyr::contains("SAP_1")))) %>% 
  dplyr::mutate(., SaplingPopSize2       = rowSums(dplyr::select(., dplyr::contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., dplyr::matches("MA_|SAP_")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., dplyr::matches("MA_1|SAP_1")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., dplyr::matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  dplyr::mutate(., Density = PopSize/area)  %>% 
  dplyr::mutate(., Density1 = PopSize1/(area/2))  %>% 
  dplyr::mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MA_Density = MaturePopSize/area) %>% 
  dplyr::mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_Density = SaplingPopSize/area) %>% 
  dplyr::mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  dplyr::mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  dplyr::mutate(., Fire_scenario = "Decreasing fire return interval") %>% 
  dplyr::mutate(., Dispersal_probability = "Low dispersal") %>% 
  dplyr::mutate(., Period = "Current") %>%
  dplyr::mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(decrease_high_distance_revision$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  dplyr::mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, - t)

decrease_high_distance_pop_size_total <- dplyr::full_join(decrease_high_distance_pop_size_total, tSinceFire, by = "IterationTime")


rm(decrease_high_distance_revision)

density_decrease_high_distance_revision <- decrease_high_distance_pop_size_total %>%
  dplyr::select(., Iteration, t, Density, Density1, Density2,
                MA_Density, MA_Density1, MA_Density2,
                SAP_Density, SAP_Density1, SAP_Density2,
                SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2) %>%
  tidyr::gather(., LifeStage_pop, Density, -Iteration, -t)
# 
# 
# density_decrease_high_distance_revision %>%
#   # filter(., t < 20) %>%
#   filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
#   ggplot2::ggplot(., ggplot2::aes(x = t, y = Density, col = Iteration))+
#   ggplot2::geom_line()+
#   ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)+
#   ggplot2::theme(legend.position = "none")
# 
# 
# summary_decrease_high_distance_density_by_t <- density_decrease_high_distance %>%
#   dplyr::group_by(., t, LifeStage_pop) %>%
#   dplyr::summarise_at(., dplyr::vars(Density), dplyr::vars(mean, median, min, max)) %>%
#   dplyr::ungroup()
# 
# summary_decrease_high_distance_density_by_t %>%
#   ggplot2::ggplot(., ggplot2::aes(x = t, y = median))+
#   ggplot2::geom_line()+
#   ggplot2::geom_ribbon(ggplot2::aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# 
# (summary_decrease_high_distance_density <- summary_decrease_high_distance_density_by_t %>%
#     dplyr::group_by(., LifeStage_pop) %>%
#     dplyr::summarise_at(., dplyr::vars(mean, median), dplyr::funs(mean)) %>%
#     dplyr::ungroup())
# 
# write.csv(summary_decrease_high_distance_density, 
#           file = paste0(proj_dir, export_table_dir, "decrease high no restoration density.csv"))
# 
# rm(summary_decrease_high_distance_density_by_t, summary_decrease_high_distance_density, density_decrease_high_distance)

# decrease_high_distance_pop_size_total <- decrease_high_distance_pop_size_total %>% 
#   dplyr::select(., Iteration, t, Density) #, Density1, Density2, SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2)


# 6) Decreasing Low Distance
#---------------------------------------|---------------------------------------
#                          decrease FRI, low distance                   
#---------------------------------------|---------------------------------------

decrease_low_distance_pop_size_total <- decrease_low_distance_revision$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  dplyr::mutate(., PopSize               = rowSums(dplyr::select(., dplyr::contains("_")))) %>% 
  dplyr::mutate(., PopSize1              = rowSums(dplyr::select(., dplyr::contains("_1")))) %>% 
  dplyr::mutate(., PopSize2              = rowSums(dplyr::select(., dplyr::contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MaturePopSize         = rowSums(dplyr::select(., dplyr::contains("MA_")))) %>% 
  dplyr::mutate(., MaturePopSize1        = rowSums(dplyr::select(., dplyr::contains("MA_1")))) %>% 
  dplyr::mutate(., MaturePopSize2        = rowSums(dplyr::select(., dplyr::contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingPopSize        = rowSums(dplyr::select(., dplyr::contains("SAP_")))) %>% 
  dplyr::mutate(., SaplingPopSize1       = rowSums(dplyr::select(., dplyr::contains("SAP_1")))) %>% 
  dplyr::mutate(., SaplingPopSize2       = rowSums(dplyr::select(., dplyr::contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., dplyr::matches("MA_|SAP_")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., dplyr::matches("MA_1|SAP_1")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., dplyr::matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  dplyr::mutate(., Density = PopSize/area)  %>% 
  dplyr::mutate(., Density1 = PopSize1/(area/2))  %>% 
  dplyr::mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MA_Density = MaturePopSize/area) %>% 
  dplyr::mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_Density = SaplingPopSize/area) %>% 
  dplyr::mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  dplyr::mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  dplyr::mutate(., Fire_scenario = "Decreasing fire return interval") %>% 
  dplyr::mutate(., Dispersal_probability = "High dispersal") %>% 
  dplyr::mutate(., Period = "Current") %>%
  dplyr::mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(decrease_low_distance_revision$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  dplyr::mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, - t)

decrease_low_distance_pop_size_total <- dplyr::full_join(decrease_low_distance_pop_size_total, tSinceFire, by = "IterationTime")


rm(decrease_low_distance_revision)


pop1 <- decrease_low_distance_pop_size_total %>% 
  dplyr::select(., Iteration,t, contains("1")) %>% 
  dplyr::mutate(., Pop = "pop1") %>% 
  dplyr::mutate(., Dispersal_probability = "Low")

colnames(pop1) <- c("Iteration","t", "CS", "SD", "SAP", "MA", "PopSize", "MaturePopSize", "SaplingPopSize", "SaplingMaturePopSize",
                    "Density", "MA_Density", "SAP_Density", "SAP_MA_Density", "tSinceFire", "Pop", "Dispersal_probability")

pop2 <- decrease_low_distance_pop_size_total %>% 
  dplyr::select(., Iteration,t, dplyr::contains("2")) %>% 
  dplyr::mutate(., Pop = "pop2") %>% 
  dplyr::mutate(., Dispersal_probability = "Low")

colnames(pop2) <- c("Iteration","t", "CS", "SD", "SAP", "MA", "PopSize", "MaturePopSize", "SaplingPopSize", "SaplingMaturePopSize",
                    "Density", "MA_Density", "SAP_Density", "SAP_MA_Density", "tSinceFire", "Pop", "Dispersal_probability")

pops <- dplyr::bind_rows(pop1, pop2)

# pops %>%
#   dplyr::group_by(t, Pop) %>%
#   dplyr::summarise_at(., vars(CS, SD), funs( median)) %>%
#   dplyr::ungroup() %>%
#   tidyr::gather(., Stage, Count, -Pop, -t) %>%
#   ggplot2::ggplot(.,ggplot2::aes(., x = t, y = Count, col = Stage))+
#   ggplot2::geom_line()+
#   ggplot2::facet_grid(~Pop)

# density_decrease_low_ditance_revision <- decrease_low_distance_pop_size_total %>%
#   dplyr::select(., Iteration, t, Density, Density1, Density2,
#                 MA_Density, MA_Density1, MA_Density2,
#                 SAP_Density, SAP_Density1, SAP_Density2,
#                 SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2) %>%
#   tidyr::gather(., LifeStage_pop, Density, -Iteration, -t)
# 
# 
# density_decrease_low_distance %>%
#   # dplyr::filter(., t < 20) %>%
#   dplyr::filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
#   ggplot2::ggplot(., ggplot2::aes(x = t, y = Density, col = Iteration))+
#   ggplot2::geom_line()+
#   ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)+
#   ggplot2::theme(legend.position = "none")
# 
# 
# summary_decrease_low_distance_density_by_t <- density_decrease_low_distance %>%
#   dplyr::group_by(., t, LifeStage_pop) %>%
#   dplyr::summarise_at(., vars(Density), vars(mean, median, min, max)) %>%
#   dplyr::ungroup()
# 
# summary_decrease_low_distance_density_by_t %>%
#   ggplot2::ggplot(., ggplot2::aes(x = t, y = median))+
#   ggplot2::geom_line()+
#   ggplot2::geom_ribbon(ggplot2::aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# 
# (summary_decrease_low_distance_density <- summary_decrease_low_distance_density_by_t %>%
#     dplyr::group_by(., LifeStage_pop) %>%
#     dplyr::summarise_at(., vars(mean, median), funs(mean)) %>%
#     dplyr::ungroup())
# 
# write.csv(summary_decrease_low_distance_density, 
#           file = paste0(proj_dir, export_table_dir, "decrease low no restoration density.csv"))
# 
# rm(summary_decrease_low_distance_density_by_t, summary_decrease_low_distance_density, density_decrease_low_distance)

# decrease_low_distance_pop_size_total <- decrease_low_distance_pop_size_total %>% 
#   dplyr::select(., Iteration, t, Density) #, Density1, Density2, SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2)

################################################################################

# 7) Constant High Distance Historical
#---------------------------------------|---------------------------------------
#                           Constant FRI, high distance_historical 
#---------------------------------------|---------------------------------------
area <- 2000
constant_high_distance_historic_revision_pop_size_total <- constant_high_distance_historic_revision$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  dplyr::mutate(., PopSize               = rowSums(dplyr::select(., dplyr::contains("_")))) %>% 
  dplyr::mutate(., PopSize1              = rowSums(dplyr::select(., dplyr::contains("_1")))) %>% 
  dplyr::mutate(., PopSize2              = rowSums(dplyr::select(., dplyr::contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MaturePopSize         = rowSums(dplyr::select(., dplyr::contains("MA_")))) %>% 
  dplyr::mutate(., MaturePopSize1        = rowSums(dplyr::select(., dplyr::contains("MA_1")))) %>% 
  dplyr::mutate(., MaturePopSize2        = rowSums(dplyr::select(., dplyr::contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingPopSize        = rowSums(dplyr::select(., dplyr::contains("SAP_")))) %>% 
  dplyr::mutate(., SaplingPopSize1       = rowSums(dplyr::select(., dplyr::contains("SAP_1")))) %>% 
  dplyr::mutate(., SaplingPopSize2       = rowSums(dplyr::select(., dplyr::contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., dplyr::matches("MA_|SAP_")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., dplyr::matches("MA_1|SAP_1")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., dplyr::matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  dplyr::mutate(., Density = PopSize/area)  %>% 
  dplyr::mutate(., Density1 = PopSize1/(area/2))  %>% 
  dplyr::mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MA_Density = MaturePopSize/area) %>% 
  dplyr::mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_Density = SaplingPopSize/area) %>% 
  dplyr::mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  dplyr::mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  dplyr::mutate(., Fire_scenario = "Historical fire return interval") %>% 
  dplyr::mutate(., Dispersal_probability = "Low dispersal") %>% 
  dplyr::mutate(., Period = "Historical") %>%
  dplyr::mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(constant_high_distance_historic_revision$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  dplyr::mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, - t)

constant_high_distance_historic_revision_pop_size_total <- dplyr::full_join(constant_high_distance_historic_revision_pop_size_total, tSinceFire, by = "IterationTime") %>% 
  dplyr::mutate(., t_burnin = t) %>% 
  dplyr::mutate(., t = t - 10) %>% 
  dplyr::filter(., t > 0)




density_constant_high_distance_historic_revision <- constant_high_distance_historic_revision_pop_size_total %>%
  # filter(., t < 250) %>%
  dplyr::select(., Iteration, t, Density, Density1, Density2,
                MA_Density, MA_Density1, MA_Density2,
                SAP_Density, SAP_Density1, SAP_Density2,
                SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2, tSinceFire1, tSinceFire2) %>%
  tidyr::gather(., LifeStage_pop, Density, -Iteration, -t,  -tSinceFire1, -tSinceFire2)
# 
# 
density_constant_high_distance_historic_revision %>%
  # dplyr::filter(., t < 5) %>%
  dplyr::filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
  ggplot2::ggplot(., ggplot2::aes(x = t, y = Density, col = Iteration))+
  ggplot2::geom_line()+
  ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)+
  ggplot2::theme(legend.position = "none")

# 
# summary_constant_high_distance_historic_revision_pop_size_totaldensity_by_t <- density_constant_high_distance_historic_revision %>%
#   # dplyr::filter(., t <250) %>%
#   dplyr::group_by(., t, LifeStage_pop) %>%
#   dplyr::summarise_at(., dplyr::vars(Density), list(mean = mean, median = median, min = min, max = max)) %>%
#   dplyr::ungroup()
# 
# summary_constant_high_distance_historic_revision_pop_size_totaldensity_by_t %>%
#   ggplot2::ggplot(., ggplot2::aes(x = t, y = median))+
#   ggplot2::geom_line()+
#   ggplot2::geom_ribbon(ggplot2::aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# 
# (summary_constant_high_distance_historic_revision_pop_size_totaldensity <- summary_constant_high_distance_historic_revision_pop_size_totaldensity_by_t %>%
#     dplyr::group_by(., LifeStage_pop) %>%
#     dplyr::summarise_at(., dplyr::vars(mean, median), funs(mean)) %>%
#     dplyr::ungroup())
# 
# write.csv(summary_constant_high_distance_historic_revision_pop_size_totaldensity, 
#           file = paste0(proj_dir, export_table_dir, "constant high restoration density.csv"))
# 
# rm(summary_constant_high_distance_historic_revision_pop_size_totaldensity_by_t, summary_constant_high_distance_historic_revision_pop_size_totaldensity, density_constant_high_distance_historic)

# constant_high_distance_historic_revision_pop_size_totalpop_size_total <- constant_high_distance_historic_revision_pop_size_totalpop_size_total %>% 
# dplyr::select(., Iteration, t, Density, Density1, Density2, SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2)

# 8) Constant Low Distance Restoration
#---------------------------------------|---------------------------------------
#                 Historical FRI, low distance, historical mortality                   
#---------------------------------------|---------------------------------------

constant_low_distance_historic_pop_size_total <- constant_low_distance_historic_revision$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  dplyr::mutate(., PopSize               = rowSums(dplyr::select(., dplyr::contains("_")))) %>% 
  dplyr::mutate(., PopSize1              = rowSums(dplyr::select(., dplyr::contains("_1")))) %>% 
  dplyr::mutate(., PopSize2              = rowSums(dplyr::select(., dplyr::contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MaturePopSize         = rowSums(dplyr::select(., dplyr::contains("MA_")))) %>% 
  dplyr::mutate(., MaturePopSize1        = rowSums(dplyr::select(., dplyr::contains("MA_1")))) %>% 
  dplyr::mutate(., MaturePopSize2        = rowSums(dplyr::select(., dplyr::contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingPopSize        = rowSums(dplyr::select(., dplyr::contains("SAP_")))) %>% 
  dplyr::mutate(., SaplingPopSize1       = rowSums(dplyr::select(., dplyr::contains("SAP_1")))) %>% 
  dplyr::mutate(., SaplingPopSize2       = rowSums(dplyr::select(., dplyr::contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  dplyr::mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., dplyr::matches("MA_|SAP_")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., dplyr::matches("MA_1|SAP_1")))) %>% 
  dplyr::mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., dplyr::matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  dplyr::mutate(., Density = PopSize/area)  %>% 
  dplyr::mutate(., Density1 = PopSize1/(area/2))  %>% 
  dplyr::mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  dplyr::mutate(., MA_Density = MaturePopSize/area) %>% 
  dplyr::mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_Density = SaplingPopSize/area) %>% 
  dplyr::mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  dplyr::mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  dplyr::mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  dplyr::mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  dplyr::mutate(., Fire_scenario = "Historical fire return interval") %>% 
  dplyr::mutate(., Dispersal_probability = "High dispersal") %>% 
  dplyr::mutate(., Period = "Historical") %>%
  dplyr::mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(constant_low_distance_historic_revision$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  dplyr::mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, - t)

constant_low_distance_historic_pop_size_total <- dplyr::full_join(constant_low_distance_historic_pop_size_total, tSinceFire, by = "IterationTime")%>% 
  dplyr::mutate(., t_burnin = t) %>% 
  dplyr::mutate(., t = t - 10) %>% 
  dplyr::filter(., t > 0)

historical <- dplyr::bind_rows(constant_high_distance_historic_revision_pop_size_total, constant_low_distance_historic_pop_size_total)

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
  dplyr::summarise_at(., dplyr::vars(SAP_MA_Density, Density), dplyr::funs(median,  q1, q3)))


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
  ggplot2::scale_fill_manual(values = c("#ef8a62", "#67a9cf")) +
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
  # ggplot2::geom_vline(xintercept = 268, colour = "darkgrey", lty = 2)
  
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
  

dev.off()

rm(constant_low_distance_historic_revision, pops, tSinceFire, legd, density_tSinceFire, constant_high_distance_historic_revision)

density_constant_low_distance_historic <- constant_low_distance_historic_pop_size_total %>%
  dplyr::select(., Iteration, t, Density, Density1, Density2,
                MA_Density, MA_Density1, MA_Density2,
                SAP_Density, SAP_Density1, SAP_Density2,
                SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2) %>%
  tidyr::gather(., LifeStage_pop, Density, -Iteration, -t)


density_constant_low_distance_historic %>%
  # dplyr::filter(., t < 20) %>%
  dplyr::filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
  ggplot2::ggplot(., ggplot2::aes(x = t, y = Density, col = Iteration))+
  ggplot2::geom_line()+
  ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)+
  ggplot2::theme(legend.position = "none")
# 
# 
# summary_constant_low_distance_historic_density_by_t <- density_constant_low_distance_historic %>%
#   dplyr::group_by(., t, LifeStage_pop) %>%
#   dplyr::summarise_at(., dplyr::vars(Density), dplyr::funs(mean, median, min, max)) %>%
#   dplyr::ungroup()
# 
# summary_constant_low_distance_historic_density_by_t %>%
#   ggplot2::ggplot(., ggplot2::aes(x = t, y = median))+
#   ggplot2::geom_line()+
#   ggplot2::geom_ribbon(ggplot2::aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   ggplot2::facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# (summary_constant_low_distance_historic_density <- summary_constant_low_distance_historic_density_by_t %>%
#     dplyr::group_by(., LifeStage_pop) %>%
#     dplyr::summarise_at(., dplyr::vars(mean, median), dplyr::funs(mean)) %>%
#     dplyr::ungroup())
# 
# write.csv(summary_constant_low_distance_historic_density, 
#           file = paste0(proj_dir, export_table_dir, "constant low restoration density.csv"))
# 
# rm(summary_constant_low_distance_historic_density_by_t, summary_constant_low_distance_historic_density, density_constant_low_distance_historic)

# constant_low_distance_historic_pop_size_total <- constant_low_distance_historic_pop_size_total %>% 
#   dplyr::select(., Iteration, t, Density, Density1, Density2, SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2)

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


current_models <- dplyr::bind_rows(constant_low_distance_pop_size_total, 
                            constant_high_distance_pop_size_total,
                            suppression_low_distance_pop_size_total,
                            suppression_high_distance_pop_size_total,
                            decrease_low_distance_pop_size_total,
                            decrease_high_distance_pop_size_total) %>% 
  dplyr::mutate(., Treatment = paste(Fire_scenario, Dispersal_probability, Period))

unique(current_models$Treatment)

rm(constant_high_distance_pop_size_total, constant_low_distance_pop_size_total,
   suppression_high_distance_pop_size_total, suppression_low_distance_pop_size_total,
   decrease_high_distance_pop_size_total, decrease_low_distance_pop_size_total)


historic_models <- dplyr::bind_rows(constant_low_distance_historic_pop_size_total,
                                constant_high_distance_historic_revision_pop_size_total) %>% 
  dplyr::mutate(., Treatment = paste(Fire_scenario, Dispersal_probability, Period))

unique(historic_models$Treatment)

rm(constant_low_distance_historic_pop_size_total,
   constant_high_distance_historic_revision_pop_size_total)
   

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

extinct_current <- current_models %>%
  # dplyr::group_by(., Iteration, Fire_scenario, Dispersal_probability) %>%
  dplyr::filter(., Density < 10) %>%  # Low threshold that is reported in results
  # dplyr::filter(., Density < 2e-04) %>%  # High threshold to show information about suppression
  # dplyr::ungroup() %>%
  dplyr::arrange(., Iteration, t) %>%
  dplyr::group_by(., Iteration, Fire_scenario, Dispersal_probability) %>%
  dplyr::filter(., t == min(t)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(., Period = "Current")

test <- extinct_current %>%
  dplyr::group_by(., Treatment, Iteration) %>%
  dplyr::tally() %>%
  dplyr::ungroup()

sum(test$n >1)

rm(test)

extinct_historic <- historic_models %>%
  dplyr::group_by(., Iteration, Fire_scenario, Dispersal_probability) %>%
  dplyr::filter(., Density < 20) %>%  # Low threshold that is reported in results
  # dplyr::filter(., Density < 2e-04) %>%  # High threshold to show information about suppression
  # dplyr::ungroup() %>%
  dplyr::arrange(., Iteration, t) %>%
  dplyr::group_by(., Iteration, Fire_scenario, Dispersal_probability) %>%
  dplyr::filter(., t == min(t)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(., Period = "Historical")

test <- extinct_historic %>%
  dplyr::group_by(., Treatment, Iteration) %>%
  dplyr::tally() %>%
  dplyr::ungroup()

sum(test$n >1)

rm(test)

extinct <- dplyr::bind_rows(extinct_current, extinct_historic)


# ggplot2::ggplot(data = extinct, ggplot2::aes(x = t, fill = Dispersal_probability))+
#   ggplot2::geom_density(alpha = 0.5)+
#   ggplot2::facet_grid( Period ~ Fire_scenario + Dispersal_probability )

(median_extinction_time <- extinct %>%
    dplyr::group_by(Period, Fire_scenario, Dispersal_probability) %>%
    dplyr::summarise_at(., vars(t), funs(min, lower, q1, median,mean, q3, upper, max)) %>%
    dplyr::ungroup())

median_extinction_time_export <- median_extinction_time %>% 
  dplyr::select(., Period, Fire_scenario, Dispersal_probability, q1, median, q3) %>% 
  dplyr::mutate(., `Time to extirpation` = paste0(round(median, 3), "(", round(q1,3), ", ",round(q3,3),")" )) %>% 
  dplyr::select(., Period, Fire_scenario, Dispersal_probability, `Time to extirpation`)

# write.csv(median_extinction_time_export, file = paste0(proj_dir, export_table_dir, "Final? extirpation times.csv"))

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


Cum_extirpation <- extinct %>% 
  dplyr::mutate(., Iteration = as.numeric(Iteration)) %>% 
  dplyr::filter(., Period == "Current") %>% 
  dplyr::mutate(., Extirpation = 1) %>% 
  dplyr::select(., Iteration, t, Fire_scenario, Dispersal_probability, Extirpation) %>% 
  tidyr::complete(.,Iteration           = 1:5000, 
                  t                     = 1:500,
                  Fire_scenario         = unique(extinct$Fire_scenario), 
                  Dispersal_probability = unique(extinct$Dispersal_probability),
                  fill                  = list(Extirpation = 0)) %>% 
  dplyr::group_by(., Fire_scenario, Dispersal_probability, t) %>% 
  dplyr::summarise_at(., dplyr::vars(Extirpation), dplyr::funs(sum)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(., Fire_scenario, Dispersal_probability) %>% 
  dplyr::mutate(., CumSum = cumsum(Extirpation)) %>% 
  dplyr::mutate(., PExt = CumSum/5000) %>% 
  dplyr::ungroup()


Cum_extirpation %>% dplyr::filter(., t == 500)

# tiff(filename = paste0(proj_dir, figures_dir, "Final cumulative extirpation.tiff"),
#      width = 12,
#      height = 8.5,
#      units = "in",
#      res = 300,
#      compression = "lzw")

Cum_extirpation %>% 
  ggplot2::ggplot(data = ., 
                  ggplot2::aes(x = t, y = PExt, col = Fire_scenario))+
  ggplot2::geom_line(data = Cum_extirpation, ggplot2::
                       aes(x = t, y = PExt, lty = Dispersal_probability), lwd = 1.5)+
  # ggplot2::scale_colour_manual(
  #   "CI horizontal line", values = c("#d73027", "#1a9850"),
  #   guide=ggplot2::guide_legend(override.aes = list(colour=c("#d73027", "#1a9850"))),
  #   labels=c("Decreasing fire return interval", "Historical fire return interval")
  # ) +
  ggplot2::scale_colour_manual(values = c("#d73027", "#1a9850")) +
  # ggplot2::facet_grid(~ Dispersal_probability) +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "\nTime (years)",
                y = "Cumulative extirpation probability \n") +
  ggplot2::theme(strip.background = ggplot2::element_rect(colour = "black", fill = "white"),
                 strip.text       = ggplot2::element_text(colour = "black",  size = strip_text_size)) +
  # ggplot2::theme(legend.position = "none") +
  ggplot2::theme(legend.position  = "top", legend.box = "vertical")+
  ggplot2::guides(linetype    = ggplot2::guide_legend(keywidth = 3, keyheight = 1)) +
  ggplot2::theme(legend.text  = ggplot2::element_text(size = axis_text_size))+
  ggplot2::theme(legend.title = ggplot2::element_blank())+
  ggplot2::theme(axis.title   = ggplot2::element_text(size = axis_label_text_size - 1)) +
  ggplot2::theme(axis.text    = ggplot2::element_text(size = axis_text_size)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.1))+
  ggplot2::theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines"))# top, right, bottom, left


# legd <- grid::legendGrob(c("Historical fire return interval", "Suppression","Decreasing fire return interval"),
#                          nrow=1,
#                          ncol=3,
#                          lines.first = TRUE,
#                          hgap = grid::unit(2, "lines"),
#                          vgap = grid::unit(1, "lines"),
#                          default.units = "lines",
#                          pch=22,
#                          gp=grid::gpar(col      = c("black","black"),
#                                        fill     = c("#1a9850", "#fee08b", "#d73027"),
#                                        fontsize = axis_text_size,
#                                        fontface = "bold"),
#                          vp = grid::viewport(x = 0, y = 0, w = 1.02, h = 1.92, just = c("left", "bottom")))
# 
# grid::grid.draw(legd)
# 
# 
# legd1 <- grid::legendGrob(c("High dispersal probability", "Low dispersal probability"),
#                          nrow=1,
#                          ncol=2,
#                          lines.first = TRUE,
#                          hgap = grid::unit(2, "lines"),
#                          vgap = grid::unit(1, "lines"),
#                          default.units = "lines",
#                          lty = c(1,2),
#                          gp=grid::gpar(col      = c("black","black"),
#                                        fontsize = axis_text_size,
#                                        fontface = "bold"),
#                          vp = grid::viewport(x = 0, y = 0, w = 1.02, h = 1.92, just = c("left", "bottom")))
# 
# grid::grid.draw(legd1)

# dev.off()




Cum_extirpation_historical <- extinct %>% 
  dplyr::mutate(., Iteration = as.numeric(Iteration)) %>% 
  dplyr::filter(., Period == "Historical") %>% 
  dplyr::mutate(., Extirpation = 1) %>% 
  dplyr::select(., Iteration, t, Fire_scenario, Dispersal_probability, Extirpation) %>% 
  tidyr::complete(.,Iteration             = 1:5000, 
           t                     = 1:500,
           Fire_scenario         = unique(extinct$Fire_scenario), 
           Dispersal_probability = unique(extinct$Dispersal_probability),
           fill                  = list(Extirpation = 0)) %>% 
  dplyr::group_by(., Fire_scenario, Dispersal_probability, t) %>% 
  dplyr::summarise_at(., dplyr::vars(Extirpation), dplyr::funs(sum)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(., Fire_scenario, Dispersal_probability) %>% 
  dplyr::mutate(., CumSum = cumsum(Extirpation)) %>% 
  dplyr::mutate(., PExt = CumSum/5000) %>% 
  dplyr::ungroup()


Cum_extirpation_historical %>% dplyr::filter(., t == 500)



################################################################################
#---------------------------------------|---------------------------------------
#                             Extirpations 200 years 
#---------------------------------------|---------------------------------------
################################################################################

#---------------------------------------|---------------------------------------
#                                  Time Period 
# Do demographics alone in absence of changes to fire return interval suggest
# an increase in odds of extirpation?
#---------------------------------------|---------------------------------------
extirpation_150_historic <- extinct %>% 
  # filter(.,  t <= 150) %>%
  dplyr::filter(Fire_scenario == "Historic fire return interval") %>%
  dplyr::group_by(.,Period) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup()

# extirpation_150_historic[2,] <- c("Historic", 0) # only if time horizon is 100

historic_table <- extirpation_150_historic %>%
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
  dplyr::mutate(., FireReturnInterval = "Historic") %>% 
  dplyr::mutate(., DispersalProbability = "All")

#---------------------------------------|--------------------------------------------------
#                                  Fire 
# Do changes to fire frequency, across all dispersal probabilities (fire sizes), increase
# odds of extirpation
#---------------------------------------|---------------------------------------------------

extirpation_150_fire <- extinct %>% 
  dplyr::filter(., Period == "Current") %>% 
  # dplyr::filter(., t <= 150) %>%
  dplyr::group_by(.,Fire_scenario) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup()

fire_table <- extirpation_150_fire %>% 
  dplyr::rename(., Extirpations = n) %>% 
  dplyr::mutate(., Surviving = reps * nrow(extirpation_150_fire) - Extirpations)# %>% 
# dplyr::mutate(., Odds = Extirpations/Surviving) %>% 
# dplyr::select(., -n, -Extirpations, - Surviving)  %>% 
# dplyr::mutate(., DecreasingHighNo  = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal No Restoration"], nrow(test))) %>% 
# dplyr::mutate(., DecreasingHighYes = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal Restoration"], nrow(test))) %>% 
# dplyr::mutate(., )

fire <- as.character(fire_table$Fire_scenario)
outcomes <- c("Etirpation", "Surviving")

(dat <- matrix(c(as.numeric(fire_table$Extirpations), as.numeric(fire_table$Surviving)), byrow = F, nrow = nrow(fire_table)))
dimnames(dat) <- list("Fire" = fire, "Outcome" = outcomes)

(fire_ORs <- fisher.test(dat))

fire_ORs <- data.frame(OR = fire_ORs$estimate, LL = fire_ORs$conf.int[1], UL = fire_ORs$conf.int[2]) %>% 
  dplyr::mutate(., Comparison = "Decreasing/Historic")  %>% 
  dplyr::mutate(., FireReturnInterval = "All") %>% 
  dplyr::mutate(., DispersalProbability = "All")
  
#---------------------------------------|---------------------------------------
#                                  Dispersal Decreasing
#  Does dispersal probability, across all fire frequencies (fire sizes) change
# odds of extirpation
#---------------------------------------|---------------------------------------

extirpation_150_dispersal_decreasing <- extinct %>% 
  dplyr::filter(., Period == "Current" & Fire_scenario == "Decreasing fire return interval") %>% 
  # dplyr::filter(., t <= 150) %>%
  dplyr::group_by(.,Dispersal_probability) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup()


dispersal_table_decreasing <- extirpation_150_dispersal_decreasing %>% 
  dplyr::select(., Dispersal_probability, n) %>% 
  dplyr::rename(., Extirpations = n) %>% 
  dplyr::mutate(., Surviving = reps - Extirpations)# %>% 
# dplyr::mutate(., Odds = Extirpations/Surviving) %>% 
# dplyr::select(., -n, -Extirpations, - Surviving)  %>% 
# dplyr::mutate(., DecreasingHighNo  = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal No Restoration"], nrow(test))) %>% 
# dplyr::mutate(., DecreasingHighYes = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal Restoration"], nrow(test))) %>% 
# dplyr::mutate(., )

dispersal <- as.character(dispersal_table_decreasing$Dispersal_probability)
outcomes <- c("Etirpation", "Surviving")

(dat <- matrix(c(as.numeric(dispersal_table_decreasing$Extirpations), as.numeric(dispersal_table_decreasing$Surviving)), byrow = F, nrow = nrow(dispersal_table_decreasing)))
dimnames(dat) <- list("Dispersal" = dispersal, "Outcome" = outcomes)

(dispersal_Current_ORs_decreasing <- fisher.test(dat))

dispersal_Current_ORs_decreasing <- data.frame(OR = dispersal_Current_ORs_decreasing$estimate, 
                                               LL = dispersal_Current_ORs_decreasing$conf.int[1], 
                                               UL = dispersal_Current_ORs_decreasing$conf.int[2]) %>% 
  dplyr::mutate(., Comparison = "High/Low") %>% 
  dplyr::mutate(., FireReturnInterval = "Decreasing") %>% 
  dplyr::mutate(., DispersalProbability = "All")

#---------------------------------------|---------------------------------------
#                                  Dispersal Decreasing
#  Does dispersal probability, across all fire frequencies (fire sizes) change
# odds of extirpation
#---------------------------------------|---------------------------------------

extirpation_150_dispersal_historic <- extinct %>% 
  dplyr::filter(., Period == "Current" & Fire_scenario == "Historic fire return interval") %>% 
  # filter(., t <= 150) %>%
  dplyr::group_by(.,Dispersal_probability) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup()


dispersal_table_historic <- extirpation_150_dispersal_historic %>% 
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

(dispersal_Current_ORs_historic <- fisher.test(dat))



dispersal_Current_ORs_historic <- data.frame(OR = dispersal_Current_ORs_historic$estimate, 
                                             LL = dispersal_Current_ORs_historic$conf.int[1], 
                                             UL = dispersal_Current_ORs_historic$conf.int[2]) %>% 
  dplyr::mutate(., Comparison = "High/Low") %>% 
  dplyr::mutate(., FireReturnInterval = "Historic") %>% 
  dplyr::mutate(., DispersalProbability = "All")


#---------------------------------------|---------------------------------------
#                                  High dispersal
#---------------------------------------|---------------------------------------

extirpation_150_highDispersal_fire <- extinct %>% 
  dplyr::filter(., Period == "Current" & Dispersal_probability == "High dispersal") %>% 
  # dplyr::filter(., t <= 150) %>%
  dplyr::group_by(.,Fire_scenario) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup()


fire_high_dispersal_table <- extirpation_150_highDispersal_fire %>% 
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

(fire_high_dispersal_ORs <- fisher.test(dat))


fire_high_dispersal_ORs <- data.frame(OR = fire_high_dispersal_ORs$estimate, 
                                      LL = fire_high_dispersal_ORs$conf.int[1], 
                                      UL = fire_high_dispersal_ORs$conf.int[2]) %>% 
  dplyr::mutate(., Comparison = "Decreasing/Historic") %>% 
  dplyr::mutate(., FireReturnInterval = "All") %>% 
  dplyr::mutate(., DispersalProbability = "High")


#---------------------------------------|---------------------------------------
#                                  Low dispersal
#---------------------------------------|---------------------------------------

extirpation_150_lowDispersal_fire <- extinct %>% 
  dplyr::filter(., Period == "Current" & Dispersal_probability == "Low dispersal") %>% 
  # filter(., t <= 150) %>%
  dplyr::group_by(.,Fire_scenario) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup()


fire_low_dispersal_table <- extirpation_150_lowDispersal_fire %>% 
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

(fire_low_dispersal_ORs <- fisher.test(dat))



fire_low_dispersal_ORs <- data.frame(OR = fire_low_dispersal_ORs$estimate, 
                                     LL = fire_low_dispersal_ORs$conf.int[1], 
                                     UL = fire_low_dispersal_ORs$conf.int[2]) %>% 
  dplyr::mutate(., Comparison = "Decreasing/Historic") %>% 
  dplyr::mutate(., FireReturnInterval = "All") %>% 
  dplyr::mutate(., DispersalProbability = "Low")


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
ORs <- bind_rows(do.call("list",mget(ORPattern)))

ORs <- ORs %>% 
  dplyr::mutate(., OddsRatio = ifelse(OR <1, 1/OR, OR)) %>% 
  dplyr::mutate(., LL_ = ifelse(OR <1, 1/UL, LL)) %>% 
  dplyr::mutate(., UL_ = ifelse(OR <1, 1/LL, UL))%>% 
  dplyr::mutate(., Comparison = ifelse(OR < 1, paste0(Comparison, "*"), Comparison)) %>% 
  dplyr::select(., Comparison, FireReturnInterval, DispersalProbability, OddsRatio, LL_, UL_) %>% 
  dplyr::rename(., UL = UL_, LL = LL_)



OR_table <- ORs %>%
  dplyr::mutate(., OR = paste0(round(OddsRatio, 3), "(", round(LL,3), ", ",round(UL,3),")" )) %>% 
  dplyr::select(., -LL, -UL, -OddsRatio) %>% 
  dplyr::arrange(., FireReturnInterval, DispersalProbability, Comparison)




write.csv(OR_table, file = paste0(proj_dir, export_table_dir, "OR table 150.csv"))

# tiff(filename = paste0(proj_dir, figures_dir, "Final? OR by category fire dispersal restoration.tiff"),
#      width = 12,
#      height = 8.5,
#      units = "in",
#      res = 300,
#      compression = "lzw")

ggplot2::ggplot(data = ORs, ggplot2::aes(x = Comparison, y = OddsRatio))+
  ggplot2::geom_hline(yintercept = 1, col = "black", lty = 2) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = LL, ymax = UL), lwd = 1.5,
                 position = pd) +
  ggplot2::geom_point(aes(x = Comparison,
                      y = OddsRatio),
                      size = 4,
                      stroke = 2,
                  # colour=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'),
                  position = pd ,
                  shape=21, 
                  fill= "white") +
                 # scale_color_manual(values = c( "#e41a1c","#377eb8","#4daf4a" ))+
  ggplot2::theme_bw() +
  ggplot2::facet_grid(~ Comparison)+
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
