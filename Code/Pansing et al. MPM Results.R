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

proj_dir <- '/Users/elizabethpansing/Box Sync/PhD/Code/WBP Demographic Model Master/Dis_WBP_MODEL_ACTIVE/'
rda_dir <- "Rda/"
code_dir <- "Code/"
model_file <- "Pansing et al. MPM Model Final? Gamma fire.R"
figures_dir <- "Figures/Final?/"
export_table_dir <-  "Exported Data Tables/"

area <- 2e3 * 10000 # Area of 2000 ha total, 1000ha/population. Multiply by 10e3 to get m2

################################################################################
#---------------------------------------|---------------------------------------
#                                   Load model 
#---------------------------------------|---------------------------------------
################################################################################
# The sourced code is only required for the first model run. Otherwise skip to line 130

# source(paste0(proj_dir, code_dir, model_file))

################################################################################
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

# n <- c(300, 90, 100, 300, 99951, 
#             500, 50, 500, 120, 99951)

n <- c(300, 90, 100, 300, 700, 
       500, 50, 500, 120, 600)

n <- c(62, 580, 38, 79, 65+ 953, 62, 580, 38, 79, 65+ 953)

reps <- 5000

axis_label_text_size <- 23
strip_text_size <- 18
axis_text_size <- 18

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
# # 1)
# constant_high_distance <- project(projection_time = 500, n0 = n, reps = reps, FRI_decrease = FALSE,
#                                   fire = TRUE, dispersal_distance = "High", period = "Current")
# save(constant_high_distance, file = paste0(proj_dir, rda_dir, "Final constant high distance fire gamma FRI.Rds"))
# 
# #2)
# constant_low_distance  <- project(projection_time = 500, n0 = n, reps = reps, FRI_decrease = FALSE,
#                                   fire = TRUE, dispersal_distance = "Low", period = "Current")
# save(constant_low_distance, file = paste0(proj_dir, rda_dir, "Final constant low distance fire gamma FRI.Rds"))
# 
# #3)
# suppression_high_distance <- project(projection_time = 500, n0 = n, reps = reps, FRI_decrease = FALSE,
#                                      fire = FALSE, dispersal_distance = "High", period = "Current")
# save(suppression_high_distance, file = paste0(proj_dir, rda_dir,"Final suppression high distance fire gamma FRI.Rds"))
# 
# #4)
# suppression_low_distance  <- project(projection_time = 500, n0 = n, reps = reps, FRI_decrease = FALSE,
#                                      fire = FALSE, dispersal_distance = "Low", period = "Current")
# save(suppression_low_distance, file = paste0(proj_dir, rda_dir, "Final suppression low distance fire gamma FRI.Rds"))
# 
# ##5)
# decrease_high_distance <- project(projection_time = 500, n0 = n, reps = reps, FRI_decrease = TRUE,
#                                   fire = TRUE, dispersal_distance = "High", period = "Current")
# save(decrease_high_distance, file = paste0(proj_dir, rda_dir, "Test decrease high distance fire gamma FRI.Rds"))
# 
# #6)
# decrease_low_distance  <- project(projection_time = 500, n0 = n, reps = reps, FRI_decrease = TRUE,
#                                   fire = TRUE, dispersal_distance = "Low", period = "Current")
# save(decrease_low_distance, file = paste0(proj_dir, rda_dir, "Test decrease low distance fire gamma FRI.Rds"))
# 
# 
# # 7)
# constant_high_distance_historic <- project(projection_time = 500, n0 = n, reps = reps, FRI_decrease = FALSE,
#                                           fire = TRUE, dispersal_distance = "High", period = "Historic")
# save(constant_high_distance_historic, file = paste0(proj_dir, rda_dir, "Final constant high distance historic fire gamma FRI.Rds"))

# # 8)
# constant_low_distance_historic  <- project(projection_time = 500, n0 = n, reps = reps, FRI_decrease = FALSE,
#                                           fire = TRUE, dispersal_distance = "Low", period = "Historic")
# save(constant_low_distance_historic, file = paste0(proj_dir, rda_dir, "Final constant low distance historic fire gamma FRI.Rds"))
# 
# # 9)
# decreasing_high_distance_historic  <- project(projection_time = 500, n0 = n, reps = reps, FRI_decrease = TRUE,
#                                               fire = TRUE, dispersal_distance = "High", period = "Historic")
# save(decreasing_high_distance_historic, file = paste0(proj_dir, rda_dir, "Final decreasing high distance historic fire gamma FRI.Rds"))
# 
# #10)
# decreasing_low_distance_historic  <- project(projection_time = 500, n0 = n, reps = reps, FRI_decrease = TRUE,
#                                           fire = TRUE, dispersal_distance = "Low", period = "Historic")
# save(decreasing_low_distance_historic, file = paste0(proj_dir, rda_dir, "Final decreasing low distance historic fire gamma FRI.Rds"))

################################################################################
#---------------------------------------|---------------------------------------
#                            Load Model Output                        
#---------------------------------------|---------------------------------------
################################################################################


## Scenario #1: Constant fire return interval, low distance

load(paste0(proj_dir, rda_dir, "Final constant high distance fire gamma FRI.Rds"))

## Scenario #2: Constant fire return interval, high distance

load(paste0(proj_dir, rda_dir, "Final constant low distance fire gamma FRI.Rds"))

## Scenario #3: Fire suppression, high distance

load(paste0(proj_dir, rda_dir, "Final suppression high distance fire gamma FRI.Rds"))

## Scenario #4: Fire suppression, low distance

load(paste0(proj_dir, rda_dir, "Final suppression low distance fire gamma FRI.Rds"))

## Scenario #5: Decreasing fire return interval, high distance

load(paste0(proj_dir, rda_dir, "Test decrease high distance fire gamma FRI.Rds"))

## Scenario #6: Decreasing fire return interval, low distance

load(paste0(proj_dir, rda_dir, "Test decrease low distance fire gamma FRI.Rds"))

## Scenario #7: Constant fire return interval, low distance

load(paste0(proj_dir, rda_dir, "Final constant high distance historic fire gamma FRI.Rds"))

## Scenario #8: Constant fire return interval, high distance

load(paste0(proj_dir, rda_dir, "Final constant low distance historic fire gamma FRI.Rds"))

################################################################################
#---------------------------------------|---------------------------------------
#                                Look at trajectories  
#---------------------------------------|---------------------------------------
################################################################################

# 1) Constant High Distance
#---------------------------------------|---------------------------------------
#                           Constant FRI, high distance 
#---------------------------------------|---------------------------------------

constant_high_distance_pop_size_total <- constant_high_distance$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  mutate(., PopSize               = rowSums(dplyr::select(., contains("_")))) %>% 
  mutate(., PopSize1              = rowSums(dplyr::select(., contains("_1")))) %>% 
  mutate(., PopSize2              = rowSums(dplyr::select(., contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  mutate(., MaturePopSize         = rowSums(dplyr::select(., contains("MA_")))) %>% 
  mutate(., MaturePopSize1        = rowSums(dplyr::select(., contains("MA_1")))) %>% 
  mutate(., MaturePopSize2        = rowSums(dplyr::select(., contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  mutate(., SaplingPopSize        = rowSums(dplyr::select(., contains("SAP_")))) %>% 
  mutate(., SaplingPopSize1       = rowSums(dplyr::select(., contains("SAP_1")))) %>% 
  mutate(., SaplingPopSize2       = rowSums(dplyr::select(., contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., matches("MA_|SAP_")))) %>% 
  mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., matches("MA_1|SAP_1")))) %>% 
  mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  mutate(., Density = PopSize/area)  %>% 
  mutate(., Density1 = PopSize1/(area/2))  %>% 
  mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  mutate(., MA_Density = MaturePopSize/area) %>% 
  mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  mutate(., SAP_Density = SaplingPopSize/area) %>% 
  mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  mutate(., Fire_scenario = "Historic fire return interval") %>% 
  mutate(., Dispersal_probability = "Low dispersal") %>% 
  mutate(., Period = "Current") %>%
  mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(constant_high_distance$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, -t)

constant_high_distance_pop_size_total <- full_join(constant_high_distance_pop_size_total, tSinceFire, by = "IterationTime")

pop1 <- constant_high_distance_pop_size_total %>% 
  dplyr::select(., Iteration, contains("1")) %>% 
  mutate(., Pop = "pop1")  %>% 
  mutate(., Dispersal_probability = "High")

colnames(pop1) <- c("Iteration","CS", "SD", "SAP", "MA", "PopSize", "MaturePopSize", "SaplingPopSize", "SaplingMaturePopSize",
                    "Density", "MA_Density", "SAP_Density", "SAP_MA_Density", "tSinceFire", "Pop", "Dispersal_probability")

pop2 <- constant_high_distance_pop_size_total %>% 
  dplyr::select(., Iteration, contains("2")) %>% 
  mutate(., Pop = "pop2") %>% 
  mutate(., Dispersal_probability = "High")

colnames(pop2) <- c("Iteration", "CS", "SD", "SAP", "MA", "PopSize", "MaturePopSize", "SaplingPopSize", "SaplingMaturePopSize",
                    "Density", "MA_Density", "SAP_Density", "SAP_MA_Density", "tSinceFire", "Pop", "Dispersal_probability")

pops <- bind_rows( pop1, pop2)

rm(pop1, pop2)

density_tSinceFire <- pops %>% 
  group_by(., tSinceFire, Dispersal_probability) %>% 
  summarise_at(., vars(MA_Density, SAP_MA_Density, Density), funs(mean, median, min, lower, q1, q3, upper, max)) %>% 
  ungroup() 


# tiff(filename = paste0(proj_dir, figures_dir, "Final SAP MA density by tSinceFire.tiff"),
#      width = 12,
#      height = 8.5,
#      units = "in",
#      res = 300,
#      compression = "lzw")



ggplot(data = density_tSinceFire, aes(x = tSinceFire, y = SAP_MA_Density_median)) +
  geom_ribbon(data = density_tSinceFire, aes(x = tSinceFire, ymin = SAP_MA_Density_q1, ymax = SAP_MA_Density_q3), 
              alpha = 0.2)+
  geom_line(lwd = 1.5) +
  theme_bw() +
  labs(x = "Time since fire", y = expression(paste("Median sapling and mature tree density (no. trees/", m^2,")")))+
  theme(axis.text = element_text(size = axis_text_size)) +
  theme(axis.title = element_text(size = axis_label_text_size))

# dev.off()

rm(constant_high_distance)

# density_constant_high_distance <- constant_high_distance_pop_size_total %>%
#   dplyr::select(., Iteration, t, Density, Density1, Density2,
#                 MA_Density, MA_Density1, MA_Density2,
#                 SAP_Density, SAP_Density1, SAP_Density2,
#                 SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2) %>%
#   gather(., LifeStage_pop, Density, -Iteration, -t)
# 
# 
# density_constant_high_distance %>%
#   # filter(., t < 20) %>%
#   filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
#   ggplot(., aes(x = t, y = Density, col = Iteration))+
#   geom_line()+
#   theme(legend.position = "none") +
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# 
# summary_constant_high_distance_density_by_t <- density_constant_high_distance %>%
#   group_by(., t, LifeStage_pop) %>%
#   summarise_at(., vars(Density), vars(mean, median, min, max)) %>%
#   ungroup()
# 
# summary_constant_high_distance_density_by_t %>%
#   ggplot(., aes(x = t, y = median))+
#   geom_line()+
#   geom_ribbon(aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# 
# (summary_constant_high_distance_density <- summary_constant_high_distance_density_by_t %>%
#     group_by(., LifeStage_pop) %>%
#     summarise_at(., vars(mean, median), funs(mean)) %>%
#     ungroup())
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

constant_low_distance_pop_size_total <- constant_low_distance$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  mutate(., PopSize               = rowSums(dplyr::select(., contains("_")))) %>% 
  mutate(., PopSize1              = rowSums(dplyr::select(., contains("_1")))) %>% 
  mutate(., PopSize2              = rowSums(dplyr::select(., contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  mutate(., MaturePopSize         = rowSums(dplyr::select(., contains("MA_")))) %>% 
  mutate(., MaturePopSize1        = rowSums(dplyr::select(., contains("MA_1")))) %>% 
  mutate(., MaturePopSize2        = rowSums(dplyr::select(., contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  mutate(., SaplingPopSize        = rowSums(dplyr::select(., contains("SAP_")))) %>% 
  mutate(., SaplingPopSize1       = rowSums(dplyr::select(., contains("SAP_1")))) %>% 
  mutate(., SaplingPopSize2       = rowSums(dplyr::select(., contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., matches("MA_|SAP_")))) %>% 
  mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., matches("MA_1|SAP_1")))) %>% 
  mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  mutate(., Density = PopSize/area)  %>% 
  mutate(., Density1 = PopSize1/(area/2))  %>% 
  mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  mutate(., MA_Density = MaturePopSize/area) %>% 
  mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  mutate(., SAP_Density = SaplingPopSize/area) %>% 
  mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  mutate(., Fire_scenario = "Historic fire return interval") %>% 
  mutate(., Dispersal_probability = "High dispersal") %>% 
  mutate(., Period = "Current")%>%
  mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(constant_low_distance$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, - t)

constant_low_distance_pop_size_total <- full_join(constant_low_distance_pop_size_total, tSinceFire, by = "IterationTime")


rm(constant_low_distance)

# density_constant_low_distance <- constant_low_distance_pop_size_total %>%
#   dplyr::select(., Iteration, t, Density, Density1, Density2,
#                 MA_Density, MA_Density1, MA_Density2,
#                 SAP_Density, SAP_Density1, SAP_Density2,
#                 SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2) %>%
#   gather(., LifeStage_pop, Density, -Iteration, -t)
# 
# 
# density_constant_low_distance %>%
#   # filter(., t < 20) %>%
#   filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
#   ggplot(., aes(x = t, y = Density, col = Iteration))+
#   geom_line()+
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)+
#   theme(legend.position = "none")
# 
# 
# summary_constant_low_distance_density_by_t <- density_constant_low_distance %>%
#   group_by(., t, LifeStage_pop) %>%
#   summarise_at(., vars(Density), vars(mean, median, min, max)) %>%
#   ungroup()
# 
# summary_constant_low_distance_density_by_t %>%
#   ggplot(., aes(x = t, y = median))+
#   geom_line()+
#   geom_ribbon(aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# (summary_constant_low_distance_density <- summary_constant_low_distance_density_by_t %>%
#     group_by(., LifeStage_pop) %>%
#     summarise_at(., vars(mean, median), funs(mean)) %>%
#     ungroup())
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

suppression_high_distance_pop_size_total <- suppression_high_distance$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  mutate(., PopSize               = rowSums(dplyr::select(., contains("_")))) %>% 
  mutate(., PopSize1              = rowSums(dplyr::select(., contains("_1")))) %>% 
  mutate(., PopSize2              = rowSums(dplyr::select(., contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  mutate(., MaturePopSize         = rowSums(dplyr::select(., contains("MA_")))) %>% 
  mutate(., MaturePopSize1        = rowSums(dplyr::select(., contains("MA_1")))) %>% 
  mutate(., MaturePopSize2        = rowSums(dplyr::select(., contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  mutate(., SaplingPopSize        = rowSums(dplyr::select(., contains("SAP_")))) %>% 
  mutate(., SaplingPopSize1       = rowSums(dplyr::select(., contains("SAP_1")))) %>% 
  mutate(., SaplingPopSize2       = rowSums(dplyr::select(., contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., matches("MA_|SAP_")))) %>% 
  mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., matches("MA_1|SAP_1")))) %>% 
  mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  mutate(., Density = PopSize/area)  %>% 
  mutate(., Density1 = PopSize1/(area/2))  %>% 
  mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  mutate(., MA_Density = MaturePopSize/area) %>% 
  mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  mutate(., SAP_Density = SaplingPopSize/area) %>% 
  mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  mutate(., Fire_scenario = "Suppression") %>% 
  mutate(., Dispersal_probability = "Low dispersal") %>% 
  mutate(., Period = "Current")%>%
  mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(suppression_high_distance$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, - t)

suppression_high_distance_pop_size_total <- full_join(suppression_high_distance_pop_size_total, tSinceFire, by = "IterationTime")


rm(suppression_high_distance)

# density_suppression_high_distance <- suppression_high_distance_pop_size_total %>%
#   dplyr::select(., Iteration, t, Density, Density1, Density2,
#                 MA_Density, MA_Density1, MA_Density2,
#                 SAP_Density, SAP_Density1, SAP_Density2,
#                 SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2) %>%
#   gather(., LifeStage_pop, Density, -Iteration, -t)
# 
# 
# density_suppression_high_distance %>%
#   # filter(., t < 20) %>%
#   filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
#   ggplot(., aes(x = t, y = Density, col = Iteration))+
#   geom_line()+
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)+
#   theme(legend.position = "none")
# 
# 
# summary_suppression_high_distance_density_by_t <- density_suppression_high_distance %>%
#   group_by(., t, LifeStage_pop) %>%
#   summarise_at(., vars(Density), vars(mean, median, min, max)) %>%
#   ungroup()
# 
# summary_suppression_high_distance_density_by_t %>%
#   ggplot(., aes(x = t, y = median))+
#   geom_line()+
#   geom_ribbon(aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# 
# (summary_suppression_high_distance_density <- summary_suppression_high_distance_density_by_t %>%
#     group_by(., LifeStage_pop) %>%
#     summarise_at(., vars(mean, median), funs(mean)) %>%
#     ungroup())
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

suppression_low_distance_pop_size_total <- suppression_low_distance$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  mutate(., PopSize               = rowSums(dplyr::select(., contains("_")))) %>% 
  mutate(., PopSize1              = rowSums(dplyr::select(., contains("_1")))) %>% 
  mutate(., PopSize2              = rowSums(dplyr::select(., contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  mutate(., MaturePopSize         = rowSums(dplyr::select(., contains("MA_")))) %>% 
  mutate(., MaturePopSize1        = rowSums(dplyr::select(., contains("MA_1")))) %>% 
  mutate(., MaturePopSize2        = rowSums(dplyr::select(., contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  mutate(., SaplingPopSize        = rowSums(dplyr::select(., contains("SAP_")))) %>% 
  mutate(., SaplingPopSize1       = rowSums(dplyr::select(., contains("SAP_1")))) %>% 
  mutate(., SaplingPopSize2       = rowSums(dplyr::select(., contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., matches("MA_|SAP_")))) %>% 
  mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., matches("MA_1|SAP_1")))) %>% 
  mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  mutate(., Density = PopSize/area)  %>% 
  mutate(., Density1 = PopSize1/(area/2))  %>% 
  mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  mutate(., MA_Density = MaturePopSize/area) %>% 
  mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  mutate(., SAP_Density = SaplingPopSize/area) %>% 
  mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  mutate(., Fire_scenario = "Suppression") %>% 
  mutate(., Dispersal_probability = "High dispersal") %>% 
  mutate(., Period = "Current")%>%
  mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(suppression_low_distance$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, - t)

suppression_low_distance_pop_size_total <- full_join(suppression_low_distance_pop_size_total, tSinceFire, by = "IterationTime")

rm(suppression_low_distance)

# density_suppression_low_distance <- suppression_low_distance_pop_size_total %>%
#   dplyr::select(., Iteration, t, Density, Density1, Density2,
#                 MA_Density, MA_Density1, MA_Density2,
#                 SAP_Density, SAP_Density1, SAP_Density2,
#                 SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2) %>%
#   gather(., LifeStage_pop, Density, -Iteration, -t)
# 
# 
# density_suppression_low_distance %>%
#   # filter(., t < 20) %>%
#   filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
#   ggplot(., aes(x = t, y = Density, col = Iteration))+
#   geom_line()+
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)+
#   theme(legend.position = "none")
# 
# 
# summary_suppression_low_distance_density_by_t <- density_suppression_low_distance %>%
#   group_by(., t, LifeStage_pop) %>%
#   summarise_at(., vars(Density), vars(mean, median, min, max)) %>%
#   ungroup()
# 
# summary_suppression_low_distance_density_by_t %>%
#   ggplot(., aes(x = t, y = median))+
#   geom_line()+
#   geom_ribbon(aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# 
# (summary_suppression_low_distance_density <- summary_suppression_low_distance_density_by_t %>%
#     group_by(., LifeStage_pop) %>%
#     summarise_at(., vars(mean, median), funs(mean)) %>%
#     ungroup())
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

decrease_high_distance_pop_size_total <- decrease_high_distance$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  mutate(., PopSize               = rowSums(dplyr::select(., contains("_")))) %>% 
  mutate(., PopSize1              = rowSums(dplyr::select(., contains("_1")))) %>% 
  mutate(., PopSize2              = rowSums(dplyr::select(., contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  mutate(., MaturePopSize         = rowSums(dplyr::select(., contains("MA_")))) %>% 
  mutate(., MaturePopSize1        = rowSums(dplyr::select(., contains("MA_1")))) %>% 
  mutate(., MaturePopSize2        = rowSums(dplyr::select(., contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  mutate(., SaplingPopSize        = rowSums(dplyr::select(., contains("SAP_")))) %>% 
  mutate(., SaplingPopSize1       = rowSums(dplyr::select(., contains("SAP_1")))) %>% 
  mutate(., SaplingPopSize2       = rowSums(dplyr::select(., contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., matches("MA_|SAP_")))) %>% 
  mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., matches("MA_1|SAP_1")))) %>% 
  mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  mutate(., Density = PopSize/area)  %>% 
  mutate(., Density1 = PopSize1/(area/2))  %>% 
  mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  mutate(., MA_Density = MaturePopSize/area) %>% 
  mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  mutate(., SAP_Density = SaplingPopSize/area) %>% 
  mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  mutate(., Fire_scenario = "Decreasing fire return interval") %>% 
  mutate(., Dispersal_probability = "Low dispersal") %>% 
  mutate(., Period = "Current")%>%
  mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(decrease_high_distance$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, - t)

decrease_high_distance_pop_size_total <- full_join(decrease_high_distance_pop_size_total, tSinceFire, by = "IterationTime")


rm(decrease_high_distance)

# density_decrease_high_distance <- decrease_high_distance_pop_size_total %>%
#   dplyr::select(., Iteration, t, Density, Density1, Density2,
#                 MA_Density, MA_Density1, MA_Density2,
#                 SAP_Density, SAP_Density1, SAP_Density2,
#                 SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2) %>%
#   gather(., LifeStage_pop, Density, -Iteration, -t)
# 
# 
# density_decrease_high_distance %>%
#   # filter(., t < 20) %>%
#   filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
#   ggplot(., aes(x = t, y = Density, col = Iteration))+
#   geom_line()+
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)+
#   theme(legend.position = "none")
# 
# 
# summary_decrease_high_distance_density_by_t <- density_decrease_high_distance %>%
#   group_by(., t, LifeStage_pop) %>%
#   summarise_at(., vars(Density), vars(mean, median, min, max)) %>%
#   ungroup()
# 
# summary_decrease_high_distance_density_by_t %>%
#   ggplot(., aes(x = t, y = median))+
#   geom_line()+
#   geom_ribbon(aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# 
# (summary_decrease_high_distance_density <- summary_decrease_high_distance_density_by_t %>%
#     group_by(., LifeStage_pop) %>%
#     summarise_at(., vars(mean, median), funs(mean)) %>%
#     ungroup())
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

decrease_low_distance_pop_size_total <- decrease_low_distance$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  mutate(., PopSize               = rowSums(dplyr::select(., contains("_")))) %>% 
  mutate(., PopSize1              = rowSums(dplyr::select(., contains("_1")))) %>% 
  mutate(., PopSize2              = rowSums(dplyr::select(., contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  mutate(., MaturePopSize         = rowSums(dplyr::select(., contains("MA_")))) %>% 
  mutate(., MaturePopSize1        = rowSums(dplyr::select(., contains("MA_1")))) %>% 
  mutate(., MaturePopSize2        = rowSums(dplyr::select(., contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  mutate(., SaplingPopSize        = rowSums(dplyr::select(., contains("SAP_")))) %>% 
  mutate(., SaplingPopSize1       = rowSums(dplyr::select(., contains("SAP_1")))) %>% 
  mutate(., SaplingPopSize2       = rowSums(dplyr::select(., contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., matches("MA_|SAP_")))) %>% 
  mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., matches("MA_1|SAP_1")))) %>% 
  mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  mutate(., Density = PopSize/area)  %>% 
  mutate(., Density1 = PopSize1/(area/2))  %>% 
  mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  mutate(., MA_Density = MaturePopSize/area) %>% 
  mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  mutate(., SAP_Density = SaplingPopSize/area) %>% 
  mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  mutate(., Fire_scenario = "Decreasing fire return interval") %>% 
  mutate(., Dispersal_probability = "High dispersal") %>% 
  mutate(., Period = "Current")%>%
  mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(decrease_low_distance$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, - t)

decrease_low_distance_pop_size_total <- full_join(decrease_low_distance_pop_size_total, tSinceFire, by = "IterationTime")


rm(decrease_low_distance)


pop1 <- decrease_low_distance_pop_size_total %>% 
  dplyr::select(., Iteration,t, contains("1")) %>% 
  mutate(., Pop = "pop1") %>% 
  mutate(., Dispersal_probability = "Low")

colnames(pop1) <- c("Iteration","t", "CS", "SD", "SAP", "MA", "PopSize", "MaturePopSize", "SaplingPopSize", "SaplingMaturePopSize",
                    "Density", "MA_Density", "SAP_Density", "SAP_MA_Density", "tSinceFire", "Pop", "Dispersal_probability")

pop2 <- decrease_low_distance_pop_size_total %>% 
  dplyr::select(., Iteration,t, contains("2")) %>% 
  mutate(., Pop = "pop2") %>% 
  mutate(., Dispersal_probability = "Low")

colnames(pop2) <- c("Iteration","t", "CS", "SD", "SAP", "MA", "PopSize", "MaturePopSize", "SaplingPopSize", "SaplingMaturePopSize",
                    "Density", "MA_Density", "SAP_Density", "SAP_MA_Density", "tSinceFire", "Pop", "Dispersal_probability")

pops <- bind_rows(pop1, pop2)

pops %>% 
  group_by(t, Pop) %>% 
  summarise_at(., vars(CS, SD), funs( median)) %>% 
  ungroup() %>% 
  gather(., Stage, Count, -Pop, -t) %>% 
  ggplot(.,aes(., x = t, y = Count, col = Stage))+
  geom_line()+
  facet_grid(~Pop)

# density_decrease_low_distance <- decrease_low_distance_pop_size_total %>%
#   dplyr::select(., Iteration, t, Density, Density1, Density2,
#                 MA_Density, MA_Density1, MA_Density2,
#                 SAP_Density, SAP_Density1, SAP_Density2,
#                 SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2) %>%
#   gather(., LifeStage_pop, Density, -Iteration, -t)
# 
# 
# density_decrease_low_distance %>%
#   # filter(., t < 20) %>%
#   filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
#   ggplot(., aes(x = t, y = Density, col = Iteration))+
#   geom_line()+
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)+
#   theme(legend.position = "none")
# 
# 
# summary_decrease_low_distance_density_by_t <- density_decrease_low_distance %>%
#   group_by(., t, LifeStage_pop) %>%
#   summarise_at(., vars(Density), vars(mean, median, min, max)) %>%
#   ungroup()
# 
# summary_decrease_low_distance_density_by_t %>%
#   ggplot(., aes(x = t, y = median))+
#   geom_line()+
#   geom_ribbon(aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# 
# (summary_decrease_low_distance_density <- summary_decrease_low_distance_density_by_t %>%
#     group_by(., LifeStage_pop) %>%
#     summarise_at(., vars(mean, median), funs(mean)) %>%
#     ungroup())
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

constant_high_distance_historic_pop_size_total <- constant_high_distance_historic$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  mutate(., PopSize               = rowSums(dplyr::select(., contains("_")))) %>% 
  mutate(., PopSize1              = rowSums(dplyr::select(., contains("_1")))) %>% 
  mutate(., PopSize2              = rowSums(dplyr::select(., contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  mutate(., MaturePopSize         = rowSums(dplyr::select(., contains("MA_")))) %>% 
  mutate(., MaturePopSize1        = rowSums(dplyr::select(., contains("MA_1")))) %>% 
  mutate(., MaturePopSize2        = rowSums(dplyr::select(., contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  mutate(., SaplingPopSize        = rowSums(dplyr::select(., contains("SAP_")))) %>% 
  mutate(., SaplingPopSize1       = rowSums(dplyr::select(., contains("SAP_1")))) %>% 
  mutate(., SaplingPopSize2       = rowSums(dplyr::select(., contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., matches("MA_|SAP_")))) %>% 
  mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., matches("MA_1|SAP_1")))) %>% 
  mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  mutate(., Density = PopSize/area)  %>% 
  mutate(., Density1 = PopSize1/(area/2))  %>% 
  mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  mutate(., MA_Density = MaturePopSize/area) %>% 
  mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  mutate(., SAP_Density = SaplingPopSize/area) %>% 
  mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  mutate(., Fire_scenario = "Historic fire return interval") %>% 
  mutate(., Dispersal_probability = "Low dispersal") %>% 
  mutate(., Period = "Historic")%>% 
  mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(constant_high_distance_historic$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, - t)

constant_high_distance_historic_pop_size_total <- full_join(constant_high_distance_historic_pop_size_total, tSinceFire, by = "IterationTime")




# density_constant_high_distance_historic <- constant_high_distance_historic_pop_size_total %>%
#   # filter(., t < 250) %>%
#   dplyr::select(., Iteration.x, t.x, Density, Density1, Density2,
#                 MA_Density, MA_Density1, MA_Density2,
#                 SAP_Density, SAP_Density1, SAP_Density2,
#                 SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2, tSinceFire1, tSinceFire2) %>%
#   gather(., LifeStage_pop, Density, -Iteration.x, -t.x, -tSinceFire1, -tSinceFire2)
# 
# 
# density_constant_high_distance_historic %>%
#   # filter(., t < 20) %>%
#   filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
#   ggplot(., aes(x = t, y = Density, col = Iteration))+
#   geom_line()+
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)+
#   theme(legend.position = "none")
# 
# 
# summary_constant_high_distance_historic_density_by_t <- density_constant_high_distance_historic %>%
#   # filter(., t <250) %>%
#   group_by(., t, LifeStage_pop) %>%
#   summarise_at(., vars(Density), vars(mean, median, min, max)) %>%
#   ungroup()
# 
# summary_constant_high_distance_historic_density_by_t %>%
#   ggplot(., aes(x = t, y = median))+
#   geom_line()+
#   geom_ribbon(aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# 
# (summary_constant_high_distance_historic_density <- summary_constant_high_distance_historic_density_by_t %>%
#     group_by(., LifeStage_pop) %>%
#     summarise_at(., vars(mean, median), funs(mean)) %>%
#     ungroup())
# 
# write.csv(summary_constant_high_distance_historic_density, 
#           file = paste0(proj_dir, export_table_dir, "constant high restoration density.csv"))
# 
# rm(summary_constant_high_distance_historic_density_by_t, summary_constant_high_distance_historic_density, density_constant_high_distance_historic)

# constant_high_distance_historic_pop_size_total <- constant_high_distance_historic_pop_size_total %>% 
# dplyr::select(., Iteration, t, Density, Density1, Density2, SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2)

# 8) Constant Low Distance Restoration
#---------------------------------------|---------------------------------------
#                         Constant FRI, low distance_restore                   
#---------------------------------------|---------------------------------------

constant_low_distance_historic_pop_size_total <- constant_low_distance_historic$pop_sizes %>% 
  dplyr::select(., -SEED2_1, - SEED2_2) %>% 
  # Total population size across both populations, pop1 and pop2
  mutate(., PopSize               = rowSums(dplyr::select(., contains("_")))) %>% 
  mutate(., PopSize1              = rowSums(dplyr::select(., contains("_1")))) %>% 
  mutate(., PopSize2              = rowSums(dplyr::select(., contains("_2")))) %>%
  # No. mature trees across both populations, pop1 and pop2
  mutate(., MaturePopSize         = rowSums(dplyr::select(., contains("MA_")))) %>% 
  mutate(., MaturePopSize1        = rowSums(dplyr::select(., contains("MA_1")))) %>% 
  mutate(., MaturePopSize2        = rowSums(dplyr::select(., contains("MA_2")))) %>% 
  # No. saplings across both populations, pop1 and pop2
  mutate(., SaplingPopSize        = rowSums(dplyr::select(., contains("SAP_")))) %>% 
  mutate(., SaplingPopSize1       = rowSums(dplyr::select(., contains("SAP_1")))) %>% 
  mutate(., SaplingPopSize2       = rowSums(dplyr::select(., contains("SAP_2")))) %>% 
  # No. mature trees & saplings across both populations, pop1 and pop2
  mutate(., SaplingMaturePopSize  = rowSums(dplyr::select(., matches("MA_|SAP_")))) %>% 
  mutate(., SaplingMaturePopSize1 = rowSums(dplyr::select(., matches("MA_1|SAP_1")))) %>% 
  mutate(., SaplingMaturePopSize2 = rowSums(dplyr::select(., matches("MA_2|SAP_2")))) %>% 
  # Density of all individuals across both populations, pop1 and pop2
  mutate(., Density = PopSize/area)  %>% 
  mutate(., Density1 = PopSize1/(area/2))  %>% 
  mutate(., Density2 = PopSize2/(area/2))  %>% 
  # Density of mature trees across both populations, pop1 and pop2
  mutate(., MA_Density = MaturePopSize/area) %>% 
  mutate(., MA_Density1 = MaturePopSize1/(area/2)) %>% 
  mutate(., MA_Density2 = MaturePopSize2/(area/2)) %>% 
  # Density of saplings across both populations, pop1 and pop2
  mutate(., SAP_Density = SaplingPopSize/area) %>% 
  mutate(., SAP_Density1 = SaplingPopSize1/(area/2)) %>% 
  mutate(., SAP_Density2 = SaplingPopSize2/(area/2)) %>% 
  # Density of mature trees and saplings across both populations, pop1 and pop2
  mutate(., SAP_MA_Density = SaplingMaturePopSize/area) %>% 
  mutate(., SAP_MA_Density1 = SaplingMaturePopSize1/(area/2)) %>% 
  mutate(., SAP_MA_Density2 = SaplingMaturePopSize2/(area/2)) %>% 
  mutate(., Fire_scenario = "Historic fire return interval") %>% 
  mutate(., Dispersal_probability = "High dispersal")%>% 
  mutate(., Period = "Historic")%>% 
  mutate(., IterationTime = paste(Iteration, t))

tSinceFire <- as.data.frame(constant_low_distance_historic$tSinceFire_tracker)
colnames(tSinceFire) <- c("Iteration", "t", "tSinceFire1", "tSinceFire2")
tSinceFire <- tSinceFire %>% 
  mutate(., IterationTime = paste(Iteration, t)) %>% 
  dplyr::select(., -Iteration, - t)

constant_low_distance_historic_pop_size_total <- full_join(constant_low_distance_historic_pop_size_total, tSinceFire, by = "IterationTime")

historical <- bind_rows(constant_high_distance_historic_pop_size_total, constant_low_distance_historic_pop_size_total)

pop1 <- historical %>% 
  dplyr::select(., Iteration, Fire_scenario, Dispersal_probability, contains("1")) %>% 
  mutate(., Pop = "pop1") 

colnames(pop1) <- c("Iteration","Fire_scenario", "Dispersal_probability", "CS", "SD", "SAP", "MA", "PopSize", "MaturePopSize", "SaplingPopSize", "SaplingMaturePopSize",
                    "Density", "MA_Density", "SAP_Density", "SAP_MA_Density", "tSinceFire", "Pop")

pop2 <- historical %>% 
  dplyr::select(., Iteration, Fire_scenario, Dispersal_probability, contains("2")) %>% 
  mutate(., Pop = "pop2") 

colnames(pop2) <- c("Iteration","Fire_scenario", "Dispersal_probability", "CS", "SD", "SAP", "MA", "PopSize", "MaturePopSize", "SaplingPopSize", "SaplingMaturePopSize",
                    "Density", "MA_Density", "SAP_Density", "SAP_MA_Density", "tSinceFire", "Pop")

pops <- bind_rows(pop1, pop2)

rm(pop1, pop2)

density_tSinceFire <-  pops %>% 
  group_by(., tSinceFire, Fire_scenario, Dispersal_probability) %>% 
  summarise_at(., vars(MA_Density, SAP_MA_Density, Density), funs(mean, median, min, lower, q1, q3, upper, max)) %>% 
  ungroup() 


(year30density <- pops %>% 
  filter(., tSinceFire == 30) %>% 
  summarise_at(., vars(MA_Density, SAP_Density, SAP_MA_Density, Density), funs(mean, median, min, lower, q1, q3, upper, max)))


write.csv(year30density, file = paste0(proj_dir, export_table_dir, "30 Year Post Fire Density Historical dispersal averaged.csv"))


year100200300400500 <- pops %>% 
  filter(., tSinceFire == 100 |
            tSinceFire == 200 |
           tSinceFire == 300 |
           tSinceFire == 400 |
           tSinceFire == 500) %>% 
  group_by(., Dispersal_probability, as.factor(tSinceFire)) %>% 
  summarise_at(., vars(SAP_MA_Density, Density), funs(mean, median, min, lower, q1, q3, upper, max)) %>% 
  ungroup()

#write.csv(year100200300400500, file = paste0(proj_dir, export_table_dir, "Many Year Post Fire Density Historic.csv"))

# tiff(filename = paste0(proj_dir, figures_dir, "Final historic SAP MA density by tSinceFire.tiff"),
#      width = 12,
#      height = 8.5,
#      units = "in",
#      res = 300,
#      compression = "lzw")

density_tSinceFire %>% 
  dplyr::mutate(., Fire_scenario = ifelse(Fire_scenario=='Decreasing fire return interval', 'Decreasing',
                                          ifelse(Fire_scenario=='Historic fire return interval', 'Historical conditions',
                                                 ifelse(Fire_scenario=='Suppression', 'Suppression', NA)))) %>%
  ggplot(data = ., aes(x = tSinceFire, y = SAP_MA_Density_median, fill = Dispersal_probability))+
  
  geom_ribbon(aes(x = tSinceFire, ymin = SAP_MA_Density_min, ymax = SAP_MA_Density_max,
                                             fill = Dispersal_probability), alpha = 0.2)+
  scale_fill_manual(values = c("#ef8a62", "#67a9cf")) +
  geom_ribbon(aes(x = tSinceFire, ymin = SAP_MA_Density_q1, ymax = SAP_MA_Density_q3,
                                             col = Dispersal_probability), alpha = 0.05, lty = 2) +
  scale_fill_manual(values = c("#ef8a62", "#67a9cf")) +
  geom_line(aes(col = Dispersal_probability), lwd = 1.5) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  theme_bw() +
  labs(x = "\nTime since fire (years)", y = expression(paste("Median sapling and mature tree density (no. trees/", m^2,")")))+
  theme(axis.text = element_text(size = axis_text_size)) +
  theme(axis.title = element_text(size = axis_label_text_size))+
  theme(legend.position = "none") +
  facet_grid(~Fire_scenario) +
  theme(strip.background = element_rect(colour = "black", fill = "white"),
        strip.text       = element_text(colour = "black",  size = strip_text_size))+
  theme(plot.margin = grid::unit(c(3, 1, 1, 1), "lines"))  # top, right, bottom, left

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

rm(constant_low_distance_historic, pops, tSinceFire, legd, density_tSinceFire, constant_high_distance_historic)

# density_constant_low_distance_historic <- constant_low_distance_historic_pop_size_total %>%
#   dplyr::select(., Iteration, t, Density, Density1, Density2,
#                 MA_Density, MA_Density1, MA_Density2,
#                 SAP_Density, SAP_Density1, SAP_Density2,
#                 SAP_MA_Density, SAP_MA_Density1, SAP_MA_Density2) %>%
#   gather(., LifeStage_pop, Density, -Iteration, -t)
# 
# 
# density_constant_low_distance_historic %>%
#   # filter(., t < 20) %>%
#   filter(., Iteration %in% sample(1:5000, 100, replace = F)) %>%
#   ggplot(., aes(x = t, y = Density, col = Iteration))+
#   geom_line()+
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)+
#   theme(legend.position = "none")
# 
# 
# summary_constant_low_distance_historic_density_by_t <- density_constant_low_distance_historic %>%
#   group_by(., t, LifeStage_pop) %>%
#   summarise_at(., vars(Density), vars(mean, median, min, max)) %>%
#   ungroup()
# 
# summary_constant_low_distance_historic_density_by_t %>%
#   ggplot(., aes(x = t, y = median))+
#   geom_line()+
#   geom_ribbon(aes(x = t, ymin = min, ymax = max), alpha = 0.5) +
#   facet_wrap(~LifeStage_pop, scale = "free", ncol = 3)
# 
# (summary_constant_low_distance_historic_density <- summary_constant_low_distance_historic_density_by_t %>%
#     group_by(., LifeStage_pop) %>%
#     summarise_at(., vars(mean, median), funs(mean)) %>%
#     ungroup())
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


current_models <- bind_rows(constant_low_distance_pop_size_total, 
                            constant_high_distance_pop_size_total,
                            suppression_low_distance_pop_size_total,
                            suppression_high_distance_pop_size_total,
                            decrease_low_distance_pop_size_total,
                            decrease_high_distance_pop_size_total) %>% 
  mutate(., Treatment = paste(Fire_scenario, Dispersal_probability, Period))

unique(current_models$Treatment)

rm(constant_high_distance_pop_size_total, constant_low_distance_pop_size_total,
   suppression_high_distance_pop_size_total, suppression_low_distance_pop_size_total,
   decrease_high_distance_pop_size_total, decrease_low_distance_pop_size_total)


historic_models <- bind_rows(constant_low_distance_historic_pop_size_total,
                                constant_high_distance_historic_pop_size_total) %>% 
  mutate(., Treatment = paste(Fire_scenario, Dispersal_probability, Period))

unique(historic_models$Treatment)

rm(constant_low_distance_historic_pop_size_total,
   constant_high_distance_historic_pop_size_total)
   

current_model_summary <- current_models %>% 
  group_by(., Fire_scenario, Dispersal_probability, t) %>% 
  summarise_at(., vars(Density, SAP_Density, MA_Density, SAP_MA_Density), 
               funs(mean, median, min, lower, q1, q3, upper, max)) %>% 
  ungroup() %>% 
  mutate(., Period   = "Current")


historic_model_summary <- historic_models %>% 
  group_by(., Fire_scenario, Dispersal_probability, t) %>% 
  summarise_at(., vars(Density, SAP_Density, MA_Density, SAP_MA_Density), 
               funs(mean, median, min, lower, q1, q3, upper, max)) %>% 
  ungroup() %>% 
  mutate(., Period   = "Historic")


summary_frame <- bind_rows(historic_model_summary, current_model_summary)

summary_frame$Fire_scenario <- factor(summary_frame$Fire_scenario, 
                                      levels = c("Historic fire return interval", "Suppression", "Decreasing fire return interval"))

rm(historic_model_summary, current_model_summary)


# #---------------------------------------|---------------------------------------
# #               Total population trajectory (ALL LIFE STAGES BUT SEEDS)                  
# #---------------------------------------|---------------------------------------
# 
# tiff(filename = paste0(proj_dir, figures_dir, "Final Current SAP MA Population Trajectory Figure.tiff"),
#      width = 12,
#      height = 8.5,
#      units = "in",
#      res = 300,
#      compression = "lzw")

(MA_SAP_population_plot <- summary_frame %>%
    # filter(., t < 200) %>%
   filter(., Period == "Current") %>%
    ggplot(data = .,
           aes(x     = t,
               y     = SAP_MA_Density_median,
               group = Dispersal_probability))+
    geom_ribbon(aes(x    = t,
                    ymin = SAP_MA_Density_min,
                    ymax = SAP_MA_Density_max,
                    fill = Dispersal_probability),
                alpha = 0.3)+
    scale_fill_manual(values = c("#ef8a62", "#67a9cf")) +
   geom_ribbon(aes(x    = t, 
                   ymin = SAP_MA_Density_q1, 
                   ymax = SAP_MA_Density_q3,
                   col  = Dispersal_probability), alpha = 0.05, lty = 2) +
   scale_fill_manual(values = c("#ef8a62", "#67a9cf")) +
    geom_line(aes(x   = t,
                  y   = SAP_MA_Density_median,
                  col = Dispersal_probability),
              lwd = 1.5) +
    scale_color_manual(values = c("#ef8a62", "#67a9cf"))+
    facet_grid(~ Fire_scenario)+ # free_x removes extra y axis tick labels.... go figure
    theme_bw() +
    theme(strip.background = element_rect(colour = "black", fill = "white"),
          strip.text       = element_text(colour = "black",  size = strip_text_size))+
    theme(axis.title      = element_text(size = axis_label_text_size))+
    theme(axis.text       = element_text(size = axis_text_size))+
    theme(legend.position = "none") +
    theme(plot.margin = grid::unit(c(2.5, 1, 1, 1), "lines")) + # top, right, bottom, left
    scale_x_continuous(breaks = seq(0, 500, by = 100))+
    labs(x = "Time (years)",
         y = expression(paste("Median sapling and mature tree density (no. trees/", m^2,")")))+
    geom_hline(yintercept = 0, col = "black", lty = 2))


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
    filter(., Period == "Historic") %>%
    ggplot(data = .,
           aes(x     = t,
               y     = SAP_MA_Density_median,
               group = Dispersal_probability))+
    geom_ribbon(aes(x    = t,
                    ymin = SAP_MA_Density_min,
                    ymax = SAP_MA_Density_max,
                    fill = Dispersal_probability),
                alpha = 0.3)+
    scale_fill_manual(values = c("#ef8a62", "#67a9cf")) +
    geom_ribbon(aes(x    = t,
                    ymin = SAP_MA_Density_q1,
                    ymax = SAP_MA_Density_q3,
                    col = Dispersal_probability),
                alpha = 0.05, lty = 2)+
    scale_fill_manual(values = c("#ef8a62", "#67a9cf")) +
    geom_line(aes(x   = t,
                  y   = SAP_MA_Density_median,
                  col = Dispersal_probability),
              lwd = 1.5) +
    scale_color_manual(values = c("#ef8a62", "#67a9cf"))+
    facet_grid(~Period, scales = "free")+ # free_x removes extra y axis tick labels.... go figure
    theme_bw() +
    theme(strip.background = element_rect(colour = "black", fill = "white"),
          strip.text       = element_text(colour = "black",  size = strip_text_size))+
    theme(axis.title      = element_text(size = axis_label_text_size))+
    theme(axis.text       = element_text(size = axis_text_size))+
    theme(legend.position = "none") +
    theme(plot.margin = grid::unit(c(2.5, 1, 1, 1), "lines")) + # top, right, bottom, left
    scale_x_continuous(breaks = seq(0, 500, by = 125))+
    labs(x = "Time (years)",
         y = expression(paste("Median sapling and mature tree density (no. trees/", m^2,")")))+
    geom_hline(yintercept = 0, col = "black", lty = 2))


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
  # group_by(., Iteration, Fire_scenario, Dispersal_probability) %>%
  filter(., Density < 2e-06) %>%  # Low threshold that is reported in results
  # filter(., Density < 2e-04) %>%  # High threshold to show information about suppression
  # ungroup() %>%
  arrange(., Iteration, t) %>%
  group_by(., Iteration, Fire_scenario, Dispersal_probability) %>%
  filter(., t == min(t)) %>%
  ungroup() %>%
  mutate(., Period = "Current")

test <- extinct_current %>%
  group_by(., Treatment, Iteration) %>%
  tally() %>%
  ungroup()

sum(test$n >1)

rm(test)

extinct_historic <- historic_models %>%
  group_by(., Iteration, Fire_scenario, Dispersal_probability) %>%
  filter(., Density < 2e-06) %>%  # Low threshold that is reported in results
  # filter(., Density < 2e-04) %>%  # High threshold to show information about suppression
  # ungroup() %>%
  arrange(., Iteration, t) %>%
  group_by(., Iteration, Fire_scenario, Dispersal_probability) %>%
  filter(., t == min(t)) %>%
  ungroup() %>%
  mutate(., Period = "Historic")

test <- extinct_historic %>%
  group_by(., Treatment, Iteration) %>%
  tally() %>%
  ungroup()

sum(test$n >1)

rm(test)

extinct <- bind_rows(extinct_current, extinct_historic)


# ggplot(data = extinct, aes(x = t, fill = Dispersal_probability))+
#   geom_density(alpha = 0.5)+
#   facet_grid( Period ~ Fire_scenario + Dispersal_probability )

(median_extinction_time <- extinct %>%
    group_by(Period, Fire_scenario, Dispersal_probability) %>%
    summarise_at(., vars(t), funs(min, lower, q1, median,mean, q3, upper, max)) %>%
    ungroup())

median_extinction_time_export <- median_extinction_time %>% 
  dplyr::select(., Period, Fire_scenario, Dispersal_probability, q1, median, q3) %>% 
  mutate(., `Time to extirpation` = paste0(round(median, 3), "(", round(q1,3), ", ",round(q3,3),")" )) %>% 
  dplyr::select(., Period, Fire_scenario, Dispersal_probability, `Time to extirpation`)

# write.csv(median_extinction_time_export, file = paste0(proj_dir, export_table_dir, "Final? extirpation times.csv"))

pd <- position_dodge(0.4)

ggplot(data = median_extinction_time,
       aes(x   = Fire_scenario,
           y   = median,
           col = Dispersal_probability))+
  geom_linerange(data = median_extinction_time,
                 aes(x        = Fire_scenario,
                     y        = median,
                     colour   = Dispersal_probability,
                     ymin     = q1,
                     ymax     = q3),
                 lwd      = 1.25,
                 position = pd) +
  geom_point(position = pd, pch = 21,fill = "white", size = 3, stroke = 2)+
  facet_wrap(~Period) +
  theme_bw() +
  theme(strip.background = element_rect(colour = "black", fill = "white"),
        strip.text       = element_text(colour = "black",  size = strip_text_size))+
  theme(axis.title   = element_text(size = axis_label_text_size))+
  theme(axis.text    = element_text(size = axis_text_size))+
  theme(strip.text.x = element_text(size = strip_text_size))+
  theme(legend.text  = element_text(size= axis_text_size)) +
  theme(legend.title = element_text(size= axis_label_text_size))+
  scale_color_discrete("Dispersal\nprobability")+
  scale_fill_discrete("Dispersal\nprobability")+
  labs(x = "Fire scenario", y = "Median time to extirpation")


################################################################################
#---------------------------------------|---------------------------------------
#                       Cumulative probability of extirpation
#---------------------------------------|---------------------------------------
################################################################################


Cum_extirpation <- extinct %>% 
  mutate(., Iteration = as.numeric(Iteration)) %>% 
  filter(., Period == "Current") %>% 
  mutate(., Extirpation = 1) %>% 
  dplyr::select(., Iteration, t, Fire_scenario, Dispersal_probability, Extirpation) %>% 
  complete(.,Iteration             = 1:5000, 
           t                     = 1:500,
           Fire_scenario         = unique(extinct$Fire_scenario), 
           Dispersal_probability = unique(extinct$Dispersal_probability),
           fill                  = list(Extirpation = 0)) %>% 
  group_by(., Fire_scenario, Dispersal_probability, t) %>% 
  summarise_at(., vars(Extirpation), funs(sum)) %>% 
  ungroup() %>% 
  group_by(., Fire_scenario, Dispersal_probability) %>% 
  mutate(., CumSum = cumsum(Extirpation)) %>% 
  mutate(., PExt = CumSum/5000) %>% 
  ungroup()


Cum_extirpation %>% filter(., t == 500)

# tiff(filename = paste0(proj_dir, figures_dir, "Final cumulative extirpation.tiff"),
#      width = 12,
#      height = 8.5,
#      units = "in",
#      res = 300,
#      compression = "lzw")

Cum_extirpation %>% 
  ggplot(data = ., aes(x = t, y = PExt, col = Fire_scenario))+
  geom_line(data = Cum_extirpation, aes(x = t, y = PExt, lty = Dispersal_probability), lwd = 1.5)+
  # scale_colour_manual(
  #   "CI horizontal line", values = c("#d73027", "#1a9850"),
  #   guide=guide_legend(override.aes = list(colour=c("#d73027", "#1a9850"))),
  #   labels=c("Decreasing fire return interval", "Historical fire return interval")
  # ) +
  scale_colour_manual(values = c("#d73027", "#1a9850")) +
  # facet_grid(~ Dispersal_probability) +
  theme_bw() +
  labs(x = "\nTime (years)",
       y = "Cumulative extirpation probability \n") +
  theme(strip.background = element_rect(colour = "black", fill = "white"),
        strip.text       = element_text(colour = "black",  size = strip_text_size)) +
  # theme(legend.position = "none") +
  theme(legend.position = "top", legend.box = "vertical")+
  guides(linetype = guide_legend(keywidth = 3, keyheight = 1)) +
  theme(legend.text = element_text(size = axis_text_size))+
  theme(legend.title = element_blank())+
  theme(axis.title = element_text(size = axis_label_text_size - 1)) +
  theme(axis.text  = element_text(size = axis_text_size)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))+
  theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines"))# top, right, bottom, left


# legd <- grid::legendGrob(c("Historic fire return interval", "Suppression","Decreasing fire return interval"),
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
  mutate(., Iteration = as.numeric(Iteration)) %>% 
  filter(., Period == "Historic") %>% 
  mutate(., Extirpation = 1) %>% 
  dplyr::select(., Iteration, t, Fire_scenario, Dispersal_probability, Extirpation) %>% 
  complete(.,Iteration             = 1:5000, 
           t                     = 1:500,
           Fire_scenario         = unique(extinct$Fire_scenario), 
           Dispersal_probability = unique(extinct$Dispersal_probability),
           fill                  = list(Extirpation = 0)) %>% 
  group_by(., Fire_scenario, Dispersal_probability, t) %>% 
  summarise_at(., vars(Extirpation), funs(sum)) %>% 
  ungroup() %>% 
  group_by(., Fire_scenario, Dispersal_probability) %>% 
  mutate(., CumSum = cumsum(Extirpation)) %>% 
  mutate(., PExt = CumSum/5000) %>% 
  ungroup()


Cum_extirpation_historical %>% filter(., t == 500)



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
  filter(Fire_scenario == "Historic fire return interval") %>%
  group_by(.,Period) %>% 
  tally() %>% 
  ungroup()

# extirpation_150_historic[2,] <- c("Historic", 0) # only if time horizon is 100

historic_table <- extirpation_150_historic %>%
  dplyr::mutate(., Extirpations = as.numeric(n)) %>%
  mutate(., Surviving = 2*reps - Extirpations) %>% 
  dplyr::select(., -n) 

period <- as.character(historic_table$Period)
outcomes <- c("Etirpation", "Surviving")

(dat <- matrix(c(as.numeric(historic_table$Extirpations), as.numeric(historic_table$Surviving)), byrow = F, nrow = nrow(historic_table)))
dimnames(dat) <- list("Period" = period, "Outcome" = outcomes)

(HistoricFRI_ORs <- fisher.test(dat))

HistoricFRI_ORs <- data.frame(OR = HistoricFRI_ORs$estimate, LL = HistoricFRI_ORs$conf.int[1], UL = HistoricFRI_ORs$conf.int[2]) %>% 
  mutate(., Comparison = "Current/Historic") %>% 
  mutate(., FireReturnInterval = "Historic") %>% 
  mutate(., DispersalProbability = "All")

#---------------------------------------|--------------------------------------------------
#                                  Fire 
# Do changes to fire frequency, across all dispersal probabilities (fire sizes), increase
# odds of extirpation
#---------------------------------------|---------------------------------------------------

extirpation_150_fire <- extinct %>% 
  filter(., Period == "Current") %>% 
  # filter(., t <= 150) %>%
  group_by(.,Fire_scenario) %>% 
  tally() %>% 
  ungroup()

fire_table <- extirpation_150_fire %>% 
  dplyr::rename(., Extirpations = n) %>% 
  mutate(., Surviving = reps * nrow(extirpation_150_fire) - Extirpations)# %>% 
# mutate(., Odds = Extirpations/Surviving) %>% 
# dplyr::select(., -n, -Extirpations, - Surviving)  %>% 
# mutate(., DecreasingHighNo  = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal No Restoration"], nrow(test))) %>% 
# mutate(., DecreasingHighYes = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal Restoration"], nrow(test))) %>% 
# mutate(., )

fire <- as.character(fire_table$Fire_scenario)
outcomes <- c("Etirpation", "Surviving")

(dat <- matrix(c(as.numeric(fire_table$Extirpations), as.numeric(fire_table$Surviving)), byrow = F, nrow = nrow(fire_table)))
dimnames(dat) <- list("Fire" = fire, "Outcome" = outcomes)

(fire_ORs <- fisher.test(dat))

fire_ORs <- data.frame(OR = fire_ORs$estimate, LL = fire_ORs$conf.int[1], UL = fire_ORs$conf.int[2]) %>% 
  mutate(., Comparison = "Decreasing/Historic")  %>% 
  mutate(., FireReturnInterval = "All") %>% 
  mutate(., DispersalProbability = "All")
  
#---------------------------------------|---------------------------------------
#                                  Dispersal Decreasing
#  Does dispersal probability, across all fire frequencies (fire sizes) change
# odds of extirpation
#---------------------------------------|---------------------------------------

extirpation_150_dispersal_decreasing <- extinct %>% 
  filter(., Period == "Current" & Fire_scenario == "Decreasing fire return interval") %>% 
  # filter(., t <= 150) %>%
  group_by(.,Dispersal_probability) %>% 
  tally() %>% 
  ungroup()


dispersal_table_decreasing <- extirpation_150_dispersal_decreasing %>% 
  dplyr::select(., Dispersal_probability, n) %>% 
  dplyr::rename(., Extirpations = n) %>% 
  mutate(., Surviving = reps - Extirpations)# %>% 
# mutate(., Odds = Extirpations/Surviving) %>% 
# dplyr::select(., -n, -Extirpations, - Surviving)  %>% 
# mutate(., DecreasingHighNo  = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal No Restoration"], nrow(test))) %>% 
# mutate(., DecreasingHighYes = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal Restoration"], nrow(test))) %>% 
# mutate(., )

dispersal <- as.character(dispersal_table_decreasing$Dispersal_probability)
outcomes <- c("Etirpation", "Surviving")

(dat <- matrix(c(as.numeric(dispersal_table_decreasing$Extirpations), as.numeric(dispersal_table_decreasing$Surviving)), byrow = F, nrow = nrow(dispersal_table_decreasing)))
dimnames(dat) <- list("Dispersal" = dispersal, "Outcome" = outcomes)

(dispersal_Current_ORs_decreasing <- fisher.test(dat))

dispersal_Current_ORs_decreasing <- data.frame(OR = dispersal_Current_ORs_decreasing$estimate, 
                                               LL = dispersal_Current_ORs_decreasing$conf.int[1], 
                                               UL = dispersal_Current_ORs_decreasing$conf.int[2]) %>% 
  mutate(., Comparison = "High/Low") %>% 
  mutate(., FireReturnInterval = "Decreasing") %>% 
  mutate(., DispersalProbability = "All")

#---------------------------------------|---------------------------------------
#                                  Dispersal Decreasing
#  Does dispersal probability, across all fire frequencies (fire sizes) change
# odds of extirpation
#---------------------------------------|---------------------------------------

extirpation_150_dispersal_historic <- extinct %>% 
  filter(., Period == "Current" & Fire_scenario == "Historic fire return interval") %>% 
  # filter(., t <= 150) %>%
  group_by(.,Dispersal_probability) %>% 
  tally() %>% 
  ungroup()


dispersal_table_historic <- extirpation_150_dispersal_historic %>% 
  dplyr::select(., Dispersal_probability, n) %>% 
  dplyr::rename(., Extirpations = n) %>% 
  mutate(., Surviving = reps - Extirpations)# %>% 
# mutate(., Odds = Extirpations/Surviving) %>% 
# dplyr::select(., -n, -Extirpations, - Surviving)  %>% 
# mutate(., DecreasingHighNo  = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal No Restoration"], nrow(test))) %>% 
# mutate(., DecreasingHighYes = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal Restoration"], nrow(test))) %>% 
# mutate(., )

dispersal <- as.character(dispersal_table_historic$Dispersal_probability)
outcomes <- c("Etirpation", "Surviving")

(dat <- matrix(c(as.numeric(dispersal_table_historic$Extirpations), as.numeric(dispersal_table_historic$Surviving)), byrow = F, nrow = nrow(dispersal_table_historic)))
dimnames(dat) <- list("Dispersal" = dispersal, "Outcome" = outcomes)

(dispersal_Current_ORs_historic <- fisher.test(dat))



dispersal_Current_ORs_historic <- data.frame(OR = dispersal_Current_ORs_historic$estimate, 
                                             LL = dispersal_Current_ORs_historic$conf.int[1], 
                                             UL = dispersal_Current_ORs_historic$conf.int[2]) %>% 
  mutate(., Comparison = "High/Low") %>% 
  mutate(., FireReturnInterval = "Historic") %>% 
  mutate(., DispersalProbability = "All")


#---------------------------------------|---------------------------------------
#                                  High dispersal
#---------------------------------------|---------------------------------------

extirpation_150_highDispersal_fire <- extinct %>% 
  filter(., Period == "Current" & Dispersal_probability == "High dispersal") %>% 
  # filter(., t <= 150) %>%
  group_by(.,Fire_scenario) %>% 
  tally() %>% 
  ungroup()


fire_high_dispersal_table <- extirpation_150_highDispersal_fire %>% 
  dplyr::select(., Fire_scenario, n) %>% 
  dplyr::rename(., Extirpations = n) %>% 
  mutate(., Surviving = reps - Extirpations)# %>% 
# mutate(., Odds = Extirpations/Surviving) %>% 
# dplyr::select(., -n, -Extirpations, - Surviving)  %>% 
# mutate(., DecreasingHighNo  = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal No Restoration"], nrow(test))) %>% 
# mutate(., DecreasingHighYes = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal Restoration"], nrow(test))) %>% 
# mutate(., )

fire <- as.character(fire_high_dispersal_table$Fire_scenario)
outcomes <- c("Etirpation", "Surviving")

(dat <- matrix(c(as.numeric(fire_high_dispersal_table$Extirpations), as.numeric(fire_high_dispersal_table$Surviving)), byrow = F, nrow = nrow(fire_high_dispersal_table)))
dimnames(dat) <- list("Fire" = fire, "Outcome" = outcomes)

(fire_high_dispersal_ORs <- fisher.test(dat))


fire_high_dispersal_ORs <- data.frame(OR = fire_high_dispersal_ORs$estimate, 
                                      LL = fire_high_dispersal_ORs$conf.int[1], 
                                      UL = fire_high_dispersal_ORs$conf.int[2]) %>% 
  mutate(., Comparison = "Decreasing/Historic") %>% 
  mutate(., FireReturnInterval = "All") %>% 
  mutate(., DispersalProbability = "High")


#---------------------------------------|---------------------------------------
#                                  Low dispersal
#---------------------------------------|---------------------------------------

extirpation_150_lowDispersal_fire <- extinct %>% 
  filter(., Period == "Current" & Dispersal_probability == "Low dispersal") %>% 
  # filter(., t <= 150) %>%
  group_by(.,Fire_scenario) %>% 
  tally() %>% 
  ungroup()


fire_low_dispersal_table <- extirpation_150_lowDispersal_fire %>% 
  dplyr::select(., Fire_scenario, n) %>% 
  dplyr::rename(., Extirpations = n) %>% 
  mutate(., Surviving = reps - Extirpations)# %>% 
# mutate(., Odds = Extirpations/Surviving) %>% 
# dplyr::select(., -n, -Extirpations, - Surviving)  %>% 
# mutate(., DecreasingHighNo  = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal No Restoration"], nrow(test))) %>% 
# mutate(., DecreasingHighYes = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal Restoration"], nrow(test))) %>% 
# mutate(., )

fire <- as.character(fire_low_dispersal_table$Fire_scenario)
outcomes <- c("Etirpation", "Surviving")

(dat <- matrix(c(as.numeric(fire_low_dispersal_table$Extirpations), as.numeric(fire_low_dispersal_table$Surviving)), byrow = F, nrow = nrow(fire_low_dispersal_table)))
dimnames(dat) <- list("Fire" = fire, "Outcome" = outcomes)

(fire_low_dispersal_ORs <- fisher.test(dat))



fire_low_dispersal_ORs <- data.frame(OR = fire_low_dispersal_ORs$estimate, 
                                     LL = fire_low_dispersal_ORs$conf.int[1], 
                                     UL = fire_low_dispersal_ORs$conf.int[2]) %>% 
  mutate(., Comparison = "Decreasing/Historic") %>% 
  mutate(., FireReturnInterval = "All") %>% 
  mutate(., DispersalProbability = "Low")


# #---------------------------------------|---------------------------------------
# #                           Current Decreaseing dispersal
# #---------------------------------------|---------------------------------------
# 
# extirpation_150_decreasing_dispersal <- extinct %>% 
#   filter(., Period == "Current" & Fire_scenario == "Decreasing fire return interval") %>% 
#   filter(., t <= 150) %>% 
#   group_by(.,Dispersal_probability) %>% 
#   tally() %>% 
#   ungroup()
# 
# 
# dispersal_decreasing <- extirpation_150_decreasing_dispersal %>% 
#   dplyr::select(., Dispersal_probability, n) %>% 
#   dplyr::rename(., Extirpations = n) %>% 
#   mutate(., Surviving = reps - Extirpations)# %>% 
# # mutate(., Odds = Extirpations/Surviving) %>% 
# # dplyr::select(., -n, -Extirpations, - Surviving)  %>% 
# # mutate(., DecreasingHighNo  = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal No Restoration"], nrow(test))) %>% 
# # mutate(., DecreasingHighYes = rep(test$Odds[test$Treatment == "Decreasing fire return interval High dispersal Restoration"], nrow(test))) %>% 
# # mutate(., )
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
#   mutate(., Comparison = "High/Low") %>% 
#   mutate(., FireReturnInterval = "Decreasing") %>% 
#   mutate(., DispersalProbability = "All")

#---------------------------------------|---------------------------------------
#                                 Create OR Frame
#---------------------------------------|---------------------------------------

ORPattern <- grep("_OR",names(.GlobalEnv),value=TRUE)
ORs <- bind_rows(do.call("list",mget(ORPattern)))

ORs <- ORs %>% 
  mutate(., OddsRatio = ifelse(OR <1, 1/OR, OR)) %>% 
  mutate(., LL_ = ifelse(OR <1, 1/UL, LL)) %>% 
  mutate(., UL_ = ifelse(OR <1, 1/LL, UL))%>% 
  mutate(., Comparison = ifelse(OR < 1, paste0(Comparison, "*"), Comparison)) %>% 
  dplyr::select(., Comparison, FireReturnInterval, DispersalProbability, OddsRatio, LL_, UL_) %>% 
  rename(., UL = UL_, LL = LL_)



OR_table <- ORs %>%
  mutate(., OR = paste0(round(OddsRatio, 3), "(", round(LL,3), ", ",round(UL,3),")" )) %>% 
  select(., -LL, -UL, -OddsRatio) %>% 
  arrange(., FireReturnInterval, DispersalProbability, Comparison)




write.csv(OR_table, file = paste0(proj_dir, export_table_dir, "OR table 150.csv"))

# tiff(filename = paste0(proj_dir, figures_dir, "Final? OR by category fire dispersal restoration.tiff"),
#      width = 12,
#      height = 8.5,
#      units = "in",
#      res = 300,
#      compression = "lzw")

ggplot(data = ORs, aes(x = Comparison, y = OddsRatio))+
  geom_hline(yintercept = 1, col = "black", lty = 2) +
  geom_linerange(aes(ymin = LL, ymax = UL), lwd = 1.5,
                 position = pd) +
  geom_point(aes(x = Comparison,
                      y = OddsRatio),
                      size = 4,
                      stroke = 2,
                  # colour=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'),
                  position = pd ,
                  shape=21, 
                  fill= "white") +
                 # scale_color_manual(values = c( "#e41a1c","#377eb8","#4daf4a" ))+
    theme_bw() +
  facet_grid(~ Comparison)+
      theme(strip.background = element_rect(colour = "black", fill = "white"),
            strip.text       = element_text(colour = "black",  size = strip_text_size)) +
    labs(x = "Time Horizon",
         y = "Probability of Extirpation") +
    theme(legend.position = "none") +
    labs(x = "",
         y = "Odds Ratio") +
    theme(axis.title = element_text(size = axis_label_text_size)) +
    theme(axis.text  = element_text(size = axis_text_size)) +
    theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines")) # top, right, bottom, left

# dev.off()


################################################################################
#---------------------------------------|---------------------------------------
#                           Logistic regression
#---------------------------------------|---------------------------------------
################################################################################

extirpation_lr <- extinct %>% 
  mutate(., Extirpated = 1) %>% 
  select(., Iteration, t, Fire_scenario, Dispersal_probability, Period, Extirpated) %>% 
  complete(.,Iteration = unique(extinct$Iteration),
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
  filter(., t == 200 & !Fire_scenario == "Decreasing fire return interval"|
           t == 300 & !Fire_scenario == "Decreasing fire return interval"|
           t == 400 & !Fire_scenario == "Decreasing fire return interval"|
           t == 500 & !Fire_scenario == "Decreasing fire return interval") %>% 
  group_by(., t, Fire_scenario, Dispersal_probability) %>% 
  summarise_at(., vars(Density, SAP_Density, MA_Density, SAP_MA_Density), funs(q1, median, q3))

ggplot(TSF_suppression, aes(x = as.character(t), y = SAP_MA_Density_median, col = Fire_scenario))+
  geom_linerange(data = TSF_suppression, aes(x = as.character(t), ymin = SAP_MA_Density_q1, ymax = SAP_MA_Density_q3),
                 position = pd)+
  geom_point(data = TSF_suppression, aes(x = as.character(t), y = SAP_MA_Density_median), 
             position = pd)+
  facet_grid(~Dispersal_probability)

################################################################################
#---------------------------------------|---------------------------------------
#                           Density at year 500
#---------------------------------------|---------------------------------------
################################################################################
not <- extinct %>% 
  mutate(., Unique = IterationTime, Fire_scenario, Dispersal_probability)

year500densityTable <- current_models %>% 
  mutate(., Unique = IterationTime, Fire_scenario, Dispersal_probability) %>% 
  # filter(., !(Unique %in% not$Unique)) %>%
  filter(., t == 500) %>%
  group_by(., Fire_scenario, Dispersal_probability) %>%
  summarise_at(., vars(Density), funs(min, lower, q1, median, mean, q3, upper, max)) %>%
  ungroup()

year500density <- current_models %>% 
  mutate(., Panel = ifelse(Fire_scenario == "Decreasing fire return interval", "One", "Two")) %>% 
  mutate(., Unique = IterationTime, Fire_scenario, Dispersal_probability) %>% 
  # filter(., !(Unique %in% not$Unique)) %>%
  filter(., t == 500)


tiff(filename = paste0(proj_dir, figures_dir, "Final Metapopulation density 500 years.tiff"),
     width = 12,
     height = 8.5,
     units = "in",
     res = 300,
     compression = "lzw")

year500density %>%
   filter(., Density < 0.003) %>%  #& Fire_scenario != "Decreasing fire return interval") %>%
  # filter(., Fire_scenario != "Decreasing fire return interval") %>% 
  dplyr::mutate(., Fire_scenario = ifelse(Fire_scenario=='Decreasing fire return interval', 'Decreasing',
                                   ifelse(Fire_scenario=='Historic fire return interval', 'Historical',
                                   ifelse(Fire_scenario=='Suppression', 'Suppression', NA)))) %>%
  ggplot( aes(x = Density, fill = Fire_scenario)) +
  # geom_linerange( aes(x = Dispersal_probability, ymin = min, ymax = max, col = Dispersal_probability),
  #                position = position_dodge(width = 0.4), lwd = 1.5) +
  # scale_colour_manual(values = c("#ef8a62", "#67a9cf")) +
  # geom_point(aes(x = Dispersal_probability, y = median, col = Dispersal_probability),
  #            position = position_dodge(width = 0.4), stroke = 2, pch = 21, fill = "white", size = 3)+
  # geom_histogram(aes(fill = Dispersal_probability), alpha = 0.5, binwidth = .001)+
  geom_density(aes(fill = Fire_scenario), alpha = 0.4)+
  scale_fill_manual(values =  c("#d73027", "#1a9850", "#fee08b")) +
  theme_bw() +
  facet_grid(Panel~Dispersal_probability, scales = "free") +
  labs(x = expression(paste("\nTree density (no. trees/", m^2,") at year 500")), y = "Probability density")+
  theme(axis.text = element_text(size = axis_text_size - 2)) +
  theme(axis.title = element_text(size = axis_label_text_size))+
  theme(strip.text =  element_text(size = strip_text_size),
        # strip.text.y = element_blank(),
        strip.background = element_rect(fill = "white"))+
  # theme(axis.title.x = element_blank()) +
  # theme(axis.text.x = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin = grid::unit(c(2.5, 1, 1, 1), "lines"))+  # top, right, bottom, left
  facet_grid(Fire_scenario~Dispersal_probability, scales = "free_y")

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
