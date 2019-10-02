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
projection_time <- 500
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
