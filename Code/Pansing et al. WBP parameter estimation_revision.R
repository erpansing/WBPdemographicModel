# rm(list = ls())

proj_dir <- '/Users/elizabethpansing/Box/PhD/Code/WBP Demographic Model Master/WBPdemographicModel/'
data_dir <- "Data/"
code_dir <- "Code/"
load(paste0(proj_dir, data_dir, "survival.Rds"))

library(fitdistrplus)
library(tidyverse)


##-----------------------------------------------------------##
##                                                           ##
##                 Define Functions                          ##
##             and script-specific vars                      ##
##                                                           ##
##-----------------------------------------------------------##


lower <- function(x){quantile(x, (0.025))} 

upper <- function(x){quantile(x, (0.975))}

estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

get_betas <- function(data, reps, size){
  output_vector <- NULL
  for(i in 1:reps){
    sample  <- sample(data, size = size, replace = T)
    output_vector[i] <- mean(sample)
  }
  
  mean  <- mean(output_vector)
  var  <- var(output_vector)
  lower <- lower(output_vector)
  upper <- upper(output_vector)
  
  betaparams <- estBetaParams(mu  = mean, 
                              var = var)
  alpha      <- betaparams$alpha
  beta       <- betaparams$beta
  
  
  result <- list(samp.dist  = output_vector, mean.samp.dist = mean,
                 var.samp.dist = var, quantiles = c(lower, upper), alpha = alpha, beta = beta)
  return(result)
}


get_gammas <- function(mu, var){
  alpha <- mu^2/var
  theta <- var/mu
  beta <- 1/theta
  
  result <- list(alpha = alpha, 
                 theta = theta,
                 beta = beta)
  return(result)
}


get_negativeBinomial <- function(mu, var){
  lambda <- mu
  
}


reps <- 10000
n <- nrow(survival)

##-----------------------------------------------------------##
##                                                           ##
##              First Year Seed Probabilities                ##
##                                                           ##
##-----------------------------------------------------------##

# 3 possible outcomes for a first year seed Pansing et al. 2017
# 1) Seed germinated 

SEED1_germ <- survival$germ.2013.y.n

# 2) Seed survives and becomes 2nd year seed
#    Bc cach is the sampling unit, germintaion in Year 1 means no survival

survival$intact.2013.y.n <- ifelse(SEED1_germ == 1, 0, survival$intact.2013.y.n) #intact = intact seeds in cache remaining

SEED1_survive <- survival$intact.2013.y.n


# 3) Seed is pilfered, which we assume = death. 
#    OR
#    Seed dies (e.g., mold, inviable embryo, etc.)
# Here, we assume that all seeds that were not pilfered have the 
# ability to germinate in year 2. In other words, seed death in
# year 1 can ONLY result from cache pilferage

SEED1_die <- ifelse(SEED1_germ == 1 | SEED1_survive == 1, 0 ,1)

sum(mean(SEED1_germ), mean(SEED1_survive), mean(SEED1_die))

## ___________________________________________________________##
##                                                            ##
##         Create dataframe for SEED1 Germination             ##
##                                                            ##
## ___________________________________________________________##

SEED_germ_ERP <- survival %>% 
  group_by(., study.area) %>% 
  summarise_at(., vars(germ.2013.y.n), lst(mean = mean))



SEED1_germ_many <- unlist(c(as.vector(SEED_germ_ERP[1,2]), as.vector(SEED_germ_ERP[2,2]),   #From Pansing et al. 2017,
                     0.05, 0.22,                                                            #Schwandt et al. 2011 (Table 1- 2006 Control (no cage) & Table 2- 2007 Control (no cage))
                     0.115,                                                                 #McCaughey 1993 (Table 2.. used 11.5 not 11 bc McCaughey 1992 says 11.5)
                     0.34, 0.337,                                                           #McCaughey 1990 in McCaughey 1992 (First Year Germination section)
                      # 0.095, 0.193, 
                     0.20, 0.22, 0.035, 0.070))                                              # Schwandt et al. 2011 (Table 2, Control column)
                     # 0.278, 0.20, 0.352, 0.587, 0.319))  Not including DeMastus bc it looks like its germination over 3 years.                                    


SEED1_germ_params <- estBetaParams(mu = mean(SEED1_germ_many), var = var(SEED1_germ_many))


SEED1_germ_alpha <- SEED1_germ_params$alpha
SEED1_germ_beta <- SEED1_germ_params$beta


## Get sampling dist from beta parameters and plot

beta.dist.germ <- rbeta(reps, shape1 = SEED1_germ_alpha, 
                   shape2 = SEED1_germ_beta)
# hist(beta.dist.germ, xlab = "Proportion germinated")
# abline(v = mean(SEED1_germ_many), col = "blue", lwd = 1.5)
# abline(v = median(SEED1_germ_many), col = "red", lwd = 1.5)
# abline(v = c(lower(beta.dist.germ),
#              upper(beta.dist.germ)), col = "forestgreen", lwd = 1.5)


rm(SEED_germ_ERP,SEED1_germ, SEED1_germ_many, SEED1_germ_params, beta.dist.germ)

## ___________________________________________________________##
##                                                            ##
##     Create sampling distribution for SEED1 Survival        ##
##                                                            ##
## ___________________________________________________________##


SEED1_survival_params <- get_betas(data = SEED1_survive, size = n,
                                   reps = reps)

SEED1_survive_alpha      <- SEED1_survival_params$alpha
SEED1_survive_beta       <- SEED1_survival_params$beta


# hist(SEED1_survival_params$samp.dist,
#      xlab = "Proportion Germinated", main = "")
# abline(v = mean(SEED1_survive), col = "blue", lwd = 1.5)
# abline(v = c(SEED1_survival_params$quantiles[1],
#              SEED1_survival_params$quantiles[2]), col = "forestgreen", lwd = 1.5)

## Get sampling dist from beta parameters and plot


# germ.beta.dist <- rbeta(reps, shape1 = SEED1_survival_params$alpha, 
#                    shape2 = SEED1_survival_params$beta)

# hist(beta.dist, xlab = "Proportion Germinated", main = "")
# abline(v = c(SEED1_survival_params$lower.samp.dist, 
#              SEED1_survival_params$upper.samp.dist), col = "blue", lwd = 1)


rm(SEED1_survive, SEED1_die, SEED1_survival_params)

##-----------------------------------------------------------##
##                                                           ##
##              Second Year Seed Probabilities               ##
##                                                           ##
##-----------------------------------------------------------##

# Assuming 2 possibilities for 2nd year seeds:
# 1) Germination
# 2) Death (i.e., no germination). 
#
# We are assuming that if seeds do not germinate in year 1, they 
# will not. 

SEED2 <- survival %>%
  filter(., intact.2013.y.n == 1)   # Collect only individuals with intact seeds in 2013. (Not considering caches that had no seeds left or all had germinated)

sum(is.na(SEED2$germ.2014.y.n))
sum(is.na(SEED2$germ.2014.y.n) & is.na(SEED2$no.germ.2014)) 

#Remove 23 missing caches

SEED2 <- SEED2 %>% 
  filter(., !is.na(germ.2014.y.n))


SEED2_germ <- SEED2 %>% 
  group_by(., study.area) %>% 
  summarise_at(., vars(germ.2014.y.n), lst(mean = mean))   #get mean germination rate for both study areas

SEED2_germ_many <- c(unlist(SEED2_germ[1,2], SEED2_germ[2,2]),  # Pansing et al. 2017
                     0.45,                                    # McCaughey 1993 (Table 2)
                     0.08, 0.26 )                             # #Schwandt et al. 2011 (Table 1- 2007 Control (no cage) & Table 2- 2008 Control (no cage))


## ___________________________________________________________##
##                                                            ##
##     Create sampling distribution for SEED2 Germination     ##
##                                                            ##
## ___________________________________________________________##

SEED2_germ_params <- estBetaParams(mu = mean(SEED2_germ_many), var = var(SEED2_germ_many))

SEED2_germ_alpha      <- SEED2_germ_params$alpha
SEED2_germ_beta       <- SEED2_germ_params$beta


## Get sampling dist from beta parameters and plot


beta.dist <- rbeta(reps, shape1 = SEED2_germ_alpha, 
                   shape2 = SEED2_germ_beta)

# hist(beta.dist, xlab = "Proportion Germinated", main = "")
# abline(v = mean(SEED2_germ_many), col = "blue", lwd = 1.5)
# abline(v = median(SEED2_germ_many), col = "red", lwd = 1.5)
# abline(v = c(SEED2_germ_params$quantiles[1],
#              SEED2_germ_params$quantiles[2]), col = "forestgreen", lwd = 1.5)


rm(SEED2_germ_many, SEED2, SEED2_germ, SEED2_germ_params)

##-----------------------------------------------------------##
##                                                           ##
##                CS Survival Probabilities                  ##
##                                                           ##
##-----------------------------------------------------------##

CS <- survival %>%
   filter(., germ.2013.y.n == 1) 

table(CS$germ.2013.y.n, CS$no.living.2013)
# 23 individuals germinated but died prior to survey in 2013; we want to include these in our survival rates

# Include those seedlings that germinated and died before being located.
CS$survive.13.14[CS$no.living.2013 == 0] <- 0

# Remove those caches that we couldn't find in 2014
sum(is.na(CS$survive.13.14)) #5 caches

CS <- CS %>%
  filter(., !is.na(survive.13.14))
sum(is.na(CS$survive.13.14))


# Define CS_survive vector

CS_survive <- CS$survive.13.14

# Redefine n bc # trials changed

n <- length(CS_survive)

CS_survive_params <- get_betas(data = CS_survive, size = n, reps = reps)

CS_survive_alpha      <- CS_survive_params$alpha
CS_survive_beta       <- CS_survive_params$beta


## Get sampling dist from beta parameters and plot

beta.dist <- rbeta(reps, shape1 = CS_survive_params$alpha, 
                   shape2 = CS_survive_params$beta)

# hist(beta.dist, xlab = "Proportion Germinated", main = "")
# abline(v = mean(CS_survive), lwd = 1.5, col = "blue")
# abline(v = c(CS_survive_params$lower.samp.dist,
#              CS_survive_params$upper.samp.dist), col = "blue", lwd = 1)


rm(CS, CS_survive, CS_survive_params, beta.dist)

##-----------------------------------------------------------##
##                                                           ##
##             Seedling Survival Probabilities               ##
##                                                           ##
##-----------------------------------------------------------##

source(file = paste0(proj_dir,code_dir,"WBP Seedling Survival Estimates.R"))

SD_beta <- estBetaParams(mu = SD_survival_mean, var = SD_survival_var)

SD_survive_alpha <- SD_beta$alpha
SD_survive_beta  <- SD_beta$beta

quantile(rbeta(5000, shape1 = SD_survive_alpha, shape2 = SD_survive_beta))

rm(SD_beta)


##-----------------------------------------------------------##
##                                                           ##
##              Sapling Survival Probabilities               ##
##                                                           ##
##-----------------------------------------------------------##



SAP_survival <- 0.8


rm(survival, n, reps, estBetaParams, lower, upper)


##-----------------------------------------------------------##
##                                                           ##
##                        Fecundity                          ##
##                                                           ##
##-----------------------------------------------------------##

# Cone count estimates derived from IGBST cone count data from
# the GYE. https://www.usgs.gov/centers/norock/science/igbst-whitebark-pine-cone-production-annual-summaries?qt-science_center_objects=1#qt-science_center_objects



cones_1980_2016 <- c(25.69, 13.23, 16.98, 17.4, 6.43, 27.2, 1.37, 2.54, 2.40, 48.8,
                     1.54, 15.5, 15.38, 10.19, 2.03, 2.73, 25.05, 4.55, 8.4, 39.5,
                     5.7, 25.5, 2.4, 28.5, 6.9, 16.8, 34.4, 14.9, 8.6, 46.5, 
                     5.2, 19.8, 33, 5.2, 20.05, 15.89, 35.9)

mu.cones <- mean(cones_1980_2016)
var.cones <- var(cones_1980_2016)

# cone_params <- get_gammas(mu = mu.cones, var = var.cones)
# 
# cone_alpha <- cone_params$alpha
# cone_beta  <- cone_params$beta
# cone_theta <- cone_params$theta

# cone_frame <- data.frame(Year = 1980:2016, ConesPerTree = cones_1980_2016)


rm(cones_1980_2016, mu.cones, var.cones)

##-----------------------------------------------------------##
##                                                           ##
##                    Seeds per cone                         ##
##                                                           ##
##-----------------------------------------------------------##

mean_seedspercone <- 41.2
sd_seedspercone <- 23.4
var_seedspercone <- sd_seedspercone^2


mu_seeds_per_cone <- 41.2
size_seeds_per_cone <- (mean_seedspercone + mean_seedspercone^2)/var_seedspercone

##-----------------------------------------------------------##
##                                                           ##
##                    Seeds per cache                        ##
##                                                           ##
##-----------------------------------------------------------##

cache_sizes <- c(rep(1, 59), rep(2, 39), rep(3, 39), rep(4, 16), 
                 rep(5, 12), rep(6,7), rep(7,7), rep(8, 6), 
                 9, rep(10,2), rep(11,3), 12, 13, 14, 15)

lambda_cache_size <- fitdistr(cache_sizes, "Poisson")$estimate

##-----------------------------------------------------------##
##                                                           ##
##                      Fire Params                          ##
##                       Historical                          ##
##-----------------------------------------------------------##

FireIntervals <- c(250, 350, 100, 340, 300) # Larson et al. 2009, Barrett 1994, Arno 1980

(mean_interval <- mean(FireIntervals))
(var_interval <- var(FireIntervals))


fire_gamma <- get_gammas(mu = mean_interval, var = var_interval)

fire_alpha <- fire_gamma$alpha
fire_beta <- fire_gamma$beta

rm(FireIntervals, fire_gamma)


##-----------------------------------------------------------##
##                                                           ##
##                      Fire Params                          ##
##                       Suppression                         ##
##-----------------------------------------------------------##

mean_interval_suppression <- 1000
(var_interval_suppression <- 750^2)


fire_suppression_gamma <- get_gammas(mu = mean_interval_suppression, var = var_interval_suppression)

fire_suppression_alpha <- fire_suppression_gamma$alpha
fire_suppression_beta  <- fire_suppression_gamma$beta

rm(fire_suppression_gamma)



##-----------------------------------------------------------##
##              Whitebark pine density from                  ##
##                   pre 1988 YNP fires                      ##
##                 Tomback et al. in prep                    ##
##-----------------------------------------------------------##


HM_dens <- c(0.001, 0.003, 0, 0, 0.009, 0.003, 0.001)
Mean_HM_dens <- mean(HM_dens)
var_HM_dens <- var(HM_dens)

MW_dens <- c(0.016, 0, 0.001, 0.001, 0.010)
Mean_MW_dens <- mean(MW_dens)
var_MW_dens <- var(MW_dens)

dens_shape <- get_gammas(mu = Mean_HM_dens, var = var_HM_dens)$alpha
dens_rate <- get_gammas(mu = Mean_HM_dens, var = var_HM_dens)$beta

del <- paste0("^MW$|^HM$|del")
rm(list = ls(pattern = del))


##-----------------------------------------------------------##
##                 Mature tree survival rates                ##
##                 NPS WBP Monitoring Program                ##
##-----------------------------------------------------------##

# data available at https://irma.nps.gov/DataStore/Collection/Profile/4061

wbp_monitoring <- read.csv("/Users/elizabethpansing/Box/References/WBP Monitoring/Data GreaterYellowstoneWhitebarkPineMonitoringDataSummary2004thru2016 .csv")
col_names <- read.csv("/Users/elizabethpansing/Box/References/WBP Monitoring/Colnames GreaterYellowstoneWhitebarkPineMonitoringDataSummary2004thru2016 .csv")


cols <- as.character(col_names$Field.Name)

colnames(wbp_monitoring) <- cols


out <- wbp_monitoring %>% 
  dplyr::arrange(., PlotID, SurveyYear) %>% 
  dplyr::mutate(., PreviousLiving = lag(TotalLiveTreesThisSurvey)) %>% 
  dplyr::group_by(., PlotID, SurveyYear) %>% 
  dplyr::mutate(., PreviousLiving = ifelse(YearSiteEstablished == SurveyYear, NA, PreviousLiving)) %>% 
  dplyr::ungroup(.) %>% 
  dplyr::select(., PlotID, SurveyYear, PreviousLiving, TotalLiveTreesThisSurvey, DeadTrees_fromLivePrecedingSurvey,
                RecentlyDeadTrees_fromLivePrecedingSurvey) %>% 
  dplyr::mutate(., TotalDead = DeadTrees_fromLivePrecedingSurvey + RecentlyDeadTrees_fromLivePrecedingSurvey) %>% 
  dplyr::mutate(., Prop = TotalDead/PreviousLiving) %>% 
  dplyr::mutate(., SurvProb = 1- Prop)


mean_MA_s <- mean(out$SurvProb, na.rm = T)
var_MA_s  <- var(out$SurvProb, na.rm = T)

MA_s_alpha_decline <- (mean_MA_s^2 - mean_MA_s^3 - mean_MA_s* var_MA_s)/var_MA_s
MA_s_beta_decline  <- (mean_MA_s - 2 * mean_MA_s^2 + mean_MA_s^3 - var_MA_s + mean_MA_s* var_MA_s)/var_MA_s


rm(out,col_names, cols, mean_MA_s, var_MA_s )


# mean_MA_s_restoration <- 1-0.05
# var_MA_s_restoration  <- (1-0.05)^2

MA_s_alpha_historic <- 10
MA_s_beta_historic  <- 0.6

# out <- rbeta(5000, MA_s_alpha_historic, MA_s_beta_historic)
# hist(out)


