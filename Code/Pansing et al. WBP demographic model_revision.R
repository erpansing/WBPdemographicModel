##-----------------------------------------------------------##
##                                                           ##
##                 WBP Demographic Model                     ##
##                    Libby Pansing                          ##
##-----------------------------------------------------------##

library(tidyverse)

## Import relevant parameter estimates calculated in other scripts.

## Required files:
## 1) GRIN parameter estimates.R
## 2) GRIN WBP Survival Estimates RMark Nest.R
## 3) 2017 YNP Data.Rda
## 4) survival.Rda
## 5) MTBS RdNBR in WBP GYE habitat.R

proj_dir <- '/Users/elizabethpansing/Box/PhD/Code/WBP Demographic Model Master/WBPdemographicModel/'
data_dir <- "Data/"
rda_dir  <- "Rda/"
code_dir <- "Code/"

# Source scripts that estimates relevant demographic parameters
source(paste0(proj_dir, code_dir,"Pansing et al. WBP parameter estimation_revision.R"))
# Import data that estimates fire severity of wildfires that burned in WBP habitat (derived from MTBS data)
load(paste0("/Users/elizabethpansing/Box/PhD/Code/WBP Demographic Model Master/Dis_WBP_MODEL_ACTIVE/",
            data_dir, "rdnbr_dat.Rds"))
# Import sapling and mature densities derived from FIA data in WBP habitat in Wyoming
load( file = paste0(proj_dir, data_dir, "fia density.Rda"))
# Source script that estimates seedling and seedling densities derived from Tomback et al. post fire data
source(file = paste0(proj_dir, code_dir, "WBP seedling density estimates.R"))

noNAdat_rdnbr <- na.omit(rdnbr_dat)


n <- c(300, 90, 100, 300, 700,
       500, 50, 500, 120, 600)

area <- 2000

## Write function for determining leaf area index (LAI) as a function
## of stage specific dbh and the number of trees on the landscape

##***********************************************************##
##                        Define LAI                         ##
##***********************************************************##

#
# ## Leaf area coefficients. Define the relationship between leaf area and diameter.
# ## Estimated via MLE assuming the general form y = ax^b
#

alpha1 <- 0.456

alpha2 <- 0.0736

alpha3 <- 2.070

LAI_data <- data.frame(DBH = c(0, 0, 2.05, 12.5, 37),
                       LA = c(0, alpha1, alpha2*2.05^alpha3, alpha2*12.5^alpha3, alpha2*37^alpha3))

# ggplot(data = LAI_data, aes(x = DBH, y = LA))+
#   geom_line()+
#   geom_point()


l <- # function(){
  matrix(c(#d1(),      # SEED1 do not contribute to LAI
           0, # d2(),      # SEED2 do not contribute to LAI
           0, #d3(),      # CS do not contribute to LAI
           0.456, #alpha1(),  # SD1 don't have DBH but do contribute to LAI
           3.762789, #alpha2() * d5() ^ alpha3(),
           85.21032, #alpha2() * d6() ^ alpha3(),
           #d1(),      # SEED1 do not contribute to LAI
           0, #d2(),      # SEED2 do not contribute to LAI
           0, #d3(),      # CS do not contribute to LAI
           0.456, #alpha1(),  # SD1 don't have DBH but do contribute to LAI
           3.762789, #alpha2() * d5() ^ alpha3(),
           85.21032),  nrow = 5) #alpha2() * d6() ^ alpha3()),
#}

##############################################################################################
##
##                                    Background LAI
##
##############################################################################################

# Pre-fire densities from plots in Tomback et al. 2001

Others <- data.frame(PICO = c( rep(0, 7),                 54, 29, 33,   1, 59),
                     PIEN = c( 6,  5, 20,  9, 10, 43, 10,  3, 12, 43, 112,  2),
                     ABLA = c(78, 81, 46, 34, 55, 33, 21, 12,  9,  4,   6, 13),
                     UNK  = c( 6,  4, 14,  4,  0,  1,  1, 25, 32, 27,   6, 43),
                     StudyArea = c(rep("Henderson", 7), rep("Washburn", 5)))


Others <- Others %>%
  dplyr::mutate(n = rowSums(dplyr::select(., -StudyArea))) %>%
  dplyr::mutate(Density = n/(30*30)) %>%  # per plot overall density for all non-whitebark conifers
  dplyr::mutate(., PlotNo = 1:nrow(.)) %>%
  dplyr::mutate(., Year = 1988 + 230) %>%
  dplyr::select(., Year, StudyArea, PlotNo, n, Density) %>%
  dplyr::mutate(., n = round(Density * 20, digits = 0)) %>%
  dplyr::mutate(., DBH_class = "11")

area <- 2e3 *10000

(mean_prefire_dens <- mean(Others$Density))
(mean_no_conifers_5000 <- mean_prefire_dens * area/2)


## Create logistic curve for largest size class
# x <- rep(0:150, each = 100)

# LA_3 <- lapply(x, function(x){rpois(n = 1, lambda = ((mean_prefire_dens * 1.25)/(1 +110*exp(-0.1*x))) * (area/2))}) # Better
# plot(x, LA_3, col  = "forestgreen", type = "p", ylim = c(0, max(unlist(LA_3))),
#                 xlab = "Time since fire (years)", ylab = "Number of individuals", xlim = c(0,200), pch = 19)

# LA_4 <- lapply(x, function(x){rpois(n = 1, lambda = (mean_no_conifers_5000*1.1)/(1+110*exp(-0.07*x)) * (area/2))})
# points(x, LA_4, col= "blue", pch = 19)

# LA_5 <- lapply(x, function(x){rpois(n = 1, lambda = ((mean_no_conifers_5000)/(1+110*exp(-0.06*x))))})
# points(x, LA_5, type = "l", xlab = "Time since Fire", ylab = "Number of individuals", pch = 19)
# abline(h = mean_prefire_dens*((area/2)), lty = 2, lwd = 3)


## Functions

LA_3 <- function(x){
  rpois(n = 1, lambda = ((mean_no_conifers_5000 * 1.25)/(1 +110*exp(-0.1*x))))
}


LA_4 <- function(x){
  rpois(n = 1, lambda = (mean_no_conifers_5000*1.1)/(1+110*exp(-0.07*x)))
}

LA_5 <- function(x){
  rpois(n = 1, lambda = ((mean_no_conifers_5000)/(1+110*exp(-0.06*x))))
}

rm(Others)



LAIb    <- function(tSinceFire){     # Background leaf area index. This is where competition can be incorporated...
  if(tSinceFire[1] == 0 & tSinceFire[2] != 0){
    c(0, matrix(c(alpha1, alpha2* 2.25^alpha3, alpha2*8.25^alpha3), ncol = 3) %*% matrix(c(LA_3(tSinceFire[2]),LA_4(tSinceFire[2]), LA_5(tSinceFire[2])), nrow = 3, byrow = F))
  }else if(tSinceFire[1] != 0 & tSinceFire[2] == 0){
    c(matrix(c(alpha1, alpha2* 2.25^alpha3, alpha2*8.25^alpha3), ncol = 3) %*% matrix(c(LA_3(tSinceFire[1]),LA_4(tSinceFire[1]), LA_5(tSinceFire[1])), nrow = 3, byrow = F), 0)
  }else if(tSinceFire[1] == 0 & tSinceFire[2] == 0){
    c(0, 0)
  }else if(tSinceFire[1] != 0 & tSinceFire[2] != 0){
    out1 <- matrix(c(alpha1, alpha2* 2.25^alpha3, alpha2*8.25^alpha3), ncol = 3) %*% matrix(c(LA_3(tSinceFire[1]),LA_4(tSinceFire[1]), LA_5(tSinceFire[1])), nrow = 3, byrow = F)
    out2 <- matrix(c(alpha1, alpha2* 2.25^alpha3, alpha2*8.25^alpha3), ncol = 3) %*% matrix(c(LA_3(tSinceFire[2]),LA_4(tSinceFire[2]), LA_5(tSinceFire[2])), nrow = 3, byrow = F)

    return(matrix(cbind(out1,out2), nrow = 2))
  }
}

LAI <- function(x, tSinceFire) {       # LAI of the study area
  c(((l[,1] %*% x[1:5]) + LAIb(tSinceFire = tSinceFire)[1])/(area/2),
    ((l[,2] %*% x[6:10])+ LAIb(tSinceFire = tSinceFire)[2])/(area/2))
}

n <- c(62, 580, 38, 79, 65+ 953, 62, 580, 38, 79, 65+ 953)
# x <- matrix(rep(1:500, 2), nrow = 500, byrow = F)
# out <- lapply(x, function(x){c((l[,1] %*% c(62, 580, 38, 79, 65+ 953) + LAIb(tSinceFire = x)[1])/(area/2),
#                                (l[,2] %*% c( 62, 580, 38, 79, 65+ 953)+ LAIb(tSinceFire = x)[2])/(area/2))})

# LAI(n, tSinceFire = c(150,500))
## Now define the distributions from which survival and
## transition rates will be drawn to create stochastic demographic rates

##-----------------------------------------------------------##
##              Empirically derived survival                 ##
##              & transition distributions                   ##
##-----------------------------------------------------------##


##-----------------------------------------------------------##
##                          SEED1                            ##
##-----------------------------------------------------------##

# s_SEED1 <- 0
# # Assume seeds either transition to SEED2,
# # transition to CS (first year seedling)
# # or die


# t1_SEED1    <- function(size = 1){       # survival probability of seeds (i.e., survive and transition to SEED2, but do not germinate)
#   rbeta(n = size,                        # Drawn from a beta to give a
#         shape1 = SEED1_survive_alpha,    # probability of seeds transitioning
#         shape2 = SEED1_survive_beta)     # to SEED2 stage
# }



# t2_SEED1 <- function(size = 1){       # Germination probability of seeds
#   rbeta(n = size,                     # Drawn from a beta to give a
#         shape1 = SEED1_germ_alpha,    # probability of seeds transitioning
#         shape2 = SEED1_germ_beta)     # to SEED2 stage
# }

# ##-----------------------------------------------------------##
# ##                            CS                             ##
# ##                     First Year Seedling                   ##
# ##-----------------------------------------------------------##

# s_CS <- 0                          # Assume seeds transition to
                                     # SD or die

t_CS     <- function(size = 1){      # Survival probability of first
  rbeta(n = size,                    # year seedlings (cotyledon seedlings)
        shape1 = CS_survive_alpha,   # Drawn from a beta to give prob
        shape2 = CS_survive_beta)    # of transitioning to SD stage
}


##-----------------------------------------------------------##
##                          SD                               ##
##-----------------------------------------------------------##

s_SD     <- function(size = 1){     # survival probability of seedlings
  rbeta(n = size,                   # Drawn from a beta to give prob
        shape1 = SD_survive_alpha,  # surviving any given year
        shape2 = SD_survive_beta)
}

##-----------------------------------------------------------##
##                           SAP                             ##
##-----------------------------------------------------------##

s_SAP     <- function(){                  # Survival probability of saplings
  runif(n = size, min = 0.8, max = 0.973) # Data from Rochefort et al. 2018
  # return(0.8)
}

##-----------------------------------------------------------##
##                            MA                             ##
##-----------------------------------------------------------##

s_MA     <- function(size = 1, MA_s_alpha, MA_s_beta){ # Survival rate of reproductively mature adults
  rbeta(n = size,              # Assume limited death from senescence because
        shape1 = MA_s_alpha,   # of long lived nature of wbp (lifespan up to 1200 yrs)
        shape2 = MA_s_beta)    # Taken from GYE monitoring data
}

##-----------------------------------------------------------##
##                          DEFINE                           ##
##                         SURVIVAL                          ##
##                          VECTOR                           ##
##-----------------------------------------------------------##

survival_vector <- function(size = 1, MA_s_alpha, MA_s_beta){   #survival vector
  c(
    rbeta(n = size,                   # Drawn from a beta to give prob
          shape1 = SD_survive_alpha,  # of seedling survival in any given year
          shape2 = SD_survive_beta),
    runif(n   = size, 
          min = 0.8, max = 0.973),
    rbeta(n = size,                   # Assume limited death from senescence because
          shape1 = MA_s_alpha_historic,        # of long lived nature of wbp (lifespan up to 1200 yrs)
          shape2 = MA_s_beta_historic) )       # But mortality comes from MBP, WPBR, and senescence
}

# survival_vector()

##-----------------------------------------------------------##
##                          DEFINE                           ##
##                       RESIDENCE TIME                      ##
##                          VECTOR                           ##
##-----------------------------------------------------------##

residence_vector <-
  c(28,
    20,
    Inf)

  # # years as seedling (SD)

  # # years as sapling (SAP)

 # # years as reproductively mature

residence_vector


si <- function(size = 1, MA_s_alpha_historic, MA_s_beta_historic){                  # Gives probability of surviving and staying in the
  (1 - (1/residence_vector)) *         # same life stage for those life stages
    survival_vector(size = size,
                    MA_s_alpha = MA_s_alpha,
                    MA_s_beta = MA_s_beta)       # that have residence time > 1 (i.e., persist in the
}                                      # same life stage for > 1 year)

ti <- function(size = 1, MA_s_alpha_historic, MA_s_beta_historic) {                 # Gives probability of surviving and transitioning
  (1/residence_vector) *               # to the next life stage for those life stages
    survival_vector(size = size,
                    MA_s_alpha = MA_s_alpha,
                    MA_s_beta = MA_s_beta)       # that have residence time > 1 (i.e., persist in the
}


##-----------------------------------------------------------##
##                         FERTILITY                         ##
##-----------------------------------------------------------##

No_seeds_per_cone <- 45

No_cones <- function(t, size = 1){ # Seed production in wbp is periodic
  result <- NULL                   # with masting years every ~ 4 years
  # so define cone production as a function
  for(i in 1:size){                # of time described by cos with normally distributed error
    value <- (12.5*cos(1.5 * t) + 14 + rnorm(1, sd = 3.5))
    if( value >= 0){               # Max values and expected values from
      result[i] <- value           # IGBST cone monitoring since 1980
    } else if(value < 0){          #
      result[i] <- value - value   # # caches assumes 45 seeds/cone
    }                              # and 3 seeds/cache. All available seeds cachek
  }                                # Assumes 45% of caches created are left for regeneration.
  return(result)
}

##-----------------------------------------------------------##
##         Define variables assumed fixed & known            ##
##-----------------------------------------------------------##

Pfind  <- 0.55   # Proportion of seeds found by nutcrackers
Pcons  <- 0.3    # Proportion of seeds consumed by nutcracers (prior to caching)
SpC    <- 3      # No. seeds per cache

##-----------------------------------------------------------##
##               Define no. caches per pop                   ##
##-----------------------------------------------------------##


No_caches_1_nofire <- function(cones_1, cones_2, t, size = 1, x, dispersal_prob){
  (cones_1 * No_seeds_per_cone * x[5]  * (1-Pcons)* (1-Pfind)/SpC * (1-dispersal_prob)) +     ## Number of caches dispersed from pop 1 to pop1
  (cones_2 * No_seeds_per_cone * x[10] * (1-Pcons)* (1-Pfind)/SpC *  dispersal_prob)         ## Number of caches dispersed from pop 2 to pop1
}

No_caches_1_fire1 <- 0

No_caches_1_fire2 <- function(cones_1, t, size = 1, x){
  cones_1 * No_seeds_per_cone * x[5] * (1-Pcons)* (1-Pfind)/3 * 1    ## where 1 is the probability of dispersal from pop1 to pop1
}

No_caches_2_nofire <- function(cones_1, cones_2, t, size = 1, x, dispersal_prob){
  (cones_1 * No_seeds_per_cone * x[5]  * (1-Pcons)* (1-Pfind)/SpC * dispersal_prob) +
   (cones_2 * No_seeds_per_cone * x[10] * (1-Pcons)* (1-Pfind)/SpC * (1 - dispersal_prob))
}

No_caches_2_fire1 <- function(cones_2, t, size = 1, x){
  cones_2 * No_seeds_per_cone * x[10] * (1-Pcons)* (1-Pfind)/SpC *1 # where 1 is the probability of dispersal from pop2 to pop2
}

No_caches_2_fire2 <- 0


##-----------------------------------------------------------##
##                       Germination                         ##
##-----------------------------------------------------------##

##-----------------------------------------------------------##
##         Define variables dependent on time vars           ##
##-----------------------------------------------------------##

## Define reduction factors. These variables reduce
## 1) rALS decreases germination as light availability decreases
## 2) rCache increases caching propensity as seed availability increases


rALS_sd   <- function(x, tSinceFire){
  # 0.8/(1 + exp(4 * (LAI(x, tSinceFire) - 2.5))) + 0.2
  # 0.7/(1 + exp(6   * (LAI(x, tSinceFire) - 2.5)))  + 0.3
  # 0.75/(1 + exp(6 * (LAI(x, tSinceFire) - 2.5))) + 0.25 # Most recent run with counterintuitive results
  
  
  #Troubleshooting
  # 0.33333333 + (1/(1+0.7^(-(LAI(x, tSinceFire) -1)))) #OG Ecosphere function
  # 0.25 + (1/(1+0.7^(-(LAI(x, tSinceFire) -1)))) #Change to only ralsSD 0.25
  # 0.25 + (1/(1+0.7^(-(LAI(x, tSinceFire) -1)))) #Change both ralsSD & rals germ 0.25
  # 0.2 + (1/(1+0.7^(-(LAI(x, tSinceFire) -1)))) #Change to only ralsSD 0.2
  0.2 + (1/(1+0.7^(-(LAI(x, tSinceFire) -1)))) #Change both ralsSD and  0.2
}


rALS_germ     <- function(x, tSinceFire){
  # 0.33333333 + (1/(1+0.5^(-(LAI(x, tSinceFire = tSinceFire)-1)))) # OG Ecosphere function
   # 0.33333333 + (1/(1+0.5^(-(LAI(x, tSinceFire = tSinceFire)-1)))) # Only changed ralsSD, this stays the same as previous
  # 0.25 + (1/(1+0.5^(-(LAI(x, tSinceFire = tSinceFire)-1)))) #  changed ralsSD & ralsgerm to 0.25
  # 0.25 + (1/(1+0.5^(-(LAI(x, tSinceFire = tSinceFire)-1)))) # Only changed ralsSD, this stays the same as previous
   0.2 + (1/(1+0.5^(-(LAI(x, tSinceFire = tSinceFire)-1)))) #  changed ralsSD & ralsgerm to 0.25
}

rcones <- function(n, tSinceFire){
  0.5/(1 + exp(5 *(LAI(n, tSinceFire = tSinceFire)-2.25)))
}


##-----------------------------------------------------------##
##                      GERMINATION                          ##
##-----------------------------------------------------------##

## DEFINE UNIT VECTORS
e2_1 <- matrix(c(0,1,0,0,0,0,0,0,0,0))
e2_2 <- matrix(c(0,1,0,0,0,0,0,0,0,0))
e7_1 <- matrix(c(0,0,0,0,0,0,1,0,0,0))
e7_2 <- matrix(c(0,0,0,0,0,0,1,0,0,0))


## DEFINE FUNCTION FOR THE NUMBER OF GERMINANTS IN EACH POPULATION AS A RESULT OF DISPERSAL FROM THE OTHER POPULATION
## AND SEED DORMANCY
germ1stpop1 <- function(t, size = 1, x, caches1, tSinceFire){
  caches1 * as.vector(rALS_germ(x, tSinceFire = tSinceFire)[1]) * rbeta(n = 1, shape1 = SEED1_germ_alpha, shape2= SEED1_germ_beta) * e2_1
}

germ1stpop2 <- function(t, size = 1, x = x, caches2, tSinceFire){
  caches2 * as.vector(rALS_germ(x, tSinceFire = tSinceFire)[2]) * rbeta(n = 1, shape1 = SEED1_germ_alpha, shape2= SEED1_germ_beta) * e7_1
}

germ2ndpop1 <- function(t, size = 1, x, tSinceFire){
  n[1] * rALS_germ(x, tSinceFire = tSinceFire)[1] * rbeta(n = 1, shape1 = SEED2_germ_alpha, shape2 = SEED2_germ_beta) * e2_2
}

germ2ndpop2 <- function(t, size = 1, x, tSinceFire){
  n[6] * rALS_germ(x, tSinceFire = tSinceFire)[2] * rbeta(n = 1, shape1 = SEED2_germ_alpha, shape2 = SEED2_germ_beta) * e7_2
}

##-----------------------------------------------------------##
##                      SEED DORMANCY                        ##
##-----------------------------------------------------------##

## DEFINE UNIT VECTORS
e1_1 <- matrix(c(1,0,0,0,0,0,0,0,0,0))
e1_2 <- matrix(c(0,0,0,0,0,1,0,0,0,0))

## DEFINE FUNCTION THAT PROJECTS NUMBER OF DORMANT CACHES IN EACH POPULATION

dorm_1 <- function(t, size = 1, caches1, tSinceFire){
  caches1 * rbeta(n = 1, shape1 = SEED1_survive_alpha, shape2= SEED1_survive_beta) * e1_1
}

dorm_2 <- function(t, size = 1, caches2, tSinceFire){
  caches2 * rbeta(n = 1, shape1 = SEED1_survive_alpha, shape2= SEED1_survive_beta) * e1_2
}

##-----------------------------------------------------------##
##        PROBABILITY OF MORTALITY IN FIRE YEARS             ##
##-----------------------------------------------------------##

# Determine the proportion of individuals in a population who are killed.
# We assume that the mortality rates are uniformly distributed following the min and max values below,
# and that trees are killed in a manner proportional to areas impacted by each fire severity.
prop_surviving <- function(){
  prop_severity <- {noNAdat_rdnbr[sample(nrow(noNAdat_rdnbr), size = 1),2:4]} # Determine the distribution of fire severities affecting each population
    1-(prop_severity$high  *   runif(n = 1, min = 0.9, max = 0.95) +          # Determine the proportion of the population that is killed by high severity fire
    prop_severity$moderate *   runif(n = 1, min = 0.6, max = 0.89) +          # Determine the proportion of the population that is killed by moderate severity fire
    prop_severity$low      *   runif(n = 1, min = 0.2, max = 0.59))}          # Determing the proportion of the population that is killed by low severity fire

##-----------------------------------------------------------##
##                DEFINE PROJECTION MATRICES                 ##
##-----------------------------------------------------------##

# NO FIRE IN EITHER POPULATION.
S <- function(tSinceFire, x = n, s1 = s1, s2 = s2, t1= t1, t2 = t2){
  #     SEED2      CS                                                SD     SAP    MA           SEED2_2     CS_2                                              SD_2  SAP_2  MA_2
  matrix(c(0,       0,                                                0,     0,     0,               0,       0,                                                0,     0,    0,
           0,       0,                                                0,     0,     0,               0,       0,                                                0,     0,    0,
           0, t_CS(1), rALS_sd(x = n, tSinceFire = tSinceFire)[1]*s1[1],     0,     0,               0,       0,                                                0,     0,    0,
           0,       0, rALS_sd(x = n, tSinceFire = tSinceFire)[1]*t1[1], s1[2],     0,               0,       0,                                                0,     0,    0,
           0,       0,                                                0, t1[2], s1[3],               0,       0,                                                0,     0,    0,
         ##############################################################################         ##################################################################################
           0,       0,                                                0,     0,     0,               0,       0,                                                0,     0,    0,
           0,       0,                                                0,     0,     0,               0,       0,                                                0,     0,    0,
           0,       0,                                                0,     0,     0,               0, t_CS(1), rALS_sd(x = n, tSinceFire = tSinceFire)[2]*s2[1],     0,    0,
           0,       0,                                                0,     0,     0,               0,       0, rALS_sd(x = n, tSinceFire = tSinceFire)[2]*t2[1], s2[2],    0,
           0,       0,                                                0,     0,     0,               0,       0,                                                0, t2[2], s2[3]),
         byrow = T, nrow = 10)
}


# FIRE IN SUBPOPULATION 1
S_fire_1 <- function(tSinceFire, x = n, s1 = s1, s2 = s2, t2 = t2){
  #     SEED2  CS   SD  SAP                     MA              SEED2_2  CS_2                                                SD_2  SAP_2   MA_2
  matrix(c(0,   0,  0,  0,                      0,               0,       0,                                                0,     0,     0,
           0,   0,  0,  0,                      0,               0,       0,                                                0,     0,     0,
           0,   0,  0,  0,                      0,               0,       0,                                                0,     0,     0,
           0,   0,  0,  0,                      0,               0,       0,                                                0,     0,     0,
           0,   0,  0,  0, prop_surviving()*s1[3],               0,       0,                                                0,     0,     0,
           ########################################            ######################################################################################
           0,   0,  0,  0,                      0,               0,       0,                                                0,     0,     0,
           0,   0,  0,  0,                      0,               0,       0,                                                0,     0,     0,
           0,   0,  0,  0,                      0,               0, t_CS(1), rALS_sd(x = n, tSinceFire = tSinceFire)[2]*s2[2],     0,     0,
           0,   0,  0,  0,                      0,               0,       0, rALS_sd(x = n, tSinceFire = tSinceFire)[2]*t2[2], s2[2],     0,
           0,   0,  0,  0,                      0,               0,       0,                                                0, t2[2],  s2[3]),
         byrow = T, nrow = 10)
}


# FIRE IN SUBPOPULATION 2
S_fire_2 <- function(tSinceFire, x = n, s1 = s1, t1= t1, s2 = s2){
  #     SEED2      CS                                                SD     SAP    MA           SEED2_2  CS_2 SD_2 SAP_2                 MA_2
  matrix(c(0,       0,                                               0,      0,     0,             0,     0,   0,   0,                    0,
           0,       0,                                               0,      0,     0,             0,     0,   0,   0,                    0,
           0, t_CS(1), rALS_sd(x = n, tSinceFire = tSinceFire)[1]*s1[1],     0,     0,             0,     0,   0,   0,                    0,
           0,       0, rALS_sd(x = n, tSinceFire = tSinceFire)[1]*t1[1], s1[2],     0,             0,     0,   0,   0,                    0,
           0,       0,                                               0,  t1[2], s1[3],             0,     0,   0,   0,                    0,
           #############################################################################        ###############################################
           0,       0,                                               0,      0,     0,             0,     0,   0,   0,                    0,
           0,       0,                                               0,      0,     0,             0,     0,   0,   0,                    0,
           0,       0,                                               0,      0,     0,             0,     0,   0,   0,                    0,
           0,       0,                                               0,      0,     0,             0,     0,   0,   0,                    0,
           0,       0,                                               0,      0,     0,             0,     0,   0,   0,    prop_surviving()*s2[3]),
         byrow = T, nrow = 10)
}


# FIRE IN BOTH SUBPOPULATIONS
S_fire_both <- function(s1 = s1, s2 = s2){
  #      SEED2  CS  SD  SAP                  MA              SEED2_2  CS_2  SD_2 SAP_2                MA_2
  matrix(c(0,   0,  0,  0,                   0,                  0,     0,    0,    0,                  0,
           0,   0,  0,  0,                   0,                  0,     0,    0,    0,                  0,
           0,   0,  0,  0,                   0,                  0,     0,    0,    0,                  0,
           0,   0,  0,  0,                   0,                  0,     0,    0,    0,                  0,
           0,   0,  0,  0, prop_surviving()*s1[3],                  0,     0,    0,    0,                  0,
        ########################################               #############################################
           0,   0,  0,  0,                   0,                  0,     0,    0,    0,                  0,
           0,   0,  0,  0,                   0,                  0,     0,    0,    0,                  0,
           0,   0,  0,  0,                   0,                  0,     0,    0,    0,                  0,
           0,   0,  0,  0,                   0,                  0,     0,    0,    0,                  0,
           0,   0,  0,  0,                   0,                  0,     0,    0,    0, prop_surviving()*s2[3]),
         byrow = T, nrow = 10)
}


##-----------------------------------------------------------##
##                                                           ##
##              Fire return interval functions               ##
##                                                           ##
##-----------------------------------------------------------##
## Westerling et al. (2011) predict a decrease from historic fire
## return intervals in the GYE from >120 years to <30 by the end
## of the 21st century. The following functions describe that decrease
## in fire return interval.

## We define current fire return intervals as ~230 years in
## (Larson et al. 2009) in wbp forests. We set middle of the
## century values following figure 3 in Westerling et al. 2011
## and end of century values as 30 years.

## The following function uses those timeframes to estimate the shape
## of the declined in fire rates (i.e., lambda)

fire_return_decrease <- data.frame(Year = c(0,25,  50, 75, 100, 200, 300,  400, 500), Interval = c(mean_interval, 250, 225, 200, 180, 161, 140, 120,  97.000))
# fit <- lm(log(Interval)~Year, data = fire_return_decrease)
fit <- glm(Interval~Year + I(Year^2), data = fire_return_decrease, family = Gamma(link = "inverse"))
summary(fit)
predicted_fire_return_decrease <- data.frame(Year = c(fire_return_decrease$Year, 500))
# predicted <- exp(fit$coefficients[1] + fit$coefficients[2]* fire_return_decrease$Year)
predicted <- predict.glm(object = fit, newdata = predicted_fire_return_decrease, type = "response")
# plot(Interval~Year, data = fire_return_decrease, xlim = c(0,550), col = "blue", pch = 19)
# points(predicted_fire_return_decrease$Year, predicted, pch = 19)
# lines(predicted_fire_return_decrease$Year, predicted)


# ## Function that determines whether fire occurs in current year
# fire_current_year <- function(t, n = 1){
#   c(rbinom(size = 1, n = 1, prob = 1/interval(t)),rbinom(size = 1, n = 1, prob = 1/interval(t)))
# }


##-----------------------------------------------------------##
##              Function that projects pop                   ##
##              sizes and incorporates fire                  ##
##-----------------------------------------------------------##

# library(plyr)


# n <- c(300, 90, 100, 300, 99951,
#        500, 50, 500, 120, 2e5-9951)

# Params for troubleshooting
# projection_time <- 10
# reps <- 3
# FRI_decrease <- FALSE
# fire <- FALSE
# dispersal_distance <- "High"
# period <- "Current"
# j <- 1
# i <- 1

seeds <- No_cones(t = 1:500, size = 5000)
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################

project <- function(projection_time, reps = 100, FRI_decrease = T, fire = T, dispersal_distance = "High",
                    period = "Current"){       # stochastic projection function
  # that tracks stage based pop sizes
  # over time for reps number of iterations
  if(!(dispersal_distance == "High" | dispersal_distance == "Low"))
    stop("\nUnknown dispersal distance. \nPlease use 'High' or 'Low'")
  if(!(period == "Current" | period == "Historic"))
    stop("\nUnknown period. \nPlease use 'Current' or 'Historic'")


  results <-                                                 #Create null matrix that will hold stage
    array(0, dim = c(projection_time + 1, 10 + 1, reps)) # based population sizes and year tracker

  # Assign dispersal probability
  if(dispersal_distance == "High"){
    dispersal_prob <- 0.06
  }else if(dispersal_distance == "Low"){
    dispersal_prob <- 0.24
  }

  # Assign mature tree survival
  if(period == "Current"){
    MA_s_alpha <- MA_s_alpha_decline
    MA_s_beta  <- MA_s_beta_decline
  } else if(period == "Historic"){
    MA_s_alpha <- MA_s_alpha_historic
    MA_s_beta  <- MA_s_beta_historic
  }

  for(j in 1:reps){       # Iterate through i years (projection_time) of population growth j times (iterations)
    # Incorporate FIRE
    cat(paste0("......Iteration ", j,"......\n"))

    if(j == 1){

      # Define empty matrixes to hold relevant information for each iteration
      fire_tracker       <- matrix(c(rep(1:reps, each = projection_time), rep(0, (projection_time)*reps*3)),
                                   nrow = (projection_time ) * reps, ncol = 4, byrow = F)
      tSinceFire_tracker <- matrix(c(rep(1:reps, each = projection_time), rep(0, (projection_time)*reps*3)),
                                   nrow = (projection_time ) * reps, ncol = 4, byrow = F)
      LAI_tracker        <- matrix(c(rep(1:reps, each = projection_time), rep(0, (projection_time)*reps*3)),
                                   nrow = (projection_time ) * reps, ncol = 4, byrow = F)
      lambda             <- matrix(c(rep(1:reps, each = projection_time), rep(0, (projection_time)*reps*3)),
                                   nrow = (projection_time ) * reps, ncol = 4, byrow = F)




    }else if(j != 1){
      fire_tracker       <- fire_tracker
      tSinceFire_tracker <- tSinceFire_tracker
      LAI_tracker        <- LAI_tracker
      lambda             <- lambda
      pops <- pops
    }


    # Define starting population sizes. These are values sampled from empirically derived wbp densities from
    # FIA data (saplings and mature trees) and post fire data from Tomback et al. (first year seedlings and seedlings;
    # see Tomback et al. 2001 Ecology for methodology). Because there is, to our knowledge, no information about the
    # number of seeds available for germination in a given year, we use the same logic applied in the model run to
    # obtain the starting number of dormant seeds: we use the number of mature (cone-bearing) trees in the starting population
    # and estimated the number of dormant seed caches given a random number of seeds per tree (derived from a vector of cones
    # produced given the equation for number of cones per tree) and the defined dispersal probability.

    n0    <- matrix(unlist(c(0, cot_seedling_density[sample(1:nrow(cot_seedling_density), size = 1), 'cot_count'], fia_density[sample(1:nrow(fia_density), size = 1), c('seedling_count', 'sapling_count', 'mature_count')],
                             0, cot_seedling_density[sample(1:nrow(cot_seedling_density), size = 1), 'cot_count'], fia_density[sample(1:nrow(fia_density), size = 1), c('seedling_count', 'sapling_count', 'mature_count')])), ncol = 1)

    cone_1_start <- sample(seeds, size = 1)
    cone_2_start <- sample(seeds, size = 1)
    n0[c(1,6)] <- c(No_caches_1_nofire(cones_1 = cone_1_start, cones_2 = cone_2_start, size = 1, x = n0,  dispersal_prob = dispersal_prob),
                    No_caches_2_nofire(cones_1 = cone_1_start, cones_2 = cone_2_start, size = 1, x = n0, dispersal_prob = dispersal_prob))

    n          <- n0


    # define starting time since the last fire. This is a bit arbitrary as there isn't much information about
    # the density of wbp as a function of time since fire, so we assign a random tSinceFire.
    #TODO: Figure out how to better estimate tsince fire for starting population sizes
    tSinceFire <- c(round(runif(n = 1,min = 1, max = 280), 0), round(runif(n = 1, min = 1, max = 280), 0))

    # Initializes empty matrix to hold population sizes

    pops <- matrix(0, nrow = projection_time + 1, ncol = length(n)) # +1 gives space for the initial year
    pops[1,] <- n0

    ##########################
    # historic FRI, both pops
    #########################

    # Create matrix of fire years for each subpopulation
    if(fire == T & FRI_decrease == F){
      fire_years <-matrix(round(c(cumsum(rgamma(6, fire_alpha, fire_beta)), cumsum(rgamma(6, fire_alpha, fire_beta))), 0),
                          byrow = F, nrow = 6)
      fire_years <- ifelse(fire_years > 500, NA, fire_years)

    } else if(fire == F){
      fire_years <-matrix(round(c(cumsum(rgamma(6, fire_suppression_alpha, fire_suppression_beta)),
                                  cumsum(rgamma(6, fire_suppression_alpha, fire_suppression_beta))), 0),
                          byrow = F, nrow = 6)

    } else if(fire == TRUE & FRI_decrease == TRUE){
      
      get_gammas <- function(mu, var){
        alpha <- mu^2/var
        theta <- var/mu
        beta <- 1/theta
        
        result <- list(alpha = alpha, 
                       theta = theta,
                       beta = beta)
        return(result)
      }
      
      
      var <- 29.5 # Clark et al. 2017 Ecosphere

      
      
      ########################
      # Decreasing FRI, pop 1
      #######################
      
      fire_intervals1 <- NA
      first <- NA
      fire_years1 <- NA

      #keep track of where we are in the vector (same things as in a for loop but since we don't have a target index I'm using in a repeat/break combo)
      index <- 1

      repeat{
        #predict a fire interval based on the year of the last fire (or year 0 for the first value)

        if(index == 1){
          year <- data.frame(Year = tSinceFire[1])
          mu <- predict.glm(object = fit, newdata = year, type = "response")

          #reshape the gamma for random sampling values
          gammas <- get_gammas(mu = mu, var = ifelse(year > 100, 29.5, 10370))

          fire_intervals1[index] <- rgamma(1, gammas$alpha, gammas$beta)
          fire_years1 <- cumsum(fire_intervals1)
          year$Year <- fire_intervals1[index]
          index <- index + 1

        }else if(index !=1){
          mu <- predict.glm(object = fit, newdata = year, type = "response")

          #reshape the gamma for random sampling values
          gammas <- get_gammas(mu = mu, var = ifelse(year > 100, 29.5, 10370))
          
          fire_intervals1[index] <- rgamma(1, gammas$alpha, gammas$beta)
          #make a vector of the years fires occur
          fire_years1 <- cumsum(fire_intervals1)
          #set the year to the one we just sampled to calculate next interval
          year$Year <- fire_years1[index]
          #increment our index
          index <- index + 1
          #once we are over year 500 break the loop!
          if(max(fire_years1) > 500){
            break
          }
        }
      }
      #######################
      # Decreasing FRI, pop 2
      #######################
      fire_intervals2 <- NA
      year <- data.frame(Year = 0)
      fire_years2 <- NA

      #keep track of where we are in the vector (same things as in a for loop but since we don't have a target index I'm using in a repeat/break combo)
      index <- 1

      repeat{
        #predict a fire interval based on the year of the last fire (or year 0 for the first value)

        if(index == 1){
          year <- data.frame(Year = tSinceFire[2])
          mu <- predict.glm(object = fit, newdata = year, type = "response")

          #reshape the gamma for random sampling values
          gammas <- get_gammas(mu = mu, var = ifelse(year > 100, 29.5, 10370))
          
          fire_intervals2[index] <- rgamma(1, gammas$alpha, gammas$beta)
          fire_years2 <- cumsum(fire_intervals2)
          year$Year <- fire_intervals2[index]
          index <- index + 1

        }else if(index !=1){
          mu <- predict.glm(object = fit, newdata = year, type = "response")

          #reshape the gamma for random sampling values
          gammas <- get_gammas(mu = mu, var = ifelse(year > 100, 29.5, 10370))
          
          fire_intervals2[index] <- rgamma(1, gammas$alpha, gammas$beta)
          #make a vector of the years fires occur
          fire_years2 <- cumsum(fire_intervals2)
          #set the year to the one we just sampled to calculate next interval
          year$Year <- fire_years2[index]
          #increment our index
          index <- index + 1
          #once we are over year 500 break the loop!
          if(max(fire_years2) > 500){
            break
          }
        }
      }

      ##### Create Matix of Fire years
      if(length(fire_years1) != length(fire_years2)){
        out <- list(fire_years1, fire_years2)
        lengths <- lapply(out, length)
        out[[which.min(lengths)]] <- c(out[[which.min(lengths)]], rep(NA, times = length(out[[which.max(lengths)]])-length(out[[which.min(lengths)]])))
        fire_years <- matrix(round(c(out[[1]], out[[2]]), 0), ncol = 2, byrow = F)
      }else if(length(fire_years1) == length(fire_years2)){
        fire_years <- matrix(round(c(fire_years1, fire_years2), 0), ncol = 2, byrow = F)
      }
    }

    ####### TRACK FIRE YEARS ACROSS ITERATIONS
    if(j == 1){
      fire_tracker2 <- matrix(c(rep(j, nrow(fire_years)),fire_years), ncol = 3, byrow = F)
    } else if(j != 1){
      fire_tracker2 <- append(fire_tracker2 , matrix(c(rep(j, nrow(fire_years)),fire_years), ncol = 3, byrow = F))
    }


    for(i in 1:projection_time){
      #--------------------------------------------------------------------------------------------------------------------------------------------
      # cat(paste0("Year ", i,"\n"))

      ## Set up initials for beginning of each iteration
      # if (i == 1){
      #
      #
      #   LAI_tracker[1, 2:4] <- c(0, LAI(x = n, tSinceFire = tSinceFire))
      #
      #
      # }else
      if(i != 1){
        tSinceFire <- tSinceFire + 1
      }

      LAI_tracker[j * projection_time - (projection_time) + i, 2:4] <- c(i, LAI(x = n, tSinceFire = tSinceFire))
      tSinceFire_tracker[j * projection_time - (projection_time) + i, 2:4] <- c(i, tSinceFire)
      # Update time counter for each time step
      t <-  i    # time counter

      s1 <- si(MA_s_alpha = MA_s_alpha, MA_s_beta = MA_s_beta)
      s2 <- si(MA_s_alpha = MA_s_alpha, MA_s_beta = MA_s_beta)
      t1 <- ti(MA_s_alpha = MA_s_alpha, MA_s_beta = MA_s_beta)
      t2 <- ti(MA_s_alpha = MA_s_alpha, MA_s_beta = MA_s_beta)
      #

      #--------------------------------------------------------------------------------------------------------------------------------------------
      ##############################################################################################################################################
      ##                                                                FIRE
      ##############################################################################################################################################
      # if(fire == T){
      fire_current <- c(NA, NA)
      fire_current[1] <- ifelse(t %in% fire_years[,1], 1, 0)
      fire_current[2] <- ifelse(t %in% fire_years[,2], 1, 0)

      fire_tracker[j * projection_time - (projection_time) + i, 2:4] <- c(i, fire_current)

      ## 1) There's a fire. This kills some proportion of the population. Assumes stand replacing burn that impacts entire population
      ##    And that no regeneration occurs the year of the fire
      ## 2) There's no fire and it's >1 year after fire. In this case, there are no modifications and the
      ##    system proceeds as normal. If it's the year after fire, the only seed source is from the other subpopulation
      #--------------------------------------------------------------------------------------------------------------------------------------------
      ## Fire can occur in different combinations
      ## 1) Fire in pop1 but not pop2: fire_current = 0,1
      ## 2) Fire in pop2 but not pop1: fire_current = 1,0
      ## 3) Fire in both populations: fire_current = 1,1

      ## 1) FIRE IN 1 BUT NOT 2

      if(fire_current[1] == T & fire_current[2] == F){

        tSinceFire <- c(0, tSinceFire[2])

        tSinceFire_tracker[j*projection_time - (projection_time)+i,3:4] <- tSinceFire

        # Determine the proportion of the population that experiences different fire severities
        # Assume that a certain proportion of saplings and mature trees are killed, but all seedlings and seeds are killed


        # Most fires go out with first snow. e.g., Romme 1982

        mat      <- S_fire_1(tSinceFire, s1 = s1, x = n, s2 = s2, t2 = t2)

        x <- mat %*% n  # Calculate intermediate population size

        ## Update parameters drawn from distributions/samples that must remain constant during each year
        cones <- No_cones(t = t, size = 1) *rcones(n, tSinceFire = tSinceFire)

        caches1 <- No_caches_1_fire1
        caches2 <- No_caches_2_fire1(cones = cones[2], t = t, size = 1, x = x)


        pops[i+1,] <- c(t(x +
                          germ1stpop2(t = t, size = 1, x = n, caches2 = caches2, tSinceFire = tSinceFire) +
                          germ2ndpop2(t = t, size = 1, x = n, tSinceFire = tSinceFire) +
                          dorm_2(t = t, caches2 = caches2, tSinceFire = tSinceFire)))

        n         <- as.matrix(pops[i+1,], nrow = length(pops[i+1,]), ncol = 1)

      }

      ## 2) FIRE IN 2 BUT NOT 1

      if(fire_current[1] == F & fire_current[2] == T){

        tSinceFire <- c(tSinceFire[1], 0)

        tSinceFire_tracker[j*projection_time - (projection_time)+i,3:4] <- tSinceFire


        # Assuming stand replacing burn with no survival and no regeneration.
        # Most fires go out with first snow. e.g., Romme 1982
        mat      <- S_fire_2(tSinceFire, x = n, s1 = s1, t1= t1, s2 = s2)

        x <- mat %*% n

        cones <- No_cones(t = t, size = 1) *rcones(n, tSinceFire = tSinceFire)

        caches1 <- No_caches_1_fire2(cones_1 = cones[1], t = t, size = 1, x = n)
        caches2 <- No_caches_2_fire2


        pops[i+1,] <- c(t(x +
                          germ1stpop1(t = t, size = 1, x = n, caches1 = caches1, tSinceFire = tSinceFire) +
                          germ2ndpop1(t = t, size = 1, x = n, tSinceFire = tSinceFire)+
                          dorm_1(t = t, caches1 = caches1, tSinceFire = tSinceFire)))

        n         <- as.matrix(pops[i+1,], nrow = length(pops[i+1,]), ncol = 1)

      }

      ## 3) FIRE IN BOTH

      if(fire_current[1] == T & fire_current[2] == T){

        tSinceFire <- c(0, 0)
        tSinceFire_tracker[j*projection_time - (projection_time)+i,3:4] <- tSinceFire

        cat(paste0("***Fire in both populations in iteration ", j, " year ",t,"***\n"))

        pops[i+1,] <- c(t(S_fire_both(s1 = s1, s2 = s2) %*% n))  # Defines the intermediate population size

        n         <- as.matrix(pops[i+1,], nrow = length(pops[i+1,]), ncol = 1)

      }
      #--------------------------------------------------------------------------------------------------------------------------------------------
      #                                                 No fire in current year in either subpopulation
      #--------------------------------------------------------------------------------------------------------------------------------------------
      else if(fire_current[1] == F & fire_current[2] == F){

        mat <- S(tSinceFire = tSinceFire, x = n, s1 = s1, s2 = s2, t1 = t1, t2 = t2)

        x <- mat%*%n

        cones <- No_cones(t = t, size = 1) *rcones(n, tSinceFire = tSinceFire)

        caches1 <- No_caches_1_nofire(cones_1 = cones[1], cones_2 = cones[2],  t = t, size = 1, x = n, dispersal_prob = dispersal_prob)
        caches2 <- No_caches_2_nofire(cones_1 = cones[1], cones_2 = cones[2],  t = t, size = 1, x = n, dispersal_prob = dispersal_prob)


        pops[i+1,]  <- c(t(x +
                           germ1stpop1(t = t, size = 1, x = n, caches1 = caches1, tSinceFire = tSinceFire) +
                           germ1stpop2(t = t, size = 1, x = n, caches2 = caches2, tSinceFire = tSinceFire) +
                           germ2ndpop1(t = t, size = 1, x = n, tSinceFire = tSinceFire) +
                           germ2ndpop2(t = t, size = 1, x = n, tSinceFire = tSinceFire) +
                           dorm_1(t = t, caches1 = caches1, tSinceFire = tSinceFire) +
                           dorm_2(t = t, caches2 = caches2, tSinceFire = tSinceFire)))

        n <- as.matrix(pops[i+1,], nrow = length(pops[i+1,]), ncol = 1)

      }
      ##############################################################################################################################################
      if(i == 1){
        lambda[j*projection_time - (projection_time) + i, 2:3] <- c(i, NA)
      }else if(i != 1){
        lambda[j*projection_time - (projection_time) + i, 2:3] <- c(i, sum(pops[i,])/sum(pops[i-1,]))
      }
    }  # END i LOOP

    pops <- cbind(pops, rep(0:projection_time))  # Appends matrix to keep track of time during iteration
    results[, ,j] <- pops                        # Combines iterations into a j dimensional array

  }

  pop_sizes <- plyr::adply(results, 3)           # Changes array to dataframe so easier to manipulate later
  colnames(pop_sizes) <- c("Iteration", "SEED2_1", "CS_1", "SD_1", "SAP_1", "MA_1", "SEED2_2", "CS_2", "SD_2", "SAP_2", "MA_2", "t")


  results <- list(pop_sizes = pop_sizes, fire_tracker = fire_tracker, LAI_track = LAI_tracker, lambda = lambda, tSinceFire_tracker = tSinceFire_tracker)

  return(results)
}
