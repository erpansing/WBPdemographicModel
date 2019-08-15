# rm(list = ls())  # only use when playing with this script, and be sure to comment out before saving as it can ruin dependent scripts

library(dplyr)
library(RMark)
library(ggplot2)
library(reshape2)

load("/Users/elizabethpansing/Box/PhD/Code/WBP Demographic Model Master/WBPdemographicModel/Data/pial_survival_data.Rds")


  # 1. A model of constant survival rate (SR)
  
wbp.results <-  RMark::mark(pial_SD, nocc = 28, model="Nest",
             model.parameters = list(S = list(formula = ~1)))


# wbp.results       

SD_survival_mean <- wbp.results$results$real$estimate
SD_survival_var  <- nrow(pial_SD) * (wbp.results$results$real$se)^2


# predictions <- data.frame(time  = seq(from = 1991, to = 2017, by = 1),
#                           prob = wbp.results_10$Dot$results$real$estimate,
#                           lcl  = wbp.results_10$Dot$results$real$lcl,
#                           ucl  = wbp.results_10$Dot$results$real$ucl)
# #
# #
# ggplot(predictions, aes(x = time, y = prob)) + 
#   # geom_point() + 
#   geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.5, fill = "#ef8a62") +
#   geom_line(size = 0.5) +
#   xlab("Year") +
#   ylab("Annual survival rate") +
#   scale_y_continuous(limits = c(0.75,1)) +
#   scale_x_continuous(limits = c(1990, 2017),
#                      breaks = seq(1990, 2017, 2)) +
#   theme(axis.title = element_text(size = 18, face = "bold")) +
#   theme(axis.text = element_text(size = 15)) 
# 
# 
# 

# rm(list = ls()[!ls() %in% c("SD_survival_mean", "SD_survival_var")])

del <- paste0("wbp|pial|^del$")

rm(list = ls(pattern = del))

# Clean system files
markFiles <- dir(path = proj_dir, pattern = ".inp$|.vcv$|.out$|.tmp$")
file.remove(markFiles)
   
      