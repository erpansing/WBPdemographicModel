################################################################################
#                                                                              #
#                     WBP Demographic Model Methods Figures                    #
#                                                                              #
# AUTHOR: Elizabeth Pansing                                                    #
# DATE:   October 25, 2018                                                     #
# DESCRIPTION: .... The following code creates the figures used to describe 
#                   methods used in the Pansing et al. MPM. This includes
#                   reduction factors
################################################################################
rm(list = ls())

options(stringsAsFactors = FALSE)

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

proj_dir <- paste0('/Users/elizabethpansing/Box Sync/PhD/Code/WBP Demographic Model Master/Dis_WBP_MODEL_ACTIVE/')
rda_dir <- "Rda/"
code_dir <- "Code/"
model_file <- "WBP Demographic Model Metapopulation.R"
export_results_dir <-  "Exported Results Tables/"

################################################################################
#---------------------------------------|---------------------------------------
#                             Set graphing parameters
#---------------------------------------|---------------------------------------
################################################################################

axis_label_text_size <- 23
strip_text_size <- 20
axis_text_size <- 18


################################################################################
#---------------------------------------|---------------------------------------
#                         Create reduction factor data frame 
#---------------------------------------|---------------------------------------
################################################################################

x <- seq(0, 6, by = 0.01)

rals_sd_plot   <- lapply(x, function(x){0.8/(1 + exp(4   * (x-2.5))) + 0.2})
rals_germ_plot <- lapply(x, function(x){0.33333333 + (1/(1+0.7^(-(x-1))))})
rals_cone_plot <- lapply(x, function(x){  0.5/(1 + exp(5 *(x-2.25)))})

data <- data.frame(CC = rep(x,3),
                   Cones = unlist(rals_cone_plot),
                   Germination = unlist(rals_germ_plot),
                   SeedlingSurvival = unlist(rals_sd_plot))

data <- gather(data = data,Stage,Reduction, -CC)

head(data)

tiff(filename = paste0(proj_dir, "Figures/Density Dependent Effects Figure Revision.tiff"),
     width = 12,
     height = 8.5,
     units = "in",
     res = 300,
     compression = "lzw")

(reduction_plot <- ggplot(data = data, aes(x = CC, y = Reduction))+
    geom_line(lwd = 1.5)+
    theme_bw() +
    # scale_color_manual(values = c("#ef8a62", "#67a9cf"))+
    facet_wrap(~ Stage, scale = "free_x") + # free_x removes extra y axis tick labels.... go figure
    theme(strip.background = element_rect(colour = "black", fill = "white"), 
          strip.text = element_text(colour = "black",  size = strip_text_size)) +
    scale_x_continuous(breaks = c(0, 2, 4, 6))+
    labs(x = "\nLeaf area index", y = "Proportion of total\n")+
    theme(axis.title = element_text(size = axis_label_text_size))+
    theme(axis.text = element_text(size = axis_text_size))+
    theme(legend.position = "none")+

    geom_hline(yintercept = 0, col = "black", lty = 2) +
    theme(plot.margin = grid::unit(c(2.5, 1, 1, 1), "lines"))) # top, right, bottom, left

dev.off()
