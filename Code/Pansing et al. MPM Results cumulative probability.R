################################################################################
#---------------------------------------|---------------------------------------
#                       Cumulative probability of extirpation
#---------------------------------------|---------------------------------------
################################################################################


Cum_extirpation_current <- extinct_current %>% 
  dplyr::mutate(., Iteration = as.numeric(Iteration)) %>% 
  dplyr::mutate(., Extirpation = 1) %>% 
  dplyr::select(., Iteration, t, Fire_scenario, Dispersal_probability, Extirpation) %>% 
  tidyr::complete(.,Iteration           = 1:reps, 
                  t                     = 1:projection_time,
                  Fire_scenario         = unique(extinct_current$Fire_scenario), 
                  Dispersal_probability = unique(extinct_current$Dispersal_probability),
                  fill                  = list(Extirpation = 0)) %>% 
  dplyr::group_by(., Fire_scenario, Dispersal_probability, t) 



  dplyr::summarise_at(., dplyr::vars(Extirpation), dplyr::funs(sum)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(., Fire_scenario, Dispersal_probability) %>% 
  dplyr::mutate(., CumSum = cumsum(Extirpation)) %>% 
  dplyr::mutate(., PExt = CumSum/reps) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Period = "Current")

Cum_extirpation_historical <- extinct_historic %>% 
  dplyr::mutate(., Iteration = as.numeric(Iteration)) %>% 
  # dplyr::filter(., Period == "Historical") %>%
  dplyr::mutate(., Extirpation = 1) %>% 
  dplyr::select(., Iteration, t, Fire_scenario, Dispersal_probability, Extirpation) %>% 
  tidyr::complete(.,Iteration           = 1:reps, 
                  t                     = 1:projection_time,
                  Fire_scenario         = unique(extinct_historic$Fire_scenario), 
                  Dispersal_probability = unique(extinct_historic$Dispersal_probability),
                  fill                  = list(Extirpation = 0)) %>% 
  dplyr::group_by(., Fire_scenario, Dispersal_probability, t) %>% 
  dplyr::summarise_at(., dplyr::vars(Extirpation), dplyr::funs(sum)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(., Fire_scenario, Dispersal_probability) %>% 
  dplyr::mutate(., CumSum = cumsum(Extirpation)) %>% 
  dplyr::mutate(., PExt = CumSum/reps) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Period = "Historical")


Cum_extirpation <- bind_rows(Cum_extirpation_current, Cum_extirpation_historical)


Cum_extirpation %>% dplyr::filter(., t == projection_time)

# tiff(filename = paste0(proj_dir, figures_dir, "Cumulative extirpation revision.tiff"),
#      width = 12,
#      height = 8.5,
#      units = "in",
#      res = 300,
#      compression = "lzw")

Cum_extirpation %>% 
  ggplot2::ggplot(data = ., ggplot2::aes(x = t, y = PExt, col = Fire_scenario))+
  ggplot2::geom_line(data = Cum_extirpation, ggplot2::aes(x = t, y = PExt, lty = Dispersal_probability), lwd = 1.5)+
  # scale_colour_manual(
  #   "CI horizontal line", values = c("#d73027", "#1a9850"),
  #   guide=guide_legend(override.aes = list(colour=c("#d73027", "#1a9850"))),
  #   labels=c("Decreasing fire return interval", "Historical fire return interval")
  # ) +
  ggplot2::scale_colour_manual(values = c("#fc8d62", "#8da0cb", "#66c2a5")) +
  # facet_grid(~ Dispersal_probability) +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "\nTime (years)",
       y = "Cumulative extirpation probability \n") +
  # ggplot2::scale_y_continuous(breaks = seq(0, 0.5, by = 0.1))+
  ggplot2::theme(strip.background = ggplot2::element_rect(colour = "black", fill = "white"),
                 strip.text       = ggplot2::element_text(colour = "black",  size = strip_text_size)) +
  # theme(legend.position = "none") +
  ggplot2::theme(legend.position = "top", legend.box = "vertical")+
  ggplot2::guides(linetype    = ggplot2::guide_legend(keywidth = 3, keyheight = 1)) +
  ggplot2::theme(legend.text  = ggplot2::element_text(size = axis_text_size))+
  ggplot2::theme(legend.title = ggplot2::element_blank())+
  ggplot2::theme(axis.title   = ggplot2::element_text(size = axis_label_text_size - 1)) +
  ggplot2::theme(axis.text    = ggplot2::element_text(size = axis_text_size)) +
  # ggplot2::scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.2)) +
  ggplot2::facet_grid(~Period, scales = "free_y")+
  ggplot2::theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines"))# top, right, bottom, left
 

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
  dplyr::mutate(., Iteration = as.numeric(Iteration)) %>% 
  dplyr::filter(., Period == "Current") %>% 
  dplyr::mutate(., Extirpation = 1) %>% 
  dplyr::select(., Iteration, t, Fire_scenario, Dispersal_probability, Extirpation) %>% 
  tidyr::complete(.,Iteration             = 1:5000, 
           t                     = 1:500,
           Fire_scenario         = unique(extinct$Fire_scenario), 
           Dispersal_probability = unique(extinct$Dispersal_probability),
           fill                  = list(Extirpation = 0)) %>% 
  dplyr::group_by(., Fire_scenario, Dispersal_probability, t) %>% 
  dplyr::summarise_at(., vars(Extirpation), funs(sum)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(., Fire_scenario, Dispersal_probability) %>% 
  dplyr::mutate(., CumSum = cumsum(Extirpation)) %>% 
  dplyr::mutate(., PExt = CumSum/5000) %>% 
  dplyr::ungroup()
  

Cum_extirpation_historical %>% dplyr::filter(., t == 500)




################################################################################
#---------------------------------------|---------------------------------------
#                       30 years post fire density < 250
#---------------------------------------|---------------------------------------
################################################################################
  
pop1 <- current_models %>% 
  select(., Iteration, t, Fire_scenario, Dispersal_probability, PopSize1, tSinceFire1) %>% 
  mutate(., Pop = "one") %>% 
  rename(., PopSize = PopSize1,
            tSinceFire = tSinceFire1)

pop2 <-  current_models %>% 
  select(., Iteration, t, Fire_scenario, Dispersal_probability, PopSize2, tSinceFire2) %>% 
  mutate(., Pop = "two") %>% 
  rename(., PopSize = PopSize2,
         tSinceFire = tSinceFire2)
  


test <- bind_rows(pop1, pop2) %>% 
  filter(., tSinceFire == 30)

total <- test %>% 
  group_by(Pop,Fire_scenario, Dispersal_probability) %>% 
             tally() %>% 
             ungroup()


test1 <- test %>% 
  filter(., PopSize < 10000) %>% 
  group_by(., Pop, Fire_scenario, Dispersal_probability) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(., Total = total$n) %>% 
  mutate(., Prop = n/Total)
  
  


ggplot(test, aes(x = PopSize, fill = Pop))  +
  geom_density()+
  geom_vline(xintercept = 10000)+
  facet_grid(Dispersal_probability~Fire_scenario)
  
