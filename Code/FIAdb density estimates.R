rm(list = ls())

library(RPostgreSQL)
library(tidyverse)
library(rgdal)
library(raster)

options(scipen = 999)

################################################################################
#---------------------------------------|---------------------------------------
#                             Establish directories
#---------------------------------------|---------------------------------------
################################################################################

proj_dir <- '/Users/elizabethpansing/Box/PhD/Code/WBP Demographic Model Master/WBPdemographicModel/'
data_dir <- 'Data/'
rda_dir <- "Rda/"
code_dir <- "Code/"
model_file <- "Pansing et al. MPM Model Final? Gamma fire.R"
figures_dir <- "Figures/Final?/"
export_table_dir <-  "Exported Data Tables/"
geospatial_dir <- "Geospatial Data/"

################################################################################
#---------------------------------------|---------------------------------------
#                             Read in data from FIADB
#---------------------------------------|---------------------------------------
################################################################################

##_________________________________________
# Get data from FIADB. Need data from tree and plot databases, so get relevant columns and merge
# by plt_cn (tree) and cn (plot)

system(paste0("pg_ctl -D /usr/local/var/postgres start"))


fia_db_conn <- DBI::dbConnect("PostgreSQL", dbname = 'FIADB', host = 'localhost', port = '5432')

treeDat <- DBI::dbGetQuery(conn = fia_db_conn, statement = "SELECT * FROM wy_tree WHERE spcd = '101'") 
names(treeDat) <- paste0("tree.", names(treeDat))

# Unique key is plot_cn, subp,tree. Therefore subplot unique is plot_cn, subp, and plot unique is plt_cn.

subplotDat <- DBI::dbGetQuery(conn = fia_db_conn, statement = "SELECT * FROM wy_subplot")
names(subplotDat) <- paste0("subp.", names(subplotDat))

plotDat <- DBI::dbGetQuery(conn = fia_db_conn, statement = "SELECT cn, manual_db, manual_rmrs, lat, lon, elev, designcd FROM wy_plot")
names(plotDat) <- paste0("plot.", names(plotDat))

seedlingDat <- DBI::dbGetQuery(conn = fia_db_conn, 
                              statement = paste0("SELECT plt_cn, plot, subp,condid, spcd, invyr, treecount, treecount_calc, tpa_unadj, countchkcd_rmrs ",
                                                 "FROM WY_seedling ",
                                                 "WHERE spcd = '101'")) %>% 
  na.omit()
names(seedlingDat) <- paste0("seedling.", names(seedlingDat))


DBI::dbDisconnect(fia_db_conn)

system(paste0("pg_ctl -D /usr/local/var/postgres stop"))


# This is comprised of all whitebark pine trees monitored since 2000.
# Sampling unit is the tree

dat <- merge(treeDat, plotDat, by.x = "tree.plt_cn", by.y = "plot.cn", all.x = TRUE) %>% 
  dplyr::select(., tree.invyr,tree.plt_cn, tree.plot, tree.subp, tree.countycd, tree.dia, tree.tpa_unadj, plot.lat, plot.lon)
  

wbp_plot <- plotDat %>% 
  dplyr::filter(., plot.cn %in% dat$tree.plt_cn)

wbp_subp <- subplotDat %>% 
  dplyr::filter(.,subp.plt_cn %in% dat$tree.plt_cn)



# # Problem. Multiple counties per plot. 
# test <- dat %>% 
#   group_by(., tree.plt_cn, tree.plot) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   group_by(tree.plot) %>% 
#   tally() %>% 
#   ungroup()
# 
# # Look at those with multiple counties to see whether they are actually differnt locations...
# 
# test1 <- dat %>% 
#   filter(., tree.plot == 40 | tree.plot == 61 | tree.plot == 136 | tree.plot == 218) %>% 
#   mutate(., county_plot = paste(tree.countycd, tree.plot))
# 
# library(ggmap)
# 
# map <- ggmap::get_map(location = c(lon =  -109.79905 , lat = 43.69521),
#                       maptype = "terrain", zoom = 8)
# 
# ggmap(map)+
#   geom_point(data = test1, aes(x = plot.lon, y = plot.lat, col = as.factor(county_plot)), size = 4)
# 
# 
# test2 <- test1 %>% 
#   filter(., tree.plot == 40)
# 
# map <- ggmap::get_map(location = c(lon =  mean(test2$plot.lon) , lat = mean(test2$plot.lat)),
#                       maptype = "terrain", zoom = 8)
# 
# ggmap(map)+
#   geom_point(data = test2, aes(x = plot.lon, y = plot.lat, col = as.factor(county_plot)), size = 4)
#   
# test3 <- test2 %>% 
#   filter(., tree.countycd == 35)
# 
# map <- ggmap::get_map(location = c(lon =  mean(test3$plot.lon) , lat = mean(test3$plot.lat)),
#                       maptype = "terrain", zoom = 15)
# 
# ggmap(map)+
#   geom_point(data = test3, aes(x = plot.lon, y = plot.lat, col = as.factor(tree.subp)), size = 1)
# 
# # Long way to figure out that plot numbering starts over in each county. 
# 
# a <- dat %>% 
#   mutate(., unique = paste(tree.countycd, tree.plt_cn, tree.plot))
#   
# length(unique(a$unique))
# 
# test <- dat %>% 
#   group_by(., tree.countycd, tree.plt_cn, tree.plot) %>% 
#   sample_n(., size = 1) %>% 
#   ungroup() %>% 
#   mutate(., unique = paste(tree.countycd, tree.plt_cn, tree.plot))
# 
# length(unique(test$unique))
# 
# dat <- dat <- merge(treeDat, plotDat, by.x = "tree.plt_cn", by.y = "plot.cn", all.x = TRUE) %>% 
#   mutate(., unique = paste(tree.countycd, tree.plt_cn, tree.plot))
# 
# length(unique(dat$unique))


###################################################################################################
#                        Map plot locations & select only those in known wbp range
###################################################################################################
# This may or may not be a useful exercise because the locations of FIA plots are fuzzed and swapped.
# However, it may be worthwhile if the densities in outside of whitebark pine range areas are substantially 
# lower than densities in known areas. If not, we'll continue with all plots that contain wbp.
# 
# 1) Import new (2019) wbp range raster. (https://whitebarkfound.org/wp-content/uploads/2019/05/WBP-April-2019.gdb_.zip)
# 2) Constrain to wbp range within Wyoming to match the spatial extent of FIA data.

# Import wbp range map
rgdal::GDALinfo("/Users/elizabethpansing/Box/References/WBP Range Maps/WBP_USdist_Apr2019/WBP_USdist_Apr20191.tif")
wbp_range <- raster::raster("/Users/elizabethpansing/Box/References/WBP Range Maps/WBP_USdist_Apr2019/WBP_USdist_Apr20191.tif")
(wbp_crs <- raster::crs(wbp_range))
# plot(wbp_range)

# Import wyoming shapefile
wyo <- rgdal::readOGR(dsn = paste0("/Users/elizabethpansing/Box/PhD/Tibbs Surivival/", data_dir,geospatial_dir,"tl_2016_56_cousub"),
                      "tl_2016_56_cousub") 
# plot(wyo)
raster::crs(wyo)
class(wyo)

# Merge counties so only the outline of the state remains
lps <- sp::coordinates(wyo)
#
IDOneBin <- raster::cut(lps[,1], range(lps[,1]), include.lowest=TRUE)
wyo   <- maptools::unionSpatialPolygons(wyo ,IDOneBin)

# plot(wyo)

wyo <- sp::spTransform(wyo, CRS = raster::crs(wbp_range))## Transform the gye bc the wbp range is a raster layer, which is more difficult to change
identical(raster::crs(wyo), raster::crs(wbp_range))

## Select only the portion of whitebark pine's range that is within wyoming. 
# 1) crop the range shp to be the same as wyo
# 2) mask the portions of the range outside of the wyo border

wbp_wyo <- raster::crop(wbp_range, wyo)
wbp_wyo <- raster::mask(wbp_wyo, wyo)

# plot(wyo)
# plot(wbp_wyo, add = T)


# map FIA plot "locations" and wbp range in wyo

coords <- plotDat %>% 
  dplyr::select(., plot.lon, plot.lat)
sp_dat <- plotDat %>% 
  dplyr::select(., -plot.lon, -plot.lat)

plots <- sp::SpatialPointsDataFrame(coords = coords, data = sp_dat, proj4string =  CRS("+init=epsg:4269"))
plots <- sp::spTransform(plots, CRS = raster::crs(wbp_range))
identical(raster::crs(wbp_wyo), raster::crs(plots))

# plot(wbp_wyo)
# plot(wyo, add = T)
# points(plots)

ones <- which(raster::extract(wbp_wyo == 1, plots) == 1)
head(ones)
plots1 <- plots[ones,]

# plot(wbp_wyo)
# plot(wyo, add = T)
# points(plots1)

plots_keep <- as.data.frame(plots1)

dat_restricted <- dat %>% 
  dplyr::filter(., tree.plt_cn %in% plots_keep$tree.plt_cn)

left_over <- dat %>% 
  dplyr::filter(., !(tree.plt_cn %in% plots_keep$tree.plt_cn))

###################################################################################################
#                                    Estimate densities
###################################################################################################
# Estimate plot densities during each survey. There are two different designs for different sized
# trees. Trees with DBH >= 5cm were measured in the entire subplot. Trees with DBH <5 cm were only
# measured in nested microplots. 
#
# I assume these are reasonable representations of sapling and mature classes present in the 
# model.

# First, because there the exact location of the plots is not reported (see above), we make sure that 
# the distributions of wbp densities within wbp's mapped distribution are not different than
# those outside of the mapped distribution. If they do not differ, we will use data from plots reported
# to be both within and outside of wbp distribution. 


## Subplots (trees >= 5 cm DBH)
dat_restricted %>% 
  group_by(., tree.invyr, tree.plt_cn, tree.subp, tree.tree) %>% 
  tally() %>% 
  filter(., n > 1)

dat_restricted %>% 
  group_by(., tree.invyr, tree.plt_cn, tree.subp) %>% 
  tally() %>% 
  filter(., n > 1)

## Can't use tpa_unadj bc we're segregating by sapling/mature, so need to 
## estimate density directly. 

mature_density <- dat_restricted %>% 
  dplyr::filter(., tree.dia >= 5) %>% 
  dplyr::group_by(tree.invyr, tree.plt_cn, tree.subp) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Density = n/(0.04166667*2.47)) %>% # density/ha. Divide by 0.1011714 bc subplots are 1/4 acre in area
  dplyr::group_by(tree.invyr, tree.plt_cn) %>% 
  dplyr::summarise_at(vars(Density), list(MedianDensity = median)) %>% 
  dplyr::ungroup() 

names(mature_density) <- c("tree.invyr", "tree.plt_cn", "mature_density")

# microplots (trees < 5cm DBH)
sapling_density <- dat_restricted %>% 
  dplyr::filter(., tree.dia < 5) %>% 
  dplyr::group_by(tree.invyr, tree.plt_cn, tree.subp) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Density = n/0.00134895) %>% # density/ha. Divide by 0.1011714 bc subplots are 1/300 acre in area
  dplyr::group_by(tree.invyr, tree.plt_cn) %>% 
  dplyr::summarise_at(vars(Density), list(MedianDensity = median)) %>% 
  dplyr::ungroup() 

names(sapling_density) <- c("tree.invyr", "tree.plt_cn", "sapling_density")

density_dbh_1_plus <- dplyr::left_join(mature_density, sapling_density, by = (c("tree.plt_cn", "tree.invyr")))

density_dbh_1_plus <- density_dbh_1_plus %>% 
  dplyr::mutate(., sapling_density = ifelse(is.na(sapling_density), 0, sapling_density)) %>% 
  dplyr::mutate(., type = "included")

density_dbh_1_plus %>% 
  tidyr::gather(., class, density, - tree.plt_cn, -tree.invyr, - type) %>% 
  ggplot2::ggplot(data = , ggplot2::aes(x = density, fill = class))+
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~class, scales = "free")


##########################################################
# Removed plots
############################################################

## Subplots (trees >= 5 cm DBH)

mature_density_left_over <- left_over %>% 
  dplyr::filter(., tree.dia >= 5) %>% 
  dplyr::group_by(tree.invyr, tree.plt_cn, tree.subp) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Density = n/0.1011714) %>% # density/ha. Divide by 0.1011714 bc subplots are 1/4 acre in area
  dplyr::group_by(tree.invyr, tree.plt_cn) %>% 
  dplyr::summarise_at(vars(Density), list(MedianDensity = median)) %>% 
  dplyr::ungroup() 

names(mature_density_left_over) <- c("tree.invyr", "tree.plt_cn", "mature_density")

# microplots (trees < 5cm DBH)
sapling_density_left_over <- left_over %>% 
  dplyr::filter(., tree.dia < 5) %>% 
  dplyr::group_by(tree.invyr, tree.plt_cn, tree.subp) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Density = n/0.00134895) %>% # density/ha. Divide by 0.1011714 bc subplots are 1/300 acre in area
  dplyr::group_by(tree.invyr, tree.plt_cn) %>% 
  dplyr::summarise_at(vars(Density), list(MedianDensity = median)) %>% 
  dplyr::ungroup() 

names(sapling_density_left_over) <- c("tree.invyr", "tree.plt_cn", "sapling_density")

density_dbh_1_plus_left_over <- dplyr::left_join(mature_density_left_over, sapling_density_left_over, by = c("tree.plt_cn", "tree.invyr"))

density_dbh_1_plus_left_over <- density_dbh_1_plus_left_over %>% 
  dplyr::mutate(., sapling_density = ifelse(is.na(sapling_density), 0, sapling_density)) %>% 
  dplyr::mutate(., type = "left over")


all <- dplyr::bind_rows(density_dbh_1_plus, density_dbh_1_plus_left_over)

all %>% 
  tidyr::gather(., class, density, - tree.plt_cn,  -tree.invyr, - type) %>% 
  ggplot2::ggplot(data = , ggplot2::aes(x = density, fill = type))+
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~class, scales = "free")


###############################################################################################
#                                         ALL
##############################################################################################
mature_density <- dat %>% 
  dplyr::filter(., tree.dia >= 5) %>% 
  dplyr::group_by(tree.invyr, tree.plt_cn, tree.subp, tree.tpa_unadj) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Density = n*tree.tpa_unadj) %>% # density/ha. Divide by 0.1011714 bc subplots are 1/4 acre in area
  dplyr::group_by(tree.invyr, tree.plt_cn) %>% 
  dplyr::summarise_at(vars(Density), lst(mature_density = median)) %>% 
  dplyr::ungroup() 

names(mature_density) <- c("tree.invyr", "tree.plt_cn", "mature_density")

# microplots (trees < 5cm DBH)
sapling_density <- dat %>% 
  dplyr::filter(., tree.dia < 5) %>% 
  dplyr::group_by(tree.invyr, tree.plt_cn, tree.subp, tree.tpa_unadj) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Density = n* tree.tpa_unadj) %>% # density/ha. Divide by 0.1011714 bc subplots are 1/300 acre in area
  dplyr::group_by(tree.invyr, tree.plt_cn) %>% 
  dplyr::summarise_at(vars(Density), lst(sapling_density = median)) %>% 
  dplyr::ungroup() 

names(sapling_density) <- c("tree.invyr", "tree.plt_cn", "sapling_density")

density_dbh_1_plus <- dplyr::full_join(mature_density, sapling_density, by = c("tree.plt_cn", "tree.invyr"))  %>% 
  dplyr::mutate(., sapling_tph = sapling_density * 2.47) %>% 
  dplyr::mutate(., mature_tph = mature_density * 2.47)# %>% 
  # dplyr::select(., tree.invyr, tree.plt_cn, sapling_tph, mature_tph)

#TODO:
#Omiting NAs so that starting populations have a distribution of both saplings and mature trees. Not sure 
#how to justify this or if it's necessary.


# a <- density_dbh_1_plus %>% 
#   # dplyr::mutate(., unique_plot.x = ifelse(!is.na(unique_plot.x), unique_plot.x, unique_plot.y)) %>% 
#   # dplyr::select(., inv.yr = invyr.x, unique_plot = unique_plot.x, mature_density, sapling_density, unique) %>% 
#   dplyr::mutate(., sapling_density = ifelse(is.na(sapling_density), 0, sapling_density)) %>% 
#   dplyr::mutate(., mature_density = ifelse(is.na(mature_density), 0, mature_density))

density_dbh_1_plus %>% 
  dplyr::select(., tree.invyr, tree.plt_cn, sapling_tph, mature_tph) %>% 
  tidyr::gather(., class, density, - tree.plt_cn,  -tree.invyr) %>% 
  ggplot2::ggplot(data = , ggplot2::aes(x = density, fill = class))+
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~class)

save(density_dbh_1_plus, file = paste0(proj_dir, data_dir, "FIA derived densitites.Rda"))
####################################################################################################
# Seedlings #
####################################################################################################
seedlingDat %>%  
  group_by(., seedling.invyr, seedling.plt_cn, seedling.subp, seedling.condid) %>% 
  tally() %>% 
  filter(., n > 1) # no subplots in multiple conditions (condid)
sum(duplicated(paste(seedlingDat$seedling.plt_cn, seedlingDat$seedling.subp, seedlingDat$seedling.condid)))

seedlingDat %>%  
  group_by(., seedling.invyr, seedling.plt_cn, seedling.subp) %>% 
  tally() %>% 
  filter(., n > 1) # subplots are unique
sum(duplicated(paste(seedlingDat$seedling.plt_cn, seedlingDat$seedling.subp)))

seedling_density <- seedlingDat %>% 
  dplyr::group_by(., seedling.plt_cn,seedling.invyr) %>% 
  dplyr::summarise_at(vars(seedling.tpa_unadj), list(meanDens = mean)) %>% 
  dplyr::mutate(., seedling_tph = meanDens * 2.47) #%>% #2.47 acres per hectare
  dplyr::select(., seedling.plt_cn, seedling.invyr, seedling_tph)


## Merge with dat

dat1 <- merge(density_dbh_1_plus, seedling_density, by.x = c("tree.plt_cn", "tree.invyr"), 
              by.y = c("seedling.plt_cn", "seedling.invyr"), all = TRUE) %>% 
  mutate(., seedling_count = seedling_tph * 1000) %>% 
  mutate(., sapling_count = sapling_tph * 1000) %>% 
  mutate(., mature_count = mature_tph * 1000)  

fia_density <- dat1 %>% 
  dplyr::select(., tree.plt_cn, tree.invyr, seedling_count, sapling_count, mature_count)

fia_density[is.na(fia_density)] <- 0

nrow(fia_density)

fia_density %>% 
  gather(., stage, count, -tree.plt_cn, -tree.invyr) %>% 
  ggplot(aes(x = count, fill = stage)) +
  geom_histogram(position = "dodge")

  


save(fia_density, file = paste0(proj_dir, data_dir, "fia density.Rda"))
