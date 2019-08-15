rm(list = ls())

library(RPostgreSQL)
library(tidyverse)
library(rgdal)
library(raster)

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

plotDat <- DBI::dbGetQuery(conn = fia_db_conn, statement = "SELECT cn, manual_db, manual_rmrs, lat, lon, elev FROM wy_plot")
names(plotDat) <- paste0("plot.", names(plotDat))

dat <- merge(treeDat, plotDat, by.x="tree.plt_cn", by.y="plot.cn", all.x=TRUE) %>% 
  dplyr::filter(., plot.elev >= 7500)

DBI::dbDisconnect(fia_db_conn)

system(paste0("pg_ctl -D /usr/local/var/postgres stop"))

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
plot(wbp_range)

# Import wyoming shapefile
wyo <- rgdal::readOGR(dsn = paste0("/Users/elizabethpansing/Box/PhD/Tibbs Surivival/", data_dir,geospatial_dir,"tl_2016_56_cousub"),
                      "tl_2016_56_cousub") 
plot(wyo)
raster::crs(wyo)
class(wyo)

# Merge counties so only the outline of the state remains
lps <- sp::coordinates(wyo)
#
IDOneBin <- raster::cut(lps[,1], range(lps[,1]), include.lowest=TRUE)
wyo   <- maptools::unionSpatialPolygons(wyo ,IDOneBin)

plot(wyo)

wyo <- sp::spTransform(wyo, CRS = raster::crs(wbp_range))## Transform the gye bc the wbp range is a raster layer, which is more difficult to change
identical(raster::crs(wyo), raster::crs(wbp_range))

## Select only the portion of whitebark pine's range that is within wyoming. 
# 1) crop the range shp to be the same as wyo
# 2) mask the portions of the range outside of the wyo border

wbp_wyo <- raster::crop(wbp_range, wyo)
wbp_wyo <- raster::mask(wbp_wyo, wyo)

plot(wyo)
plot(wbp_wyo, add = T)


# map FIA plot "locations" and wbp range in wyo

coords <- dat %>% 
  dplyr::select(., plot.lon, plot.lat)
sp_dat <- dat %>% 
  dplyr::select(., -plot.lon, -plot.lat)

plots <- sp::SpatialPointsDataFrame(coords = coords, data = dat, proj4string =  CRS("+init=epsg:4269"))
plots <- sp::spTransform(plots, CRS = raster::crs(wbp_range))
identical(raster::crs(wbp_wyo), raster::crs(plots))

plot(wbp_wyo)
plot(wyo, add = T)
points(plots)

ones <- which(raster::extract(wbp_wyo == 1,plots) == 1)
head(ones)
plots1 <- plots[ones,]

plot(wbp_wyo)
plot(wyo, add = T)
points(plots1)

plots_keep <- as.data.frame(plots1)

dat_restricted <- dat %>% 
  dplyr::filter(., tree.plot %in% plots_keep$tree.plot)

left_over <- dat %>% 
  dplyr::filter(., !(tree.plot %in% plots_keep$tree.plot))

###################################################################################################
#                                    Estimate densities
###################################################################################################
# Estimate plot densities during each survey. There are two different designs for different sized
# trees. Trees with DBH >= 5cm were measured in the entire subplot. Trees with DBH <5 cm were only
# measured in nested microplots. 
#
# I assume these are reasonable representations of sapling and mature classes present in the 
# model.


## Subplots (trees >= 5 cm DBH)

mature_density <- dat_restricted %>% 
  dplyr::filter(., tree.dia >= 5) %>% 
  dplyr::group_by(tree.invyr, tree.plot, tree.subp) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Density = n/0.1011714) %>% # density/ha. Divide by 0.1011714 bc subplots are 1/4 acre in area
  dplyr::group_by(tree.invyr, tree.plot) %>% 
  dplyr::summarise_at(vars(Density), list(MedianDensity = median)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., unique = paste(tree.plot, tree.invyr))

names(mature_density) <- c("inv.yr", "plot", "mature_density", "unique")

# microplots (trees < 5cm DBH)
sapling_density <- dat_restricted %>% 
  dplyr::filter(., tree.dia < 5) %>% 
  dplyr::group_by(tree.invyr, tree.plot, tree.subp) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Density = n/0.00134895) %>% # density/ha. Divide by 0.1011714 bc subplots are 1/300 acre in area
  dplyr::group_by(tree.invyr, tree.plot) %>% 
  dplyr::summarise_at(vars(Density), list(MedianDensity = median)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., unique = paste(tree.plot, tree.invyr))

names(sapling_density) <- c("inv.yr", "plot", "sapling_density", "unique")

density_dbh_1_plus <- dplyr::left_join(mature_density, sapling_density, by = "unique")

density_dbh_1_plus <- density_dbh_1_plus %>% 
  dplyr::select(., inv.yr = inv.yr.x, plot = plot.x, mature_density, sapling_density, unique) %>% 
  dplyr::mutate(., sapling_density = ifelse(is.na(sapling_density), 0, sapling_density)) %>% 
  dplyr::mutate(., type = "included")

density_dbh_1_plus %>% 
  tidyr::gather(., class, density, - unique, - plot, -inv.yr) %>% 
  ggplot2::ggplot(data = , ggplot2::aes(x = density, fill = class))+
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~class, scales = "free")


##########################################################
# Removed plots
############################################################

## Subplots (trees >= 5 cm DBH)

mature_density_left_over <- left_over %>% 
  dplyr::filter(., tree.dia >= 5) %>% 
  dplyr::group_by(tree.invyr, tree.plot, tree.subp) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Density = n/0.1011714) %>% # density/ha. Divide by 0.1011714 bc subplots are 1/4 acre in area
  dplyr::group_by(tree.invyr, tree.plot) %>% 
  dplyr::summarise_at(vars(Density), list(MedianDensity = median)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., unique = paste(tree.plot, tree.invyr))

names(mature_density_left_over) <- c("inv.yr", "plot", "mature_density", "unique")

# microplots (trees < 5cm DBH)
sapling_density_left_over <- left_over %>% 
  dplyr::filter(., tree.dia < 5) %>% 
  dplyr::group_by(tree.invyr, tree.plot, tree.subp) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Density = n/0.00134895) %>% # density/ha. Divide by 0.1011714 bc subplots are 1/300 acre in area
  dplyr::group_by(tree.invyr, tree.plot) %>% 
  dplyr::summarise_at(vars(Density), list(MedianDensity = median)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., unique = paste(tree.plot, tree.invyr))

names(sapling_density_left_over) <- c("inv.yr", "plot", "sapling_density", "unique")

density_dbh_1_plus_left_over <- dplyr::left_join(mature_density_left_over, sapling_density_left_over, by = "unique")

density_dbh_1_plus_left_over <- density_dbh_1_plus_left_over %>% 
  dplyr::select(., inv.yr = inv.yr.x, plot = plot.x, mature_density, sapling_density, unique) %>% 
  dplyr::mutate(., sapling_density = ifelse(is.na(sapling_density), 0, sapling_density)) %>% 
  dplyr::mutate(., type = "left over")


all <- dplyr::bind_rows(density_dbh_1_plus, density_dbh_1_plus_left_over)

all %>% 
  tidyr::gather(., class, density, - unique, - plot, -inv.yr, - type) %>% 
  ggplot2::ggplot(data = , ggplot2::aes(x = density, fill = type))+
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~class, scales = "free")


###############################################################################################
#                                         ALL
##############################################################################################
mature_density <- dat %>% 
  dplyr::filter(., tree.dia >= 5) %>% 
  dplyr::group_by(tree.invyr, tree.plot, tree.subp) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Density = n/0.1011714) %>% # density/ha. Divide by 0.1011714 bc subplots are 1/4 acre in area
  dplyr::group_by(tree.invyr, tree.plot) %>% 
  dplyr::summarise_at(vars(Density), lst(mature_density = median)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., unique = paste(tree.plot, tree.invyr))

names(mature_density) <- c("inv.yr", "plot", "mature_density", "unique")

# microplots (trees < 5cm DBH)
sapling_density <- dat %>% 
  dplyr::filter(., tree.dia < 5) %>% 
  dplyr::group_by(tree.invyr, tree.plot, tree.subp) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., Density = n/0.00134895) %>% # density/ha. Divide by 0.1011714 bc subplots are 1/300 acre in area
  dplyr::group_by(tree.invyr, tree.plot) %>% 
  dplyr::summarise_at(vars(Density), lst(sapling_density = median)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., unique = paste(tree.plot, tree.invyr))

names(sapling_density) <- c("inv.yr", "plot", "sapling_density", "unique")

density_dbh_1_plus <- dplyr::full_join(mature_density, sapling_density, by = "unique")

density_dbh_1_plus <- density_dbh_1_plus %>% 
  dplyr::mutate(., plot.x = ifelse(!is.na(plot.x), plot.x, plot.y)) %>% 
  dplyr::select(., inv.yr = inv.yr.x, plot = plot.x, mature_density, sapling_density, unique) %>% 
  dplyr::mutate(., sapling_density = ifelse(is.na(sapling_density), 0, sapling_density)) 

density_dbh_1_plus %>% 
  gather(., class, density, - unique, - plot, -inv.yr) %>% 
  ggplot(data = , aes(x = density, fill = class))+
  geom_density(alpha = 0.25) +
  facet_wrap(~class, scales = "free")

save(density_dbh_1_plus, file = paste0(proj_dir, data_dir, "FIA derived densitites.Rda"))


