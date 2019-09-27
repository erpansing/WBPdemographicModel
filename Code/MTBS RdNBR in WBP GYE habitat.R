rm(list = ls())
library(tidyverse)
library(raster)
library(rgeos)
library(rgdal)
library(ggmap)
library(gtable)
library(grid)
library(maps)
################################################################################
#---------------------------------------|---------------------------------------
#                             Establish directories
#---------------------------------------|---------------------------------------
################################################################################

proj_dir <- '/Users/elizabethpansing/Box/PhD/Code/WBP Demographic Model Master/Dis_WBP_MODEL_ACTIVE/'
proj_dir_linux <- '/Users/elizabethpansing/Box/PhD/Code/WBP Demographic Model Master/Dis_WBP_MODEL_ACTIVE/'
data_dir <- 'Data/'
rda_dir <- "Rda/"
code_dir <- "Code/"
model_file <- "Pansing et al. MPM Model Final? Gamma fire.R"
figures_dir <- "Figures/Final?/"
export_table_dir <-  "Exported Data Tables/"
geospatial_dir <- "Geospatial Data/"

################################
#
#     Plotting parameters
#
################################

axis_title_size = 25
axis_text_size  = 12
strip_text_size = 20

################################################################################
#---------------------------------------|---------------------------------------
#                               Import wbp range
#---------------------------------------|---------------------------------------
################################################################################

# Import wbp range map
GDALinfo("/Users/elizabethpansing/Box/References/WBP Range Maps/WBP_USdist_Apr2019/WBP_USdist_Apr20191.tif")
wbp_range <- raster("/Users/elizabethpansing/Box/References/WBP Range Maps/WBP_USdist_Apr2019/WBP_USdist_Apr20191.tif")
(wbp_crs <- crs(wbp_range))


# crop to Greater Yellowstone Ecosystem (GYE) extent
ynp <- rgdal::readOGR(dsn = paste0("/Users/elizabethpansing/Box/PhD/Tibbs Surivival/", data_dir,geospatial_dir,"National_Park_Boundaries_GYE"),
                      "National_Park_Boundaries_GYE") %>%
  sp::spTransform(., CRS("+proj=longlat +zone=12 ellps=WGS84"))
gye_nf <- rgdal::readOGR(dsn = paste0("/Users/elizabethpansing/Box/PhD/Tibbs Surivival/", data_dir,geospatial_dir,"National_Forest_Boundaries_GYE"),
                         "National_Forest_Boundaries_GYE") %>%
  sp::spTransform(., CRS("+proj=longlat +zone=12 ellps=WGS84"))
gye <- rbind(ynp, gye_nf, makeUniqueIDs = TRUE)
#
lps <- sp::coordinates(gye)
#
IDOneBin <- cut(lps[,1], range(lps[,1]), include.lowest=TRUE)
gye   <- maptools::unionSpatialPolygons(gye ,IDOneBin)

#check to make sure projections are the same
crs(gye)
crs(wbp_range)

gye <- spTransform(gye, CRS = crs(wbp_range))## Transform the gye bc the wbp range is a raster of identical CRS to the fire rasters
identical(crs(gye), crs(wbp_range))

gye_extent <- extent(gye)

# crop
gye_wbp <- crop(wbp_range, gye_extent)
plot(gye_wbp)

################################################################################
#---------------------------------------|---------------------------------------
#                           Import fire severity geotiffs 
#---------------------------------------|---------------------------------------
################################################################################


# This process:
# 1) unzips all fire files from the specified state,
# 2) deletes all files that are not fire severity maps
# 3) loads all geotiffs into the global environment

MTBS_rdnbr_dir <- "/Users/elizabethpansing/Documents/MTBS_RdNBR/"

#---------------------------------------|---------------------------------------
#        Unzip files and delete all files that are not fire severity maps 
#---------------------------------------|---------------------------------------

# Only needs to be run the first time. These steps are unnecessary going forward,
# as the files have already been unzipped.

#Extract file names
# 
# files <- list.files(MTBS_rdnbr_dir)
# 
#   for(i in unique(files)){
# 
#     fire_files <- list.files(paste0(MTBS_rdnbr_dir, i, "/fire_level_tar_files"))
# 
#     for(j in unique(fire_files)){
#       system(paste0("cd ", MTBS_rdnbr_dir, i,"/fire_level_tar_files && tar xvzf ", MTBS_rdnbr_dir, i,"/fire_level_tar_files/", j))
#       files <- list.files(paste0(MTBS_rdnbr_dir, i,"/fire_level_tar_files"))
#       if (sum(grepl(x = files, pattern = "_rdnbr.tif")) == 0){
#         system(paste0("cd ",MTBS_rdnbr_dir, i,"/fire_level_tar_files && ls -1 | grep -v '.tar.gz' | xargs rm -f"))
#       } else {system(paste0("mkdir ", MTBS_rdnbr_dir, i,"/", gsub(".tar.gz", "", j),
#                     " && mkdir ",MTBS_rdnbr_dir,  i,"/", gsub(".tar.gz", "", j),"/shpfile && mkdir ",MTBS_rdnbr_dir, i,"/", gsub(".tar.gz", "", j),"/rdnbr",
#                     " && mv ",MTBS_rdnbr_dir, i,"/fire_level_tar_files/*_rdnbr.tif ",MTBS_rdnbr_dir, i,"/",gsub(".tar.gz", "", j),"/rdnbr",
#                     " && mv ",MTBS_rdnbr_dir, i,"/fire_level_tar_files/*_bndy.prj ", MTBS_rdnbr_dir, i,"/",gsub(".tar.gz", "", j),"/shpfile",
#                     " && mv ",MTBS_rdnbr_dir, i,"/fire_level_tar_files/*_bndy.dbf ", MTBS_rdnbr_dir, i,"/",gsub(".tar.gz", "", j),"/shpfile",
#                     " && mv ",MTBS_rdnbr_dir, i,"/fire_level_tar_files/*_bndy.sbn ", MTBS_rdnbr_dir, i,"/",gsub(".tar.gz", "", j),"/shpfile",
#                     " && mv ",MTBS_rdnbr_dir, i,"/fire_level_tar_files/*_bndy.sbx ", MTBS_rdnbr_dir, i,"/",gsub(".tar.gz", "", j),"/shpfile",
#                     " && mv ",MTBS_rdnbr_dir, i,"/fire_level_tar_files/*_bndy.shp ", MTBS_rdnbr_dir, i,"/",gsub(".tar.gz", "", j),"/shpfile",
#                     " && mv ",MTBS_rdnbr_dir, i,"/fire_level_tar_files/*_bndy.shx ", MTBS_rdnbr_dir, i,"/",gsub(".tar.gz", "", j),"/shpfile"))
#       }
#     }
#     system(paste0("rm -r ",MTBS_rdnbr_dir, i,"/fire_level_tar_files"))
#   }


#---------------------------------------|---------------------------------------
#                                Load all geotiffs 
#---------------------------------------|---------------------------------------
#Extract file names
MTBS_rdnbr_dir <- "/Users/elizabethpansing/Documents/MTBS_rdnbr/"

#loop through fire directories and import all files ending in "_rrdnbr.tif"
# these files provide Rrdnbr values for each pixel within the fire perimeter

files <- list.files(paste0(MTBS_rdnbr_dir))

for(i in files){
  fires <- list.files(paste0(MTBS_rdnbr_dir,i))
  for(k in fires){
    name <- paste0(i,"_",k)
    fire_bndy_file_name <- list.files(paste0(MTBS_rdnbr_dir, i, "/", k,"/shpfile"))
    out <- fire_bndy_file_name[grepl(fire_bndy_file_name, pattern = ".dbf")]
    shp_file_name <- gsub(".dbf", "", x = out)
    fire_directory <- paste0(MTBS_rdnbr_dir,i,"/",k)
    # import file boundary shapefile  
    try(assign(paste0(name,"_boundary"), rgdal::readOGR(paste0(fire_directory,"/shpfile"), shp_file_name)), silent = FALSE)
    # import rdnbr raster file
    rdnbr_file <- list.files(paste0(fire_directory,"/rdnbr"))
    try(assign(paste0(name,"_rdnbr"), raster::raster(paste0(fire_directory, "/rdnbr/", rdnbr_file))))
  }
}


# Make sure there is a polygon and raster for each fire

fires_genv <- grep("_id|_wy|_mt",names(.GlobalEnv),value=TRUE)
fires_genv_generic <- gsub(pattern = "_rdnbr", replacement = "", x = fires_genv)
fires_genv_generic <- gsub(pattern = "_boundary", replacement = "", x = fires_genv_generic)

fires_genv_generic <- data.frame(fire = fires_genv_generic, file = fires_genv)

(missing <- fires_genv_generic %>% 
  group_by(fire) %>% 
  tally() %>% 
  filter(., n != 2))

ID_2000_id4217911557220000626_rdnbr <- raster("/Users/elizabethpansing/Documents/MTBS_RdNBR/ID/1998/id4275411271419980831/rdnbr/id4275411271419980831_19980810_19980911_rdnbr.tif")
ID_2000_id4375011512820000815_boundary <- readOGR("/Users/elizabethpansing/Documents/MTBS_RdNBR/ID/2000/id4375011512820000815/shpfile")
ID_2000_id4317611213220000818_rdnbr <- raster("/Users/elizabethpansing/Documents/MTBS_RdNBR/ID/2000/id4317611213220000818/rdnbr/id4317611213220000818_20000604_20010615_rdnbr.tif")
ID_2000_id4256311532020000823_rdnbr <- raster("/Users/elizabethpansing/Documents/MTBS_RdNBR/ID/2000/id4256311532020000823/rdnbr/id4256311532020000823_20000712_20000829_rdnbr.tif")

## Check to make sure that projections are the same

identical(crs(wbp_range), crs(WY_1988_wy4470811082119880722_rdnbr))
identical(crs(wbp_range), crs(WY_1988_wy4470811082119880722_rdnbr))
identical(crs(wbp_range), crs(WY_1988_wy4470811082119880722_rdnbr))

################################################################################
#---------------------------------------|---------------------------------------
# Determine the proportion of each fire that is high, moderate, and low severity
#---------------------------------------|---------------------------------------
################################################################################

# This process considers the wildfire as the sampling unit and estimates the 
# the proportion of each fire that experienced high, moderate, and low intensity burns.
#
# For this analysis, we focus on fires within the GYE because the wbp range map
# has substantially higher resolution than other areas of the range map.
#
# For each fire, the steps are as follows:
# 1) Omit fire severity levels 0, 1, 5, and 6. These values correspond to 
#    NA values, unburned/low severity, increased greenness, and non-mappable area,
#    respectively. We omit these to focus on burns that impacted tree cover.
# 2) Reproject the wbp range rasters so the extent and resolutions are consistent 
#    with the fire rasters (30 x 30 m resolution, extent varies by fire)
# 3) Mask (i.e., coerce to NA) values in the fire raster that do not have values in
#    the wbp range map. This is done to ensure that we're only evaluating fire
#    severity of areas that burned in WBP habitat.
# 4) Calculate the proportion of cells in each fire that experienced high, moderate,
#    and low severity fire. RdNBR fire severity thresholds are those used by Miller & Thode 2007.
#    We re-run the same analysis using the stand replacing threshold described by Harvey 2015.


Pattern <- grep(".rdnbr", names(.GlobalEnv),value = TRUE)
length(Pattern)
sum(duplicated(Pattern))

high <- NULL
moderate <- NULL
low <- NULL
unburned <- NULL
ID <- NULL

for(i in Pattern){
  cat(paste0("Running iteration ", which(Pattern == i), " of ",length(Pattern) ,"..."))
  fire <- mget(i)[[1]] # get the raster object that corresponds to pattern[i] character string
  boundary <- mget(gsub("_rdnbr", "_boundary", i))[[1]]
  new_projection <- try(new_projection <- projectRaster(gye_wbp, fire, res = 30), silent = FALSE)
  if(isTRUE(class(new_projection) == "try-error")){ 
    high[which(Pattern == i)] <- NA
    moderate[which(Pattern == i)] <- NA
    low[which(Pattern == i)] <- NA
    unburned[which(Pattern == i)] <- NA
    ID[which(Pattern == i)] <- Pattern[i]
    cat(paste0("DONE.\n")) 
  } else if(isTRUE(class(new_projection) != "try-error")){
    # new_projection <- projectRaster(gye_wbp, fire)
    # cropped_range <- crop(new_projection, fire)
    masked_fire <- mask(fire, new_projection)
    # masked_fire[masked_fire < 69] <- NA  # Of the areas that burned, what proportion were low, moderate, severe. Not including unburned area within the perimeter
    values_within_boundary <- extract(masked_fire, boundary)[[1]] # extract rdnbr values from pixels that lie within the fire perimeter
    prop_high <- sum(values_within_boundary > 641, na.rm = T)/sum(!is.na(values_within_boundary))
    prop_moderate <- sum(values_within_boundary >= 316 & values_within_boundary < 640, na.rm = T)/sum(!is.na(values_within_boundary))
    prop_low <- sum(values_within_boundary >= 69 & values_within_boundary < 315, na.rm = T)/sum(!is.na(values_within_boundary))
    prop_unburned <- sum(values_within_boundary < 69, na.rm = T)/sum(!is.na(values_within_boundary))
    # prop_unchanged <- sum(values(masked_fire)< 69, na.rm = T)/sum(!is.na(values(masked_fire)))
    high[which(Pattern == i)] <- prop_high
    moderate[which(Pattern == i)] <- prop_moderate
    low[which(Pattern == i)] <- prop_low
    unburned[which(Pattern == i)] <- prop_unburned
    ID[which(Pattern == i)] <- Pattern[i]
    cat(paste0("DONE.\n"))
  }
}


dat <- matrix(c(high, moderate, low, unburned), ncol = 4, byrow = F)
total <- apply(dat, MARGIN = 1, FUN = sum)
range(total, na.rm = T)

rdnbr_dat <- data.frame( High = high, Moderate = moderate, Low = low, Unburned = unburned) %>% 
  na.omit(.)
  
rdnbr_dat_plot <- rdnbr_dat %>% 
  gather(., severity, prop)

save(rdnbr_dat, file = paste0(proj_dir, data_dir,"rdnbr_dat.Rds"))
load(paste0(proj_dir, data_dir, "rdnbr_dat.Rds"))

rdnbr_dat_plot %>% 
  dplyr::mutate(., severity = factor(severity, levels = c("High", "Moderate", "Low", "Unburned"))) %>% 
  # filter(., severity == "high")%>% 
  ggplot(aes(x = prop, col = severity, fill = severity, bins = 50))+
  geom_density(alpha = 0.65)+
  scale_fill_manual(values = c("#ca0020", "#f4a582","#92c5de","#0571b0")) +
  scale_color_manual(values = c("#ca0020", "#f4a582","#92c5de","#0571b0")) +
  theme_bw() +
  labs(x = "Proportion", y = "Density")+
  theme(axis.title = element_text(size = axis_title_size)) + 
  theme(axis.text  = element_text(size = axis_text_size)) +
  theme(legend.position = "none")+
  # facet_grid(~severity) + 
  ggplot2::theme(strip.background = ggplot2::element_rect(colour = "black", fill = "white"),
                 strip.text       = ggplot2::element_text(colour = "black",  size = strip_text_size))+
  ggplot2::theme(plot.margin = grid::unit(c(3, 1, 1, 1), "lines"))  # top, right, bottom, left

legd <- grid::legendGrob(c("High","Moderate", "Low", "Unburned"),
                         nrow          = 1,
                         ncol          = 4,
                         lines.first   = TRUE,
                         hgap          = grid::unit(2, "lines"),
                         vgap          = grid::unit(1, "lines"),
                         default.units = "lines",
                         pch           = 22,
                         gp = grid::gpar(col      = rep("black",4),
                                         fill     = c("#ca0020", "#f4a582","#92c5de","#0571b0"),
                                         fontsize = axis_text_size,
                                         fontface = "bold"),
                         vp = grid::viewport(x    = 0,
                                             y    = 0,
                                             w    = 1.05,
                                             h    = 1.94,
                                             just = c("left", "bottom")))

grid::grid.draw(legd)


noNAdat_rdnbr <- na.omit(rdnbr_dat)

(summary_rdnbr <- noNAdat_rdnbr %>% 
    group_by(severity) %>% 
    summarise_at(., vars(prop), funs(mean, median)) %>% 
    ungroup)

base::colSums(summary_rdnbr[,2:3])


# Harvey 2015 stand replacing RdNBR threshold

high_harvey <- NULL
moderate_harvey <- NULL
low_harvey <- NULL
ID <- NULL

for(i in Pattern){
  cat(paste0("Running iteration ", which(Pattern == i), " of ",length(Pattern) ,"..."))
  fire <- mget(i)[[1]] # get the raster object that corresponds to pattern[i] character string
  boundary <- mget(gsub("_rdnbr", "_boundary", i))[[1]]
  new_projection <- try(new_projection <- projectRaster(gye_wbp, fire, res = 30), silent = FALSE)
  if(isTRUE(class(new_projection) == "try-error")){ 
    high_harvey[which(Pattern == i)] <- NA
    moderate_harvey[which(Pattern == i)] <- NA
    low_harvey[which(Pattern == i)] <- NA
    ID[which(Pattern == i)] <- Pattern[i]
    cat(paste0("DONE.\n")) 
  } else if(isTRUE(class(new_projection) != "try-error")){
    # new_projection <- projectRaster(gye_wbp, fire)
    # cropped_range <- crop(new_projection, fire)
    masked_fire <- mask(fire, new_projection)
    masked_fire[masked_fire < 69] <- NA  # Of the areas that burned, what proportion were low, moderate, severe. Not including unburned area within the perimeter
    values_within_boundary <- extract(masked_fire, boundary)[[1]] # extract rdnbr values from pixels that lie within the fire perimeter
    prop_high <- sum(values_within_boundary > 675, na.rm = T)/sum(!is.na(values_within_boundary))
    prop_moderate <- sum(values_within_boundary >= 316 & values_within_boundary < 675, na.rm = T)/sum(!is.na(values_within_boundary))
    prop_low <- sum(values_within_boundary >= 69 & values_within_boundary < 315, na.rm = T)/sum(!is.na(values_within_boundary))
    # prop_unchanged <- sum(values(masked_fire)< 69, na.rm = T)/sum(!is.na(values(masked_fire)))
    high_harvey[which(Pattern == i)] <- prop_high
    moderate_harvey[which(Pattern == i)] <- prop_moderate
    low_harvey[which(Pattern == i)] <- prop_low
    ID[which(Pattern == i)] <- Pattern[i]
    cat(paste0("DONE.\n"))
  }
}

rdnbr_harvey_dat <- data.frame(id = Pattern, high = high_harvey, moderate = moderate_harvey, low = low_harvey) %>%  
  gather(., severity, prop, -id)

save(rdnbr_harvey_dat, file = paste0(proj_dir, data_dir,"rdnbr_harvey_dat.Rds"))
load(paste0(proj_dir, data_dir, "rdnbr_harvey_dat.Rds"))

rdnbr_harvey_dat %>% 
  # filter(., severity == "high")%>% 
  ggplot(aes(x = prop, col = severity, fill = severity))+
  geom_density(alpha = 0.2, bins = 40)#+
# facet_wrap(~severity, ncol = 1)

noNAdat_rdnbr_harvey <- na.omit(rdnbr_harvey_dat)

(summary_rdnbr_harvey <- noNAdat_rdnbr_harvey %>% 
    group_by(severity) %>% 
    summarise_at(., vars(prop), funs(mean, median)) %>% 
    ungroup)

#---------------------------------------|---------------------------------------
#                          Entire Rocky Mountain Region
#---------------------------------------|---------------------------------------

# Pattern <- grep(".tif",names(.GlobalEnv),value=TRUE)
# length(Pattern)
# sum(duplicated(Pattern))
# 
# high_range <- NULL
# moderate_range <- NULL
# low_range <- NULL
# ID <- NULL
# 
# for(i in Pattern){
#   cat(paste0("Running iteration ", which(Pattern == i), " of ",length(Pattern) ,"..."))
#   fire <- mget(i)[[1]] # get the raster object that corresponds to pattern[i] character string
#   fire[fire == 0 | fire == 1 | fire == 5 |fire == 6] <- NA # set to NA values that are 0 (NA), 1 (unburned/low), or 6 (increased greenness)
#   fire <- as.factor(fire)
#   new_projection <- projectRaster(wbp_range, fire)
#   new_projection <- as.factor(new_projection)
#   # cropped_range <- crop(new_projection, fire)
#   masked_fire <- mask(fire, new_projection)
#   prop_high <- sum(values(masked_fire) == "4", na.rm = T)/sum(!is.na(values(masked_fire)))
#   prop_moderate <- sum(values(masked_fire) == "3", na.rm = T)/sum(!is.na(values(masked_fire)))
#   prop_low <- sum(values(masked_fire) == "2", na.rm = T)/sum(!is.na(values(masked_fire)))
#   high_range[which(Pattern == i)] <- prop_high
#   moderate_range[which(Pattern == i)] <- prop_moderate
#   low_range[which(Pattern == i)] <- prop_low
#   ID[which(Pattern == i)] <- Pattern[i]
#   cat(paste0("DONE.\n"))
# }
# 
# dat_range <- matrix(c(high_range, moderate_range, low_range), ncol = 3, byrow = F)
# total_range <- apply(dat_range, MARGIN = 1, FUN = sum)
# range(total_range, na.rm = T)
# 
# dat_range <- data.frame(id = Pattern, high = high_range, moderate = moderate_range,
#                         low = low_range) %>%  
#   gather(., severity, prop, -id)
# 
# dat_range %>% 
#   # filter(., severity == "high")%>% 
#   ggplot(aes(x = prop, col = severity, fill = severity))+
#   geom_density(alpha = 0.2, bins = 40) #+
# # facet_wrap(~severity, ncol = 1)
# 
# noNAdat_range <- na.omit(dat_range)
# 
# noNAdat_range %>% 
#   group_by(severity) %>% 
#   summarise_at(., vars(prop), funs(mean, median))


#---------------------------------------|---------------------------------------
#       Determine the parameters that define the multinomial distribution
#---------------------------------------|---------------------------------------


