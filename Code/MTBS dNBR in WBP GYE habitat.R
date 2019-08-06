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

proj_dir <- '/Users/elizabethpansing/Box Sync/PhD/Code/WBP Demographic Model Master/Dis_WBP_MODEL_ACTIVE/'
proj_dir_linux <- '/Users/elizabethpansing/Box\ Sync/PhD/Code/WBP Demographic Model Master/Dis_WBP_MODEL_ACTIVE/'
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
GDALinfo("/Users/elizabethpansing/Box Sync/References/WBP Range Maps/WBP_USdist_Apr2019/WBP_USdist_Apr20191.tif")
wbp_range <- raster("/Users/elizabethpansing/Box Sync/References/WBP Range Maps/WBP_USdist_Apr2019/WBP_USdist_Apr20191.tif")
(wbp_crs <- crs(wbp_range))


# crop to Greater Yellowstone Ecosystem (GYE) extent
ynp <- rgdal::readOGR(dsn = paste0("/Users/elizabethpansing/Box Sync/PhD/Tibbs Surivival/", data_dir,geospatial_dir,"National_Park_Boundaries_GYE"),
                      "National_Park_Boundaries_GYE") %>%
  sp::spTransform(., CRS("+proj=longlat +zone=12 ellps=WGS84"))
gye_nf <- rgdal::readOGR(dsn = paste0("/Users/elizabethpansing/Box Sync/PhD/Tibbs Surivival/", data_dir,geospatial_dir,"National_Forest_Boundaries_GYE"),
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

MTBS_dir <- "/Users/elizabethpansing/Documents/MTBS_dNBR/"

#---------------------------------------|---------------------------------------
#        Unzip files and delete all files that are not fire severity maps 
#---------------------------------------|---------------------------------------

# Only needs to be run the first time. These steps are unnecessary going forward,
# as the files have already been unzipped.

#Extract file names
states <- list.files(MTBS_dir)

for(h in states){
  files <- list.files(paste0(MTBS_dir, h))
  
  for(i in unique(files)){
    
    fire_files <- list.files(paste0(MTBS_dir, h, "/", i, "/fire_level_tar_files"))
    
    for(j in unique(fire_files)){
      system(paste0("cd ", MTBS_dir,h,"/", i,"/fire_level_tar_files && tar xvzf ",MTBS_dir,h, "/", i,"/fire_level_tar_files/", j))
      files <- list.files(paste0(MTBS_dir, h,"/", i,"/fire_level_tar_files"))
      if (sum(grepl(x = files, pattern = "_dnbr.tif")) == 0){
        system(paste0("cd ",MTBS_dir,h,"/", i,"/fire_level_tar_files && ls -1 | grep -v '.tar.gz' | xargs rm -f"))
      } else {system(paste0("mkdir " ,MTBS_dir, h,"/", i,"/", gsub(".tar.gz", "", j),
                            " && mkdir ",MTBS_dir, h,"/", i,"/", gsub(".tar.gz", "", j),"/shpfile && mkdir ",MTBS_dir, h,"/", i,"/", gsub(".tar.gz", "", j),"/dnbr",
                            " && mv ",MTBS_dir, h,"/", i,"/fire_level_tar_files/*_dnbr.tif ",MTBS_dir, h,"/", i,"/",gsub(".tar.gz", "", j),"/dnbr",
                            " && mv ",MTBS_dir, h,"/", i,"/fire_level_tar_files/*_bndy.prj ",MTBS_dir, h,"/", i,"/",gsub(".tar.gz", "", j),"/shpfile",
                            " && mv ",MTBS_dir, h,"/", i,"/fire_level_tar_files/*_bndy.dbf ",MTBS_dir, h,"/", i,"/",gsub(".tar.gz", "", j),"/shpfile",
                            " && mv ",MTBS_dir, h,"/", i,"/fire_level_tar_files/*_bndy.sbn ",MTBS_dir, h,"/", i,"/",gsub(".tar.gz", "", j),"/shpfile",
                            " && mv ",MTBS_dir, h,"/", i,"/fire_level_tar_files/*_bndy.sbx ",MTBS_dir, h,"/", i,"/",gsub(".tar.gz", "", j),"/shpfile",
                            " && mv ",MTBS_dir, h,"/", i,"/fire_level_tar_files/*_bndy.shp ",MTBS_dir, h,"/", i,"/",gsub(".tar.gz", "", j),"/shpfile",
                            " && mv ",MTBS_dir, h,"/", i,"/fire_level_tar_files/*_bndy.shx ",MTBS_dir, h,"/", i,"/",gsub(".tar.gz", "", j),"/shpfile"))
      }
    }
    system(paste0("rm -r ",MTBS_dir, h,"/", i,"/fire_level_tar_files"))
  }
}

#---------------------------------------|---------------------------------------
#                                Load all geotiffs 
#---------------------------------------|---------------------------------------
#Extract file names
MTBS_dir <- "/Users/elizabethpansing/Documents/MTBS_dnbr/"
states <- list.files(MTBS_dir)

#loop through fire directories and import all files ending in "_dnbr.tif"
# these files provide dNBR values for each pixel within the fire perimeter

for(h in states){
  files <- list.files(paste0(MTBS_dir,h))
  for(i in files){
    fires <- list.files(paste0(MTBS_dir,h,"/",i))
    for(k in fires){
      name <- paste0(h,"_", i,"_",k)
    fire_directory <- paste0(MTBS_dir,h,"/",i,"/",k)
    # import file boundary shapefile  
    try(assign(paste0(name,"_boundary"), rgdal::readOGR(dsn = paste0(fire_directory,"/shpfile"))), silent = FALSE)
    # import dNBR raster file
    dnbr_file <- list.files(paste0(fire_directory,"/dnbr"))
    try(assign(paste0(name,"_dnbr"), raster::raster(paste0(fire_directory, "/dnbr/", dnbr_file))))
    }
  }
}

## Check to make sure that projections are the same

identical(crs(wbp_range), crs(WY_1988_wy4408310714319880730_dnbr))
identical(crs(wbp_range), crs(ID_1996_id4245511223519960809_dnbr))

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
#    and low severity fire.

Pattern <- grep(".dnbr",names(.GlobalEnv),value=TRUE)
length(Pattern)
sum(duplicated(Pattern))

high <- NULL
moderate <- NULL
low <- NULL
ID <- NULL

for(i in Pattern){
  cat(paste0("Running iteration ", which(Pattern == i), " of ",length(Pattern) ,"..."))
  fire <- mget(i)[[1]] # get the raster object that corresponds to pattern[i] character string
  boundary <- mget(gsub("_dnbr", "_boundary", i))[[1]]
  new_projection <- try(new_projection <- projectRaster(gye_wbp, fire, res = 30), silent = FALSE)
  if(isTRUE(class(new_projection) == "try-error")){ 
    high[which(Pattern == i)] <- NA
    moderate[which(Pattern == i)] <- NA
    low[which(Pattern == i)] <- NA
    ID[which(Pattern == i)] <- Pattern[i]
    cat(paste0("DONE.\n")) 
  } else if(isTRUE(class(new_projection) != "try-error")){
    # new_projection <- projectRaster(gye_wbp, fire)
    # cropped_range <- crop(new_projection, fire)
    masked_fire <- mask(fire, new_projection)
    masked_fire[masked_fire < 195] <- NA  # Of the areas that burned, what proportion were low, moderate, severe. Not including unburned area within the perimeter
    values_within_boundary <- extract(masked_fire, boundary)[[1]] # extract dnbr values from pixels that lie within the fire perimeter
    prop_high <- sum(values_within_boundary > 556, na.rm = T)/sum(!is.na(values_within_boundary))
    prop_moderate <- sum(values_within_boundary >= 276 & values_within_boundary < 556, na.rm = T)/sum(!is.na(values_within_boundary))
    prop_low <- sum(values_within_boundary >= 195 & values_within_boundary < 276, na.rm = T)/sum(!is.na(values_within_boundary))
    # prop_unchanged <- sum(values(masked_fire)< 69, na.rm = T)/sum(!is.na(values(masked_fire)))
    high[which(Pattern == i)] <- prop_high
    moderate[which(Pattern == i)] <- prop_moderate
    low[which(Pattern == i)] <- prop_low
    ID[which(Pattern == i)] <- Pattern[i]
    cat(paste0("DONE.\n"))
  }
}




dat <- matrix(c(high, moderate, low), ncol = 3, byrow = F)
total <- apply(dat, MARGIN = 1, FUN = sum)
range(total, na.rm = T)

dnbr_dat <- data.frame(id = Pattern, high = high, moderate = moderate, low = low) %>%  
  gather(., severity, prop, -id)

save(dnbr_dat, file = paste0(proj_dir, data_dir,"dnbr_data.Rds"))
load(paste0(proj_dir, data_dir,"dnbr_data.Rds"))

dnbr_dat %>% 
  # filter(., severity == "high")%>% 
  ggplot(aes(x = prop, col = severity, fill = severity))+
  geom_density(alpha = 0.2, bins = 40)#+
# facet_wrap(~severity, ncol = 1)

noNAdat_dnbr <- na.omit(dnbr_dat)

(summary_dnbr <- noNAdat_dnbr %>% 
    group_by(severity) %>% 
    summarise_at(., vars(prop), funs(mean, median)))

#---------------------------------------|---------------------------------------
#                          Entire Rocky Mountain Region
#---------------------------------------|---------------------------------------

Pattern <- grep(".tif",names(.GlobalEnv),value=TRUE)
length(Pattern)
sum(duplicated(Pattern))

high_range <- NULL
moderate_range <- NULL
low_range <- NULL
ID <- NULL

for(i in Pattern){
  cat(paste0("Running iteration ", which(Pattern == i), " of ",length(Pattern) ,"..."))
  fire <- mget(i)[[1]] # get the raster object that corresponds to pattern[i] character string
  fire[fire == 0 | fire == 1 | fire == 5 |fire == 6] <- NA # set to NA values that are 0 (NA), 1 (unburned/low), or 6 (increased greenness)
  fire <- as.factor(fire)
  new_projection <- projectRaster(wbp_range, fire)
  new_projection <- as.factor(new_projection)
  # cropped_range <- crop(new_projection, fire)
  masked_fire <- mask(fire, new_projection)
  prop_high <- sum(values(masked_fire) == "4", na.rm = T)/sum(!is.na(values(masked_fire)))
  prop_moderate <- sum(values(masked_fire) == "3", na.rm = T)/sum(!is.na(values(masked_fire)))
  prop_low <- sum(values(masked_fire) == "2", na.rm = T)/sum(!is.na(values(masked_fire)))
  high_range[which(Pattern == i)] <- prop_high
  moderate_range[which(Pattern == i)] <- prop_moderate
  low_range[which(Pattern == i)] <- prop_low
  ID[which(Pattern == i)] <- Pattern[i]
  cat(paste0("DONE.\n"))
}

dat_range <- matrix(c(high_range, moderate_range, low_range), ncol = 3, byrow = F)
total_range <- apply(dat_range, MARGIN = 1, FUN = sum)
range(total_range, na.rm = T)

dat_range <- data.frame(id = Pattern, high = high_range, moderate = moderate_range,
                        low = low_range) %>%  
  gather(., severity, prop, -id)

dat_range %>% 
  # filter(., severity == "high")%>% 
  ggplot(aes(x = prop, col = severity, fill = severity))+
  geom_density(alpha = 0.2, bins = 40) #+
# facet_wrap(~severity, ncol = 1)

noNAdat_range <- na.omit(dat_range)

noNAdat_range %>% 
  group_by(severity) %>% 
  summarise_at(., vars(prop), funs(mean, median))


#---------------------------------------|---------------------------------------
#       Determine the parameters that define the multinomial distribution
#---------------------------------------|---------------------------------------


