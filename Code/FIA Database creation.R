library(RPostgreSQL)
library(rpostgis)
library(postGIStools)
library(tidyverse)
library(sp)
# library(sf)
library(ggmap)
library(readxl)
library(lubridate)

options(scipen = 999)

##------------------------
## SET UP DATABASE
##------------------------
# 
# # Intially create DB. Only needs to be done 1st time. Creates db called "FIADB"
# system(paste0("createdb FIADB"))
# 
# #connect to database - locally hosted
# fia_db_conn <- DBI::dbConnect("PostgreSQL", dbname = 'FIADB', host = 'localhost', port = '5432')
# 
# #check connection - list tables in db
# dbListTables(fia_db_conn)
# 
# dbDisconnect(conn = fia_db_conn)
# 
# #read in Wyoming FIA data
# 
# file_list <- list.files(path = "/Users/elizabethpansing/Box/PhD/WY FIA Data/WY/", pattern = "*.csv")
# 
# 
# for(i in 1:length(file_list)){
#   print(paste0("Reading file ", i, " of ", length(file_list)))
#   name <- gsub(pattern =  '.csv*', '', file_list[i])
#   try(assign(name, read.csv(paste0("/Users/elizabethpansing/Box/PhD/WY FIA Data/WY/",file_list[i]))), silent = FALSE)
# }
# 
# # From AP's og script: "Insert the tables into the database. In this case if the table already exists it
# # overwrites it, which is not a good idea for a safe workflow as you wouldn't want to change the DB completely
# # everytime you added in data, I only do this here because my structure is not determined yet and
# # I keep changing the structure have backups of the original dta. In the future this command can
# # be easily changed to only update/append to an existing table for a safer workflow."
# 
# fia_db_conn <- DBI::dbConnect("PostgreSQL", dbname = 'FIADB', host = 'localhost', port = '5432')
# 
# wyfia_pattern <- grep("WY_",names(.GlobalEnv),value = TRUE)
# 
# for(i in wyfia_pattern){
#   if(nrow(get(i))> 0){
#     x <- get(i)
#   names(x) <- tolower(names(x))
#   DBI::dbWriteTable(conn = fia_db_conn, name = tolower(i), value = x)
#   }
# }
# 
# # check to make sure they uploaded properly
# dbListTables(fia_db_conn)
# 
# # assign primary key. All PKs are the CN variable, so write a loop to assign it to each
# # table with a new PK title.
# 
# tables <- data.frame(table = DBI::dbListTables(conn = fia_db_conn),
#                      pk = tolower(c("DVT_PK",
#                                     "SDL_PK",
#                                     "PEG_PK",
#                                     "PEU_PK",
#                                     "PSM_PK",
#                                     "PEV_PK",
#                                     "SIT_PK",
#                                     "TGE_PK",
#                                     "CTY_PK",
#                                     "PPSA_PK",
#                                     NA,
#                                     "DDL_PK",
#                                     "PLOTGEOM_PK",
#                                     NA,
#                                     NA,
#                                     "PLOTSNP_PK",
#                                     "SCD_PK",
#                                     "BND_PK",
#                                     "TRE_GRM_MIDPT_PK", 
#                                     "PEA_PK",
#                                     "CDC_PK",
#                                     "PET_PK",
#                                     "CND_PK",
#                                     "PLT_PK",
#                                     "TRE_PK",
#                                     "P2VSSP_PK",
#                                     "SBP_PK",
#                                     "SRV_PK",
#                                     "DRP_PK",
#                                     "TRB_PK",
#                                     "ISS_PK",
#                                     "P2VSS_PK",
#                                     "DFW_PK",
#                                     "DCW_PK",
#                                     "DTS_PK",
#                                     "TRE_GRM_CMP_PK")))
# 
# tables <- na.omit(tables)
# 
# DBI::dbDisconnect(fia_db_conn)
# 
# 
# fia_db_conn <- DBI::dbConnect("PostgreSQL", dbname = 'FIADB', host = 'localhost', port = '5432')
# 
# # set primary key for the
# for(i in tables$table){
#   x <- tables[tables$table == i,]
# try(dbGetQuery(conn = fia_db_conn, paste0('ALTER TABLE ',x$table,' ADD CONSTRAINT ', x$pk,' PRIMARY KEY ("cn");')))
# }
# 
# ## Doesn't work for those with multiple entries for the primary key (CN)
# 
# # out <- dbListTables(conn = fia_db_conn)
# # for(i in out){
# #   dbRemoveTable(conn = fia_db_conn, name = i)
# # }
# # 
# # dbListTables(conn = fia_db_conn)
# 
# DBI::dbDisconnect(fia_db_conn)

#--------------------------------
# Merge manual version into TRE
#--------------------------------

fia_db_conn <- DBI::dbConnect("PostgreSQL", dbname = 'FIADB', host = 'localhost', port = '5432')

query <- paste0("SELECT * ", 
                "FROM wy_tree ",
                "LEFT JOIN wy_plot ON wy_tree.plt_cn = wy_plot.cn ",
                "WHERE spcd = '101'")

##_________________________________________
##      THIS IS THE GOOD ONE
##_________________________________________

query1 <- paste0("SELECT * ", 
                "FROM wy_tree AS tree ",
                "LEFT JOIN (SELECT cn, manual_db, manual_rmrs FROM wy_plot) AS plot ON tree.plt_cn = plot.cn ",
                "WHERE spcd = '101'")

dat <- dbGetQuery(conn = fia_db_conn, statement = query1)




#-----------------------
# LOCATION LEVEL
#-----------------------
# 2.1 SURVEY TABLE
#   primary key = CN (SRV_PK)
#   unique key  = STATECD, UNITCD, COUNTYCD (SRV_UK)
#
# 2.2 COUNTY TABLE
#   primary = CN (CTY_PK)
#   unique = STATECD, UNITCD, COUNTYCD (CTY_UK)
#
# 2.3 PLOT TABLE
#   primary = CN  (PLT_PK)
#   unique  = STATECD, INVYR, UNITCD, COUNTYCD, PLOT  (PLT_UK)
#   foreign = CTY_CN (PLOT to COUNTY) (PLT_CTY_FK)
#   foreign = SRV_CN (PLOT to SURVEY)  (PLT_SRV_FK)
#
# 2.4 CONDITION TABLE
#   primary = CN  (CND_PK)
#   unique  = PLT_CN, CONDID (CND_UK) 
#   natural = STATECD, INVYR, UNITCD, COUNTYCD, PLOT, CONDID  (CND_NAT_I)
#   foreign = PLT_CN (CONDITION to PLOT) (CND_PLT_FK)
#
#  2.5 SUBPLOT TABLE
#   primary = CN (SBP_PK)
#   unique  = PLT_CN, SUBP (SBP_UK)
#   natural = STATECD, INVYR, UNITCD, COUNTYCD, PLOT, SUBP  (SBP_NAT_I)
#   foreign = PLT_CN, MICRCOND (SUBPLOT to COND) (SBP_CND_FK2)
#   foreign = PLT_CN, MACRCOND (SUBPLOT to COND) (SBP_CND_FK3)
#   foreign = PLT_CN, SUBPCOND (SUBPLOT to COND) (SBP_CND_FK)
#   foreign = PLT_CN (SUBPLOT to PLOT) (SBP_PLT_FK)
#
# 2.6 SUBPLOT CONDITION TABLE
#   primary = CN (SCD_PK)
#   unique  = PLT_CN, SUBP, CONDID (SCD_UK)
#   natural = STATECD, INVYR, UNITCD, COUNTYCD, PLOT, SUBP, CONDID (SCD_NAT_I)
#   foreign = PLT_CN, CONDID (SUBP_COND to COND) (SCD_CND_FK)
#   foreign = PLT_CN (SUBPCOND to PLOT) (SCD_PLT_FK)
#   foreign = PLT_CN, SUBP (SUBP_COND to SUBPLOT) (SCD_SBP_FK)
#
# 2.7 BOUNDARY TABLE
#   primary = CN (BND_PK)
#   unique  = PLT_CN, SUBP, SUBTYP, AZMLEFT, AZMRIGHT (BND_UK)
#   natural = STATECD, INVYR, UNITCD, COUNTYCD, PLOT, SUBP, SUBTYP, AZMLEFT, AZMRIGHT (BND_NAT_I)
#   foreign = PLT_CN (BOUNDARY to PLOT) (BND_PLT_FK)
#
# 2.8 SUBPLOT CONDITION CHANGE MATRIX
#   primary = CN (CMX_PK)
#   unique  = PLT_CN, PREV_PLT_CN, SUBP, SUBTYP, CONDID, PREVCOND (CMX_UK)
#   foreign = PREV_PLT_CN (SUBP_COND_CHNG_MTRX to PLOT) (CMX_PLT_FK)
#   foreign = SUBP_COND_CHNG_MTRX to PLOT (CMX_PLT_FK2)
#
#-----------------------
# TREE LEVEL
#-----------------------
# 3.1 TREE TABLE
#   primary = CN (TRE_PK)
#   unique  = PLT_CN, SUBP, TREE (TRE_UK)
#   foreign = STATECD, INVYR, UNITCD, COUNTYCD, PLOT, SUBP, TREE (TRE_NAT_I)
#   foreign = PLT_CN (TREE to PLOT) (TRE_PLT_FK)
#
# 3.2 


# WBP species code = 101




# keys <- data.frame(table   = c(rep("SURVEY", 4),
#                                rep("COUNTY", 3),
#                                rep("PLOT", 5 * 5),
#                                rep("CONDITION", 6 * 2)
#                                ),
#                    primary = c(rep("CN", 4),
#                                rep("CN", 3),
#                                rep("CN", 5 * 5),
#                                rep("CN", 6 * 2)
#                                ),
#                    natural = c(rep("NA", 4),
#                                rep("NA", 3),
#                                rep("NA", 5 * 5),
#                                rep(c("STATECD", "INVYR", "UNITCD", "COUNTYCD", "PLOT", "CONDID", 2))
#                                ),
#                    unique  = c("STATECD", "INVYR", "P3_OZONE_IND", "CYCLE",
#                               "STATECD", "UNITCD", 'COUNTYCD',
#                               rep(c("STATECD", "INVYR", "UNITCD", "COUNTYCD", "PLOT"), 5),
#                               rep(c(""))),
#                    foreign = c(rep("NA",4), 
#                                rep("NA", 3),
#                                rep(c("CTY_CN", "SRV_CN"), each = 5),
#                                )

#Set primary keys
dbGetQuery(conn, 'ALTER WY_SURVEY ADD CONSTRAINT TRE_pk PRIMARY KEY ("CN");') 
dbGetQuery(conn, 'ALTER WY_SURVEY ADD CONSTRAINT WY_SURVEY_pk PRIMARY KEY ("CN");') 


# pgInsert(conn, name=c("public","nests"), data.obj = nests, geog = T, overwrite = T)

# From AP's og script: "This command will set the column that serves as the primary identifier for your units. 
# These are the keys you can refer to when querying data and linking tables in a relational way."
dbAddKey(conn, name = c("public","nests"), colname = "nestid", type = "primary")


# DBI::dbDisconnect(conn = conn) # disconnects from the database after queries (max 14) disconnect after each query

###############################################################################################################
# USING TREE TABLE ONLY GIVEN KNOWN SIZE OF PLOTS AND SUBPLOTS
###############################################################################################################

rm(list = setdiff(ls(), "WY_TREE"))





