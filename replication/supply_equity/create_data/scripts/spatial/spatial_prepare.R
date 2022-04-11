########################################################
# This script processes our original dataset of 
# permits collected from city council minutes
# It geocodes these permits and aggregates them
# to the block group level
# Then it merges on Census covariates, imputes missing
# values, and creates some variables for final analysis
# Output: Block group by year dataset of permits, 
# treatment, and covariates 
########################################################

rm(list = ls())

set.seed(123)

# install libraries
#install.packages("ggmap")
#install.packages("ggplot2")
#install.packages("rworldmap")
#install.packages("rgdal")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("Amelia")

# load libraries
library(ggmap)
library(ggplot2)
library(rworldmap)
library(rgdal)
library(plyr); library(dplyr)
library(Amelia)

# set working directory 
setwd("") # set to create_data

# source geocoding script
source("scripts/spatial/getGeoDetails.R")

########################################
# read in data sets and combine
########################################

anaheim <- read.csv("raw/spatialPermits/cvraPermits - anaheimFinal.csv")
sb <- read.csv("raw/spatialPermits/cvraPermits - santaBarbaraFinal.csv")
sc <- read.csv("raw/spatialPermits/cvraPermits - santaCruzFinal.csv")
escondido <- read.csv("raw/spatialPermits/cvraPermits - escondidoFinal.csv")
glendale <- read.csv("raw/spatialPermits/cvraPermits - glendaleFinal.csv")
ventura <- read.csv("raw/spatialPermits/cvraPermits - venturaFinal.csv")

keepvars <- c("city", "application", "address", "zoning", "totalUnits", "singleFamily", "multiFamily", "totalUnitsNet",
              "singleFamilyNet", "multiFamilyNet", "affordability", "planningVote", "planningApprove", "councilVote", 
              "councilApprove", "finalApproval", "year", "redesign")

anaheim <- anaheim[,keepvars]
sb <- sb[,keepvars]
sc <- sc[,keepvars]
escondido <- escondido[,keepvars]
glendale <- glendale[,keepvars]
ventura <- ventura[,keepvars]

all <- rbind(anaheim, sb, sc, escondido, glendale, ventura)
addresses <- paste0(all$address, ", ", all$city, ", California")

################################################################################################
# geocode data
# this process has been run and saved into the location: output/temp/all_permits_geocoded.csv
################################################################################################

# register your google key here 
# register_google(key = "", write = TRUE)

# initialise a dataframe to hold the results
# infile <- "data" 
# geocoded <- data.frame()
# # find out where to start in the address list (if the script was interrupted before):
# startindex <- 1
# #if a temp file exists - load it up and count the rows!
# tempfilename <- paste0(infile, '_temp_geocoded.rds')
# if (file.exists(tempfilename)){
#   print("Found temp file - resuming from index:")
#   geocoded <- readRDS(tempfilename)
#   startindex <- nrow(geocoded) + 1
#   print(startindex)
# }
# 
# # Start the geocoding process address by address. geocode() function takes care of query speed limit.
# for (ii in seq(startindex, length(addresses))){
#   print(paste("Working on index", ii, "of", length(addresses)))
#   #query the google geocoder - this will pause here if we are over the limit.
#   result = getGeoDetails(address = addresses[ii]) 
#   print(result$status)     
#   result$index <- ii
#   #append the answer to the results file.
#   geocoded <- rbind(geocoded, result)
#   #save temporary results as we are going along
#   saveRDS(geocoded, tempfilename)
# }
# 
# # a few checks and save
# mean(substr(all$address, 1, 3)==substr(geocoded$formatted_address, 1, 3), na.rm=TRUE) # yes - 98% 
# all$address[substr(all$address, 1, 3)!=substr(geocoded$formatted_address, 1, 3)] 
# 
# all$lat <- geocoded$lat
# all$long <- geocoded$long
# all$geocoded_address <- geocoded$formatted_address
#write.csv(all, file = "output/temp/all_permits_geocoded.csv", row.names = FALSE)
all <- read.csv("output/temp/all_permits_geocoded.csv")

########################################
# aggregate to the block group level
########################################

bg <- readOGR(dsn = "raw/shapefiles/tl_2015_06_bg/")

# get coordinates
coords <- all[,c("long", "lat")]
sp <- SpatialPoints(coords)

# change projections to match
proj4string(sp) <- proj4string(bg)

# code tract of coordinates
by_bg <- over(sp, bg)

# merge with original data
all$tract <- by_bg$TRACTCE
all$bg <- by_bg$BLKGRPCE
all$county <- by_bg$COUNTYFP
all$geoid <- by_bg$GEOID

# aggregate observations to block group/year level 
# base dataset
geoids <- unique(all[,c("geoid", "city")])
base <- data.frame(geoid = rep(geoids$geoid, 10),
                   city = rep(geoids$city, 10),
                   year = rep(c(2009:2018), each = nrow(geoids)))
base <- base[order(base$geoid, base$year),]

# aggregate
agg <- all %>% 
  group_by(geoid, year) %>% 
  summarise(nbuilds = n(),
            nunits_single = sum(singleFamily[zoning!=1]),
            nunits_multi = sum(multiFamily[zoning!=1]),
            nunits_total = sum(totalUnits[zoning!=1]),
            nunits_single_z = sum(singleFamily), # keep zonings
            nunits_multi_z = sum(multiFamily),
            nunits_total_z = sum(totalUnits))

agg <- merge(base, agg, by = c("geoid", "year"), all.x = TRUE)
agg$nbuilds[is.na(agg$nbuilds)] <- 0
agg$nunits_single[is.na(agg$nunits_single)] <- 0
agg$nunits_multi[is.na(agg$nunits_multi)] <- 0
agg$nunits_total[is.na(agg$nunits_total)] <- 0
agg$nunits_single_z[is.na(agg$nunits_single_z)] <- 0
agg$nunits_multi_z[is.na(agg$nunits_multi_z)] <- 0
agg$nunits_total_z[is.na(agg$nunits_total_z)] <- 0

# drop if missing a geoid 
agg <- agg[!is.na(agg$geoid),]

##############################################
# get statistics for block group from census 
##############################################

# function to subset and transform census data
cleanfun <- function(df, year, keepvars, names) {
  df <- df[-1, keepvars]
  names(df) <- names
  for (i in 2:ncol(df)) {
    df[,i] <- as.numeric(as.character(df[,i]))
  }
  df$year <- year
  return(df)
}

###################### INCOME DATA ########################

# read income data
df13 <- read.csv("raw/census/income/bg/ACS_13_5YR_B19013/ACS_13_5YR_B19013_with_ann.csv")
df14 <- read.csv("raw/census/income/bg/ACS_14_5YR_B19013/ACS_14_5YR_B19013_with_ann.csv")
df15 <- read.csv("raw/census/income/bg/ACS_15_5YR_B19013/ACS_15_5YR_B19013_with_ann.csv")
df16 <- read.csv("raw/census/income/bg/ACS_16_5YR_B19013/ACS_16_5YR_B19013_with_ann.csv")
df17 <- read.csv("raw/census/income/bg/ACS_17_5YR_B19013/ACS_17_5YR_B19013_with_ann.csv")

# clean & transform 
keepvars <- c("GEO.id2", # id
              "HD01_VD01") # median household income

names <- c("GEO_id", "med_income")

# throughout, will assign 2013 data to 2011 and 2012
out11 <- cleanfun(df = df13, year = 2011, keepvars = keepvars, names = names)
out12 <- cleanfun(df = df13, year = 2012, keepvars = keepvars, names = names)
out13 <- cleanfun(df = df13, year = 2013, keepvars = keepvars, names = names)
out14 <- cleanfun(df = df14, year = 2014, keepvars = keepvars, names = names)
out15 <- cleanfun(df = df15, year = 2015, keepvars = keepvars, names = names)
out16 <- cleanfun(df = df16, year = 2016, keepvars = keepvars, names = names)
out17 <- cleanfun(df = df17, year = 2017, keepvars = keepvars, names = names)

# stack 
income <- rbind(out11, out12, out13, out14, out15, out16, out17)

###################### ETHNICITY ########################

# read white nonlatino data
df13 <- read.csv("raw/census/race/bg/ACS_13_5YR_B11001H/ACS_13_5YR_B11001H_with_ann.csv")
df14 <- read.csv("raw/census/race/bg/ACS_14_5YR_B11001H/ACS_14_5YR_B11001H_with_ann.csv")
df15 <- read.csv("raw/census/race/bg/ACS_15_5YR_B11001H/ACS_15_5YR_B11001H_with_ann.csv")
df16 <- read.csv("raw/census/race/bg/ACS_16_5YR_B11001H/ACS_16_5YR_B11001H_with_ann.csv")
df17 <- read.csv("raw/census/race/bg/ACS_17_5YR_B11001H/ACS_17_5YR_B11001H_with_ann.csv")

# clean & transform 
keepvars <- c("GEO.id2", # id
              "HD01_VD01") # total number of households

names <- c("GEO_id", "n_households_nlwhite")

out11 <- cleanfun(df = df13, year = 2011, keepvars = keepvars, names = names)
out12 <- cleanfun(df = df13, year = 2012, keepvars = keepvars, names = names)
out13 <- cleanfun(df = df13, year = 2013, keepvars = keepvars, names = names)
out14 <- cleanfun(df = df14, year = 2014, keepvars = keepvars, names = names)
out15 <- cleanfun(df = df15, year = 2015, keepvars = keepvars, names = names)
out16 <- cleanfun(df = df16, year = 2016, keepvars = keepvars, names = names)
out17 <- cleanfun(df = df17, year = 2017, keepvars = keepvars, names = names)

# stack 
households_nlwhite <- rbind(out11, out12, out13, out14, out15, out16, out17)

# read latino data
df13 <- read.csv("raw/census/race/bg/latino/ACS_13_5YR_B11001I_with_ann.csv")
df14 <- read.csv("raw/census/race/bg/latino/ACS_13_5YR_B11001I_with_ann.csv")
df15 <- read.csv("raw/census/race/bg/latino/ACS_13_5YR_B11001I_with_ann.csv")
df16 <- read.csv("raw/census/race/bg/latino/ACS_13_5YR_B11001I_with_ann.csv")
df17 <- read.csv("raw/census/race/bg/latino/ACS_13_5YR_B11001I_with_ann.csv")

# clean & transform 
keepvars <- c("GEO.id2", # id
              "HD01_VD01") # total number of households

names <- c("GEO_id", "n_households_latino")

out11 <- cleanfun(df = df13, year = 2011, keepvars = keepvars, names = names)
out12 <- cleanfun(df = df13, year = 2012, keepvars = keepvars, names = names)
out13 <- cleanfun(df = df13, year = 2013, keepvars = keepvars, names = names)
out14 <- cleanfun(df = df14, year = 2014, keepvars = keepvars, names = names)
out15 <- cleanfun(df = df15, year = 2015, keepvars = keepvars, names = names)
out16 <- cleanfun(df = df16, year = 2016, keepvars = keepvars, names = names)
out17 <- cleanfun(df = df17, year = 2017, keepvars = keepvars, names = names)

# stack 
households_latino <- rbind(out11, out12, out13, out14, out15, out16, out17)

# read ethnicities data
df13 <- read.csv("raw/census/race/bg/ACS_13_5YR_B25006/ACS_13_5YR_B25006_with_ann.csv")
df14 <- read.csv("raw/census/race/bg/ACS_14_5YR_B25006/ACS_14_5YR_B25006_with_ann.csv")
df15 <- read.csv("raw/census/race/bg/ACS_15_5YR_B25006/ACS_15_5YR_B25006_with_ann.csv")
df16 <- read.csv("raw/census/race/bg/ACS_16_5YR_B25006/ACS_16_5YR_B25006_with_ann.csv")
df17 <- read.csv("raw/census/race/bg/ACS_17_5YR_B25006/ACS_17_5YR_B25006_with_ann.csv")

# clean & transform 
keepvars <- c("GEO.id2", # id
              "HD01_VD01", # total number of households
              "HD01_VD02", # white alone
              "HD01_VD03", # black alone
              "HD01_VD05") # asian alone

names <- c("GEO_id", "n_households", "nhouseholds_white", 
           "nhouseholds_black", "nhouseholds_asian")

out11 <- cleanfun(df = df13, year = 2011, keepvars = keepvars, names = names)
out12 <- cleanfun(df = df13, year = 2012, keepvars = keepvars, names = names)
out13 <- cleanfun(df = df13, year = 2013, keepvars = keepvars, names = names)
out14 <- cleanfun(df = df14, year = 2014, keepvars = keepvars, names = names)
out15 <- cleanfun(df = df15, year = 2015, keepvars = keepvars, names = names)
out16 <- cleanfun(df = df16, year = 2016, keepvars = keepvars, names = names)
out17 <- cleanfun(df = df17, year = 2017, keepvars = keepvars, names = names)

# stack 
households <- rbind(out11, out12, out13, out14, out15, out16, out17)

###################### OCCUPANCY ########################

df13 <- read.csv("raw/census/occupancy/bg/ACS_13_5YR_B25002/ACS_13_5YR_B25002_with_ann.csv")
df14 <- read.csv("raw/census/occupancy/bg/ACS_14_5YR_B25002/ACS_14_5YR_B25002_with_ann.csv")
df15 <- read.csv("raw/census/occupancy/bg/ACS_15_5YR_B25002/ACS_15_5YR_B25002_with_ann.csv")
df16 <- read.csv("raw/census/occupancy/bg/ACS_16_5YR_B25002/ACS_16_5YR_B25002_with_ann.csv")
df17 <- read.csv("raw/census/occupancy/bg/ACS_17_5YR_B25002/ACS_17_5YR_B25002_with_ann.csv")

# clean & transform 
keepvars <- c("GEO.id2", # id
              "HD01_VD01", # total 
              "HD01_VD02", # occupied
              "HD01_VD03") # vacant

names <- c("GEO_id", "total_occ", "occupied", "vacant")

out11 <- cleanfun(df = df13, year = 2011, keepvars = keepvars, names = names)
out12 <- cleanfun(df = df13, year = 2012, keepvars = keepvars, names = names)
out13 <- cleanfun(df = df13, year = 2013, keepvars = keepvars, names = names)
out14 <- cleanfun(df = df14, year = 2014, keepvars = keepvars, names = names)
out15 <- cleanfun(df = df15, year = 2015, keepvars = keepvars, names = names)
out16 <- cleanfun(df = df16, year = 2016, keepvars = keepvars, names = names)
out17 <- cleanfun(df = df17, year = 2017, keepvars = keepvars, names = names)

# stack 
occupancy <- rbind(out11, out12, out13, out14, out15, out16, out17)

###################### OWNER ########################

df13 <- read.csv("raw/census/owner/bg/ACS_13_5YR_B25008/ACS_13_5YR_B25008_with_ann.csv")
df14 <- read.csv("raw/census/owner/bg/ACS_14_5YR_B25008/ACS_14_5YR_B25008_with_ann.csv")
df15 <- read.csv("raw/census/owner/bg/ACS_15_5YR_B25008/ACS_15_5YR_B25008_with_ann.csv")
df16 <- read.csv("raw/census/owner/bg/ACS_16_5YR_B25008/ACS_16_5YR_B25008_with_ann.csv")
df17 <- read.csv("raw/census/owner/bg/ACS_17_5YR_B25008/ACS_17_5YR_B25008_with_ann.csv")

# clean & transform 
keepvars <- c("GEO.id2", # id
              "HD01_VD01", # total 
              "HD01_VD02", # owner occupied
              "HD01_VD03") # renter occupied

names <- c("GEO_id", "total_own", "owner", "renter")

out11 <- cleanfun(df = df13, year = 2011, keepvars = keepvars, names = names)
out12 <- cleanfun(df = df13, year = 2012, keepvars = keepvars, names = names)
out13 <- cleanfun(df = df13, year = 2013, keepvars = keepvars, names = names)
out14 <- cleanfun(df = df14, year = 2014, keepvars = keepvars, names = names)
out15 <- cleanfun(df = df15, year = 2015, keepvars = keepvars, names = names)
out16 <- cleanfun(df = df16, year = 2016, keepvars = keepvars, names = names)
out17 <- cleanfun(df = df17, year = 2017, keepvars = keepvars, names = names)

# stack 
owner <- rbind(out11, out12, out13, out14, out15, out16, out17)


###################### HOME PRICE ########################

df13 <- read.csv("raw/census/value/bg/ACS_13_5YR_B25077/ACS_13_5YR_B25077_with_ann.csv")
df14 <- read.csv("raw/census/value/bg/ACS_14_5YR_B25077/ACS_14_5YR_B25077_with_ann.csv")
df15 <- read.csv("raw/census/value/bg/ACS_15_5YR_B25077/ACS_15_5YR_B25077_with_ann.csv")
df16 <- read.csv("raw/census/value/bg/ACS_16_5YR_B25077/ACS_16_5YR_B25077_with_ann.csv")
df17 <- read.csv("raw/census/value/bg/ACS_17_5YR_B25077/ACS_17_5YR_B25077_with_ann.csv")

# clean & transform 
keepvars <- c("GEO.id2", # id
              "HD01_VD01") # median home value

names <- c("GEO_id", "med_value")

out11 <- cleanfun(df = df13, year = 2011, keepvars = keepvars, names = names)
out12 <- cleanfun(df = df13, year = 2012, keepvars = keepvars, names = names)
out13 <- cleanfun(df = df13, year = 2013, keepvars = keepvars, names = names)
out14 <- cleanfun(df = df14, year = 2014, keepvars = keepvars, names = names)
out15 <- cleanfun(df = df15, year = 2015, keepvars = keepvars, names = names)
out16 <- cleanfun(df = df16, year = 2016, keepvars = keepvars, names = names)
out17 <- cleanfun(df = df17, year = 2017, keepvars = keepvars, names = names)

# stack 
value <- rbind(out11, out12, out13, out14, out15, out16, out17)

###################### TOTAL PERMITS BY CITY ########################

city <- agg %>% 
  group_by(city, year) %>% 
  summarise(nunits_multi_city = sum(nunits_multi),
            nunits_total_city = sum(nunits_total),
            nunits_multi_city_z = sum(nunits_multi_z),
            nunits_total_city_z = sum(nunits_total_z))

###################### MERGE ALL ########################

m1 <- merge(households, households_nlwhite, by = c("GEO_id", "year"), all.x = TRUE)
m2 <- merge(m1, households_latino, by = c("GEO_id", "year"), all.x = TRUE)
m3 <- merge(m2, income, by = c("GEO_id", "year"), all.x = TRUE)
m4 <- merge(m3, occupancy, by = c("GEO_id", "year"), all.x = TRUE)
m5 <- merge(m4, value, by = c("GEO_id", "year"), all.x = TRUE)
m6 <- merge(m5, owner, by = c("GEO_id", "year"), all.x = TRUE)

# impute missing values for 2018 using three-year averages 
sub1517 <- m6[m6$year %in% c(2015:2017),]
df18 <- sub1517 %>%
  group_by(GEO_id) %>%
  dplyr::summarise(n_households = mean(n_households, na.rm=TRUE),
                   nhouseholds_white = mean(nhouseholds_white, na.rm=TRUE),
                   nhouseholds_black = mean(nhouseholds_black, na.rm=TRUE),
                   nhouseholds_asian = mean(nhouseholds_asian, na.rm=TRUE),
                   n_households_nlwhite = mean(n_households_nlwhite, na.rm=TRUE),
                   n_households_latino = mean(n_households_latino, na.rm=TRUE),
                   med_income = mean(med_income, na.rm = TRUE),
                   total_occ = mean(total_occ, na.rm=TRUE),
                   occupied = mean(occupied, na.rm=TRUE),
                   vacant = mean(vacant, na.rm=TRUE),
                   med_value = mean(med_value, na.rm=TRUE),
                   total_own = mean(total_own, na.rm=TRUE),
                   owner = mean(owner, na.rm=TRUE),
                   renter = mean(renter, na.rm=TRUE))
df18$year <- 2018
m6 <- rbind(m6, df18)

# merge with geocoded data
names(m6)[1] <- "geoid"
m7 <- merge(agg, m6, by = c("geoid", "year"), all.x = TRUE)

# merge with city data
final <- merge(m7, city, by = c("city", "year"), all.x = TRUE)

# code variables
final$own_rate <- final$owner/final$total_own
final$vacancy_rate <- final$vacant/final$total_occ
final$pct_white <- final$nhouseholds_white/final$n_households
final$pct_black <- final$nhouseholds_black/final$n_households
final$pct_hisp <- final$n_households_latino/final$n_households
final$pct_nlw <- final$n_households_nlwhite/final$n_households

# code DVs
final$lnunits_total <- log(final$nunits_total + 1)
final$lnunits_multi <- log(final$nunits_multi + 1)
final$propcity_total <- final$nunits_total/final$nunits_total_city
final$propcity_multi <- final$nunits_multi/final$nunits_multi_city
final$propcity_total_z <- final$nunits_total_z/final$nunits_total_city_z
final$propcity_multi_z <- final$nunits_multi_z/final$nunits_multi_city_z

# code treatment status 
final$treat <- 0
final$treat[final$year>=2015 & final$city=="Santa Barbara"] <- 1
final$treat[final$year>=2016 & final$city=="Anaheim"] <- 1
final$treat[final$year>=2014 & final$city=="Escondido"] <- 1
final$treat[final$year>=2018 & final$city=="Ventura"] <- 1

# subset to 2011 and forward
sub <- subset(final, year>2010)

# impute missing observations  
df <- sub[,names(sub) %in% c("nunits_multi", "nunits_single", "nunits_total", "nunits_multi_z", "nunits_single_z", "nunits_total_z",
                             "propcity_total_z", "propcity_multi_z", "propcity_total", "propcity_multi", 
                             "treat", "city", "geoid", "year", "n_households", "med_income", "med_value", 
                             "vacancy_rate", "own_rate", "pct_black", "pct_hisp", "pct_nlw")]
imp <- amelia(df, m = 5, idvars = c("city", "year", "geoid", "nunits_multi", "nunits_single", "nunits_total",
                                    "propcity_total_z", "propcity_multi_z", "propcity_total", "propcity_multi",
                                    "nunits_multi_z", "nunits_single_z", "nunits_total_z"))

df$med_income_imp <- rowMeans(data.frame(imp1 = imp$imputations$imp1$med_income, imp2 = imp$imputations$imp2$med_income,
                                         imp3 = imp$imputations$imp3$med_income, imp4 = imp$imputations$imp4$med_income, 
                                         imp5 = imp$imputations$imp1$med_income))
df$med_value_imp <- rowMeans(data.frame(imp1 = imp$imputations$imp1$med_value, imp2 = imp$imputations$imp2$med_value,
                                        imp3 = imp$imputations$imp3$med_value, imp4 = imp$imputations$imp4$med_value, 
                                        imp5 = imp$imputations$imp1$med_value))

# recode percent hispanic to be no greater than 1 
df$pct_hisp[df$pct_hisp > 1] <- 1

# code switcher status
df$switcher <- ifelse(df$city %in% c("Anaheim", "Santa Barbara", "Escondido", "Ventura"), 1, 0)

# keep only needed variables 
keeps <- c("city", "geoid", "year", "treat", "switcher",
           "nunits_multi_z", "nunits_single_z", "nunits_total_z",
           "pct_black", "pct_hisp", "pct_nlw",
           "n_households", "own_rate", "vacancy_rate", 
           "med_value",  "med_value_imp", "med_income", "med_income_imp") 
df <- df[,keeps]

# write out data
write.csv(df, file = "output/final/housing_spatial_fresh.csv", row.names = FALSE)
