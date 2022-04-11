########################################################
# This script processes Census data and merges
# it together
# It brings in segregation data from Trounstine
# and city electoral institutions data collected by 
# Hankinson & Magazinnik
# Output: City by year dataset of institutions 
# (treatment) and covariates (city.csv)
########################################################

rm(list = ls())

# install packages
# install.packages(dplyr)

# load packages
library(dplyr)

# set working directory 
setwd("") # set to create_data
setwd("raw/census/")

########################################################
# Census data
########################################################

# function to subset and transform census data
cleanfun <- function(df, year, keepvars, names) {
  df <- df[-1, keepvars]
  names(df) <- names
  for (i in 3:ncol(df)) {
    df[,i] <- as.numeric(as.character(df[,i]))
  }
  df$year <- year
  return(df)
}

# function to subset and transform census data (2018)
cleanfun18 <- function(df, year, keepvars, names) {
  df <- df[-1,]
  df$GEO_ID <- as.character(df$GEO_ID)
  df$GEO.id2 <- substr(df$GEO_ID, nchar(df$GEO_ID)-6, nchar(df$GEO_ID))
  df <- df[,keepvars]
  names(df) <- names
  for (i in 3:ncol(df)) {
    df[,i] <- as.numeric(as.character(df[,i]))
  }
  df$year <- year 
  return(df)
} 

###################### NONHISPANIC WHITE ########################

# read data
df18 <- read.csv("race/cdp/ACS_18_5YR_B03002_with_ann.csv")
df17 <- read.csv("race/cdp/ACS_17_5YR_B03002_with_ann.csv")
df16 <- read.csv("race/cdp/ACS_16_5YR_B03002_with_ann.csv")
df15 <- read.csv("race/cdp/ACS_15_5YR_B03002_with_ann.csv")
df14 <- read.csv("race/cdp/ACS_14_5YR_B03002_with_ann.csv")
df13 <- read.csv("race/cdp/ACS_13_5YR_B03002_with_ann.csv")
df12 <- read.csv("race/cdp/ACS_12_5YR_B03002_with_ann.csv")
df11 <- read.csv("race/cdp/ACS_11_5YR_B03002_with_ann.csv")
df10 <- read.csv("race/cdp/ACS_10_5YR_B03002_with_ann.csv")

# clean & transform 
keepvars <- c("GEO.id2", # id
                "GEO.display.label", # city 
                "HD01_VD03", # white nonhispanic
                "HD01_VD01") # total

keepvars18 <- c("GEO.id2", # id
              "NAME", # city 
              "B03002_003E", # white nonhispanic
              "B03002_001E") # total

names <- c("GEO_id", "city", "n_wnh", "population")

out18 <- cleanfun18(df = df18, year = 2018, keepvars = keepvars18, names = names)
out17 <- cleanfun(df = df17, year = 2017, keepvars = keepvars, names = names)
out16 <- cleanfun(df = df16, year = 2016, keepvars = keepvars, names = names)
out15 <- cleanfun(df = df15, year = 2015, keepvars = keepvars, names = names)
out14 <- cleanfun(df = df14, year = 2014, keepvars = keepvars, names = names)
out13 <- cleanfun(df = df13, year = 2013, keepvars = keepvars, names = names)
out12 <- cleanfun(df = df12, year = 2012, keepvars = keepvars, names = names)
out11 <- cleanfun(df = df11, year = 2011, keepvars = keepvars, names = names)
out10 <- cleanfun(df = df10, year = 2010, keepvars = keepvars, names = names)

# stack 
wnh <- rbind(out10, out11, out12, out13, out14, out15, out16, out17, out18)

# make variables
wnh$pct_wnh <- ifelse(wnh$population==0, 0, (wnh$n_wnh/wnh$population*100))

###################### INCOME DATA ########################

# read income data
df18 <- read.csv("income/cdp/ACS_18_5YR_S1903_with_ann.csv")
df17 <- read.csv("income/cdp/ACS_17_5YR_S1903_with_ann.csv")
df16 <- read.csv("income/cdp/ACS_16_5YR_S1903_with_ann.csv")
df15 <- read.csv("income/cdp/ACS_15_5YR_S1903_with_ann.csv")
df14 <- read.csv("income/cdp/ACS_14_5YR_S1903_with_ann.csv")
df13 <- read.csv("income/cdp/ACS_13_5YR_S1903_with_ann.csv")
df12 <- read.csv("income/cdp/ACS_12_5YR_S1903_with_ann.csv")
df11 <- read.csv("income/cdp/ACS_11_5YR_S1903_with_ann.csv")
df10 <- read.csv("income/cdp/ACS_10_5YR_S1903_with_ann.csv")

# clean & transform 
keepvars18 <- c("GEO.id2", # id
                "NAME", # city 
                "S1903_C01_001E", # n households
                "S1903_C03_001E", # median income
                "S1903_C02_002E", # percent white
                "S1903_C02_003E", # percent black
                "S1903_C02_005E", # percent asian
                "S1903_C02_009E") # percent hispanic

keepvars17 <- c("GEO.id2", # id
                "GEO.display.label", # city 
                "HC01_EST_VC02", # n households
                "HC03_EST_VC02", # median income
                "HC02_EST_VC04", # percent white
                "HC02_EST_VC05", # percent black
                "HC02_EST_VC07", # percent asian
                "HC02_EST_VC12") # percent hispanic

keepvars16 <- c("GEO.id2", # id
                "GEO.display.label", # city 
                "HC01_EST_VC02", # n households
                "HC02_EST_VC02", # median income
                "HC01_EST_VC04", # percent white
                "HC01_EST_VC05", # percent black
                "HC01_EST_VC07", # percent asian
                "HC01_EST_VC12") # percent hispanic

names <- c("GEO_id", "city", "n_household", "med_income", "pct_white", "pct_black", 
              "pct_asian", "pct_hisp")

out18 <- cleanfun18(df = df18, year = 2018, keepvars = keepvars18, names = names)
out17 <- cleanfun(df = df17, year = 2017, keepvars = keepvars17, names = names)
out16 <- cleanfun(df = df16, year = 2016, keepvars = keepvars16, names = names)
out15 <- cleanfun(df = df15, year = 2015, keepvars = keepvars16, names = names)
out14 <- cleanfun(df = df14, year = 2014, keepvars = keepvars16, names = names)
out13 <- cleanfun(df = df13, year = 2013, keepvars = keepvars16, names = names)
out12 <- cleanfun(df = df12, year = 2012, keepvars = keepvars16, names = names)
out11 <- cleanfun(df = df11, year = 2011, keepvars = keepvars16, names = names)
out10 <- cleanfun(df = df10, year = 2010, keepvars = keepvars16, names = names)

# stack 
income <- rbind(out10, out11, out12, out13, out14, out15, out16, out17, out18)

###################### OCCUPANCY DATA ########################

# read occupancy data
df18 <- read.csv("occupancy/cdp/ACS_18_5YR_B25002/ACS_18_5YR_B25002_with_ann.csv")
df17 <- read.csv("occupancy/cdp/ACS_17_5YR_B25002/ACS_17_5YR_B25002_with_ann.csv")
df16 <- read.csv("occupancy/cdp/ACS_16_5YR_B25002/ACS_16_5YR_B25002_with_ann.csv")
df15 <- read.csv("occupancy/cdp/ACS_15_5YR_B25002/ACS_15_5YR_B25002_with_ann.csv")
df14 <- read.csv("occupancy/cdp/ACS_14_5YR_B25002/ACS_14_5YR_B25002_with_ann.csv")
df13 <- read.csv("occupancy/cdp/ACS_13_5YR_B25002/ACS_13_5YR_B25002_with_ann.csv")
df12 <- read.csv("occupancy/cdp/ACS_12_5YR_B25002/ACS_12_5YR_B25002_with_ann.csv")
df11 <- read.csv("occupancy/cdp/ACS_11_5YR_B25002/ACS_11_5YR_B25002_with_ann.csv")
df10 <- read.csv("occupancy/cdp/ACS_10_5YR_B25002/ACS_10_5YR_B25002_with_ann.csv")

# clean & transform 
keepvars <- c("GEO.id2", "GEO.display.label", "HD01_VD01", "HD01_VD02", "HD01_VD03")
keepvars18 <- c("GEO.id2", "NAME", "B25002_001E", "B25002_002E", "B25002_003E")

names <- c("GEO_id", "city", "n_housing_units", "n_occupied_units", "n_vacant_units")

out18 <- cleanfun18(df = df18, year = 2018, keepvars = keepvars18, names = names)
out17 <- cleanfun(df = df17, year = 2017, keepvars = keepvars, names = names)
out16 <- cleanfun(df = df16, year = 2016, keepvars = keepvars, names = names)
out15 <- cleanfun(df = df15, year = 2015, keepvars = keepvars, names = names)
out14 <- cleanfun(df = df14, year = 2014, keepvars = keepvars, names = names)
out13 <- cleanfun(df = df13, year = 2013, keepvars = keepvars, names = names)
out12 <- cleanfun(df = df12, year = 2012, keepvars = keepvars, names = names)
out11 <- cleanfun(df = df11, year = 2011, keepvars = keepvars, names = names)
out10 <- cleanfun(df = df10, year = 2010, keepvars = keepvars, names = names)

# stack 
occupy <- rbind(out10, out11, out12, out13, out14, out15, out16, out17, out18)

# make variables 
occupy$vacancy_rate <- occupy$n_vacant_units/occupy$n_housing_units

###################### OWNERSHIP DATA ########################

# read ownership data
df18 <- read.csv("owner/cdp/ACS_18_5YR_B25026/ACS_18_5YR_B25026_with_ann.csv")
df17 <- read.csv("owner/cdp/ACS_17_5YR_B25026/ACS_17_5YR_B25026_with_ann.csv")
df16 <- read.csv("owner/cdp/ACS_16_5YR_B25026/ACS_16_5YR_B25026_with_ann.csv")
df15 <- read.csv("owner/cdp/ACS_15_5YR_B25026/ACS_15_5YR_B25026_with_ann.csv")
df14 <- read.csv("owner/cdp/ACS_14_5YR_B25026/ACS_14_5YR_B25026_with_ann.csv")
df13 <- read.csv("owner/cdp/ACS_13_5YR_B25026/ACS_13_5YR_B25026_with_ann.csv")
df12 <- read.csv("owner/cdp/ACS_12_5YR_B25026/ACS_12_5YR_B25026_with_ann.csv")
df11 <- read.csv("owner/cdp/ACS_11_5YR_B25026/ACS_11_5YR_B25026_with_ann.csv")
df10 <- read.csv("owner/cdp/ACS_10_5YR_B25026/ACS_10_5YR_B25026_with_ann.csv")

# clean & transform 
keepvars <- c("GEO.id2", "GEO.display.label", "HD01_VD01", "HD01_VD02")
keepvars18 <- c("GEO.id2", "NAME", "B25026_001E", "B25026_002E")

names <- c("GEO_id", "city", "n_housing_units", "n_owner")

out18 <- cleanfun18(df = df18, year = 2018, keepvars = keepvars18, names = names)
out17 <- cleanfun(df = df17, year = 2017, keepvars = keepvars, names = names)
out16 <- cleanfun(df = df16, year = 2016, keepvars = keepvars, names = names)
out15 <- cleanfun(df = df15, year = 2015, keepvars = keepvars, names = names)
out14 <- cleanfun(df = df14, year = 2014, keepvars = keepvars, names = names)
out13 <- cleanfun(df = df13, year = 2013, keepvars = keepvars, names = names)
out12 <- cleanfun(df = df12, year = 2012, keepvars = keepvars, names = names)
out11 <- cleanfun(df = df11, year = 2011, keepvars = keepvars, names = names)
out10 <- cleanfun(df = df10, year = 2010, keepvars = keepvars, names = names)

# stack 
owner <- rbind(out10, out11, out12, out13, out14, out15, out16, out17, out18)

# make variables 
owner$own_rate <- owner$n_owner/owner$n_housing_units

###################### HOME VALUES ########################

# read value data
df18 <- read.csv("value/cdp/ACS_18_5YR_B25077/ACS_18_5YR_B25077_with_ann.csv")
df17 <- read.csv("value/cdp/ACS_17_5YR_B25077/ACS_17_5YR_B25077_with_ann.csv")
df16 <- read.csv("value/cdp/ACS_16_5YR_B25077/ACS_16_5YR_B25077_with_ann.csv")
df15 <- read.csv("value/cdp/ACS_15_5YR_B25077/ACS_15_5YR_B25077_with_ann.csv")
df14 <- read.csv("value/cdp/ACS_14_5YR_B25077/ACS_14_5YR_B25077_with_ann.csv")
df13 <- read.csv("value/cdp/ACS_13_5YR_B25077/ACS_13_5YR_B25077_with_ann.csv")
df12 <- read.csv("value/cdp/ACS_12_5YR_B25077/ACS_12_5YR_B25077_with_ann.csv")
df11 <- read.csv("value/cdp/ACS_11_5YR_B25077/ACS_11_5YR_B25077_with_ann.csv")
df10 <- read.csv("value/cdp/ACS_10_5YR_B25077/ACS_10_5YR_B25077_with_ann.csv")

# clean & transform 
keepvars <- c("GEO.id2", "GEO.display.label", "HD01_VD01")
keepvars18 <- c("GEO.id2", "NAME", "B25077_001E")

names <- c("GEO_id", "city", "med_value")

out18 <- cleanfun18(df = df18, year = 2018, keepvars = keepvars18, names = names)
out17 <- cleanfun(df = df17, year = 2017, keepvars = keepvars, names = names)
out16 <- cleanfun(df = df16, year = 2016, keepvars = keepvars, names = names)
out15 <- cleanfun(df = df15, year = 2015, keepvars = keepvars, names = names)
out14 <- cleanfun(df = df14, year = 2014, keepvars = keepvars, names = names)
out13 <- cleanfun(df = df13, year = 2013, keepvars = keepvars, names = names)
out12 <- cleanfun(df = df12, year = 2012, keepvars = keepvars, names = names)
out11 <- cleanfun(df = df11, year = 2011, keepvars = keepvars, names = names)
out10 <- cleanfun(df = df10, year = 2010, keepvars = keepvars, names = names)

# stack 
value <- rbind(out10, out11, out12, out13, out14, out15, out16, out17, out18)

###################### DENSITY ########################

# read data
df <- read.csv("density/DEC_10_SF1_GCTPH1.ST10_with_ann.csv")

# clean & transform 
keepvars <- c("GCT_STUB.target.geo.id2", "GEO.display.label", "GCT_STUB.display.label", 
              "HD01", "SUBHD0303", "SUBHD0401")
names <- c("GEO_id", "state", "city", "pop", "land_area", "density")

out <- cleanfun(df = df, year = NA, keepvars = keepvars, names = names)
density <- out[out$state=="California",]
density <- density[,c("GEO_id", "land_area", "density")]

################## MERGE ALL ###########################

occupy <- occupy[,c("GEO_id", "year", "vacancy_rate")]
owner <- owner[,c("GEO_id", "year", "own_rate")]
value <- value[,c("GEO_id", "year", "med_value")]
wnh <- wnh[,c("GEO_id", "year", "pct_wnh", "population")]

full1 <- merge(income, occupy, by = c("GEO_id", "year"), all = TRUE)
full2 <- merge(full1, owner, by = c("GEO_id", "year"), all = TRUE)
full3 <- merge(full2, value, by = c("GEO_id", "year"), all = TRUE)
final <- merge(full3, wnh, by = c("GEO_id", "year"), all = TRUE)

################### IMPUTE MISSING VALUES ####################
# fill in missing values for 2019 using three-year averages 

sub1618 <- final[final$year %in% c(2016:2018),]
df19 <- sub1618 %>%
        group_by(GEO_id) %>%
        dplyr::summarise(n_household = mean(n_household, na.rm=TRUE),
                  med_income = mean(med_income, na.rm = TRUE),
                  pct_white = mean(pct_white, na.rm = TRUE),
                  pct_black = mean(pct_black, na.rm = TRUE),
                  pct_asian = mean(pct_asian, na.rm = TRUE),
                  pct_hisp = mean(pct_hisp, na.rm = TRUE),
                  pct_wnh = mean(pct_wnh, na.rm = TRUE),
                  vacancy_rate = mean(vacancy_rate, na.rm = TRUE),
                  own_rate = mean(own_rate, na.rm = TRUE),
                  med_value = mean(med_value, na.rm = TRUE), 
                  population = mean(population, na.rm = TRUE))

ids <- sub1618[sub1618$year==2018, c("GEO_id", "city")]
df19 <- merge(ids, df19, by = "GEO_id", all.x = TRUE)
df19$year <- 2019 

final2 <- rbind(final, df19)

# merge on density 
final2 <- merge(final2, density, by = "GEO_id", all.x = TRUE)

########################################################
# Segregation 
########################################################

segregation <- read.table("../trounstineTheil.tab", header = T, sep = "\t", fill = TRUE)

seg11 <- subset(segregation, year=="2011")
seg11 <- subset(seg11, select=-c(year))
seg11$GEO_id <- formatC(seg11$geo_id2, width = 7, format= "d", flag = "0")
seg11 <- seg11[,names(seg11) %in% c("GEO_id", "year", "H_citytract_NHW")]

final2 <- merge(final2, seg11, all.x = T, by = "GEO_id")

#which cities are missing?
unique(subset(final2, is.na(H_citytract_NHW))$city) # a bunch of CDPs

########################################################
# Institutions 
########################################################

inst <- read.csv("../cvraCities.csv")
inst <- inst[,c(1:4,6)]
names(inst) <- c("city2", "county", "date_switch_agree", "date_switch_elect", "decided_by")

# create city variable to match census data 
inst$city <- as.character(paste0(inst$city2, " city, California")) 
inst$city[inst$city=="Apple Valley city, California"] <- "Apple Valley town, California"
inst$city[inst$city=="Paso Robles city, California"] <- "El Paso de Robles (Paso Robles) city, California"
inst$city[inst$city=="Ventura city, California"] <- "San Buenaventura (Ventura) city, California"
inst$city[inst$city=="Yucca Valley city, California"] <- "Yucca Valley town, California"
inst$city[inst$city=="Windsor city, California"] <- "Windsor town, California"

# code always district places
always <- inst$city[inst$decided_by=="Always"]

# code threatened places
threats <- inst$city[inst$decided_by %in% c("Letter", "Settlement", "Threat")]

# code time-varying treatment (from first election)
treats1 <- inst$city[!is.na(inst$date_switch_elect)]

# code time-varying treatment (from agreement to switch) 
treats2 <- inst$city[!is.na(inst$date_switch_agree)]

### code treatment in final2 dataset
# fix eastvale in 2010 (was CDP, then became city)
final2$city <- as.character(final2$city)
final2$city[final2$year==2010 & final2$city=="Eastvale CDP, California"] <- "Eastvale city, California"

# create balanced panel, standardizing city name over time
ids <- unique(final2[,c("GEO_id", "city")])
# identify geoids with multiple city names
dup.ids <- unique(ids$GEO_id[duplicated(ids$GEO_id)]) # some multiple city names per geoid
# subset to these duplicates and sort them to put the right city name on top
dups <- ids[ids$GEO_id %in% dup.ids,]
dups <- dups[order(dups$GEO_id, dups$city),]
# drop observations after the first
dups$drop <- duplicated(dups$GEO_id)
dups <- dups[dups$drop==FALSE,] 
# make final base dataset 
ids <- rbind(ids[!ids$GEO_id %in% dup.ids,], dups[,!names(dups) %in% "drop"])
base <- expand.grid(ids$GEO_id, c(2010:2019))
names(base) <- c("GEO_id", "year")
base <- merge(base, ids, by = "GEO_id", all.x = TRUE)
final2 <- merge(base, final2[,!names(final2) %in% "city"], by = c("GEO_id", "year"), all.x = TRUE)

final2$switcher <- ifelse(final2$city %in% treats2, 1, 0) # switcher=city that would eventually agree to switch
final2$treat <- final2$treat2 <- 0

# main treatment: coded from year of first election 
final2$treat[final2$city %in% always] <- 1 
for (i in 1:length(treats1)) {
  year <- inst$date_switch_elect[inst$city==treats1[i]]
  final2$treat[final2$city==treats1[i] & final2$year>=year] <- 1
  final2$year_switch[final2$city==treats1[i]] <- year
}

# second treatment: coded from year of agreement to switch
final2$treat2[final2$city %in% always] <- 1
for (i in 1:length(treats2)) {
  year <- inst$date_switch_agree[inst$city==treats2[i]]
  final2$treat2[final2$city==treats2[i] & final2$year>=year] <- 1
  final2$year_agree[final2$city==treats2[i]] <- year
}

# sort by location/year
final2 <- final2[order(final2$city, final2$year),]
check <- final2[final2$switcher==1, c("city", "year", "switcher", "treat")]
unique(check$city) %in% inst$city

# write out dataset
write.csv(final2, file = "../../output/temp/city.csv", row.names = FALSE)
