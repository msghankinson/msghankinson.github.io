########################################################
# This script processes data on aggregate permits 
# from the Census (statewide_190715.csv)
# as well as affordability annual progress reports
# (affordability_mh.csv)
# It merges these aggregate outcomes and outputs a 
# dataset of city by year outcome variables called 
# housing_apr.csv
########################################################

# clear workspace
rm(list = ls())

# install libraries
#install.packages("gdata")
#install.packages("readxlsb")

# load libraries
library(gdata)
library(readxlsb)

# set working directory
setwd("") # set to create_data
setwd("raw/aggregatePermits/")

df <- read.xls("affordability_mh.xlsx", sheet = "5th Cycle APR Raw Data-Final", header = TRUE)
df <- df[, c(1, 2, 4, 5, 7, 11, 15, 17, 19)]
names(df) <- c("location", "county", "year", "status", "permits_vli", "permits_li", "permits_mod", "permits_abovemod", "permits")
df <- df[c(2:nrow(df)),]
df <- df[df$location!="",]

# append 2019 data
df19 <- read_xlsb("affordability_2020.xlsb", sheet = "5th Cycle APR Raw Data", header = TRUE)
df19 <- df19[, c(2, 1, 4, 5, 7, 11, 15, 17, 19)]
names(df19) <- c("location", "county", "year", "status", "permits_vli", "permits_li", "permits_mod", "permits_abovemod", "permits")
df19 <- df19[c(2:nrow(df19)),]
df19 <- df19[df19$location!="" & df19$year=="2019",]

df <- rbind(df, df19)
df <- df[order(df$location, df$year),]

# one manual correction (probably typo)
df$permits[df$permits=="200%"] <- "2"

# change factors to character/numeric
df$location <- as.character(df$location)
df$county <- as.character(df$county)
df$year <- as.character(df$year)
df$status <- as.character(df$status)
df$permits_vli <- as.numeric(as.character(df$permits_vli))
df$permits_li <- as.numeric(as.character(df$permits_li))
df$permits_mod <- as.numeric(as.character(df$permits_mod))
df$permits_abovemod <- as.numeric(gsub(",","",df$permits_abovemod)) # comma in xlsx file
df$permits <- as.numeric(as.character(df$permits))
                         
# change NAs to 0 
df$permits_vli[is.na(df$permits_vli)] <- 0
df$permits_li[is.na(df$permits_li)] <- 0
df$permits_mod[is.na(df$permits_mod)] <- 0
df$permits_abovemod[is.na(df$permits_abovemod)] <- 0
df$permits[is.na(df$permits)] <- 0

# check that columns sum to total
df$permits_check <- df$permits_vli + df$permits_li + df$permits_mod + df$permits_abovemod
df[df$permits_check!=df$permits,]

# merge reports onto permit data
housing <- read.csv("statewide_190715.csv")
housing19 <- read.csv("statewide_2019.csv")
housing19 <- housing19[housing19$Location!="Location",]

# combine datasets
housing19$Permits <- as.numeric(housing19$Permits)
total19 <- housing19[housing19$Series=="Total Units", c("Location", "Year", "Permits")]
single19 <- housing19[housing19$Series=="Units in Single-Family Structures", c("Location", "Year", "Permits")]
multi19 <- housing19[housing19$Series=="Units in All Multi-Family Structures", c("Location", "Year", "Permits")]
names(total19) <- c("location", "year", "total")
names(single19) <- c("location", "year", "single")
names(multi19) <- c("location", "year", "multi")
temp1 <- merge(total19, single19, by = c("location", "year"), all.x = TRUE)
out19 <- merge(temp1, multi19, by = c("location", "year"), all.x = TRUE)
out19$multiTwo <- out19$multiThreeFour <- out19$multiFivePlus <- NA
housing <- rbind(housing, out19)

# merge a few manually
in_housing <- unique(housing$location)
in_df <- unique(df$location)
notmerged <- in_housing[!in_housing %in% in_df]
notmerged <- notmerged[-grep("UNINCORPORATED AREA", notmerged)]

df$location_df <- df$location
df$location[df$location_df=="APPLE VALLEY"] <- "APPLE VALLEY TOWN"
df$location[df$location_df=="ATHERTON"] <- "ATHERTON TOWN"
df$location[df$location_df=="CATHEDRAL"] <- "CATHEDRAL CITY"
df$location[df$location_df=="COLMA"] <- "COLMA TOWN"
df$location[df$location_df=="CORTE MADERA"] <- "CORTE MADERA TOWN"
df$location[df$location_df=="FAIRFAX"] <- "FAIRFAX TOWN"
df$location[df$location_df=="HILLSBOROUGH"] <- "HILLSBOROUGH TOWN"
df$location[df$location_df=="LOOMIS"] <- "LOOMIS TOWN"
df$location[df$location_df=="LOS GATOS"] <- "LOS GATOS TOWN"
df$location[df$location_df=="MAMMOTH LAKES"] <- "MAMMOTH LAKES TOWN"
df$location[df$location_df=="PARADISE"] <- "PARADISE TOWN"
df$location[df$location_df=="PORTOLA VALLEY"] <- "PORTOLA VALLEY TOWN"
df$location[df$location_df=="ST. HELENA"] <- "SAINT HELENA"
df$location[df$location_df=="TIBURON"] <- "TIBURON TOWN"
df$location[df$location_df=="WINDSOR"] <- "WINDSOR TOWN"
df$location[df$location_df=="WOODSIDE"] <- "WOODSIDE TOWN"
df$location[df$location_df=="YOUNTVILLE"] <- "YOUNTVILLE TOWN"
df$location[df$location_df=="YUCCA VALLEY"] <- "YUCCA VALLEY TOWN"

# merge
final <- merge(housing, df, by = c("location", "year"), all.x = TRUE)

# see how much missing data remains
prop.table(table(is.na(final$permits[final$year>=2013])))

# see how new data aligns with census
plot(final$permits, final$total)
abline(a=0, b=1)
cor(final$permits, final$total, use = "complete.obs")

# save data
write.csv(final, file = "../../output/temp/housing_apr.csv", row.names = FALSE)
