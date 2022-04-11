########################################################
# This script processes CEDA data, codes ethnicities
# of last names, and aggregates to city/year level 
########################################################

rm(list = ls())

# install packages
#install.packages("wru")
#install.packages("stringr")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("DataCombine")

# load packages
library("wru")
library("stringr")
library(plyr); library(dplyr)
library(DataCombine)

# set working directory 
setwd("") # set to create_data

# set seed
set.seed(123)

# load data
elec <- read.csv("output/temp/ceda.csv", stringsAsFactors = FALSE)
elec$state <- "ca"

# create location variable for merge
elec$loc2merge <- paste0(str_to_title(tolower(elec$location)), " city, California")

# manually recode some names 
elec$loc2merge[elec$loc2merge=="La Canada Flintridge city, California"] <- "La Ca\xf1ada Flintridge city, California"
elec$loc2merge[elec$loc2merge=="Alamo city, California"] <- "Amador CDP, California"
elec$loc2merge[elec$loc2merge=="Amador city, California"] <- "Amador City city, California"
elec$loc2merge[elec$loc2merge=="Angels City city, California"] <- "Angels city, California"
elec$loc2merge[elec$loc2merge=="Apple Valley city, California"] <- "Apple Valley town, California"
elec$loc2merge[elec$loc2merge=="Apple Valley-Recall city, California"] <- "Apple Valley town, California"
elec$loc2merge[elec$loc2merge=="Arcata City city, California"] <- "Arcata city, California"
elec$loc2merge[elec$loc2merge=="Arden Arcade - Proposed city, California"] <- "Arden-Arcade CDP, California"
elec$loc2merge[elec$loc2merge=="Atherton city, California"] <- "Atherton town, California"
elec$loc2merge[elec$loc2merge=="Barstow-R city, California"] <- "Barstow city, California"
elec$loc2merge[elec$loc2merge=="Berkeley - Rcv city, California"] <- "Berkeley city, California"
elec$loc2merge[elec$loc2merge=="Briggs city, California"] <- "Biggs city, California"
elec$loc2merge[elec$loc2merge=="Carmel-By-The-Sea city, California"] <- "Carmel-by-the-Sea city, California"
elec$loc2merge[elec$loc2merge=="Castro Valley city, California"] <- "Castro Valley CDP, California"
elec$loc2merge[elec$loc2merge=="City Of Angels city, California"] <- "Angels city, California"
elec$loc2merge[elec$loc2merge=="City Of Commerce city, California"] <- "Commerce city, California"
elec$loc2merge[elec$loc2merge=="City Of Taft-R city, California"] <- "Taft city, California"
elec$loc2merge[elec$loc2merge=="Colma city, California"] <- "Colma town, California"
elec$loc2merge[elec$loc2merge=="Coloma city, California"] <- "Coloma CDP, California"
elec$loc2merge[elec$loc2merge=="Corte Madera city, California"] <- "Corte Madera town, California"
elec$loc2merge[elec$loc2merge=="Crescent city, California"] <- "Crescent City city, California"
elec$loc2merge[elec$loc2merge=="Cresent City city, California"] <- "Crescent City city, California"
elec$loc2merge[elec$loc2merge=="Cuadahy city, California"] <- "Cudahy city, California"
elec$loc2merge[elec$loc2merge=="Culver city, California"] <- "Culver City city, California"
elec$loc2merge[elec$loc2merge=="Danville city, California"] <- "Danville town, California"
elec$loc2merge[elec$loc2merge=="East Paolo Alto city, California"] <- "East Palo Alto city, California"
elec$loc2merge[elec$loc2merge=="Eastvale - Proposed city, California"] <- "Eastvale city, California"
elec$loc2merge[elec$loc2merge=="El Paso De Robles city, California"] <- "El Paso de Robles (Paso Robles) city, California"
elec$loc2merge[elec$loc2merge=="El Segundo City city, California"] <- "El Segundo city, California"
elec$loc2merge[elec$loc2merge=="Fairfax city, California"] <- "Fairfax town, California"
elec$loc2merge[elec$loc2merge=="Filmore city, California"] <- "Fillmore city, California"
elec$loc2merge[elec$loc2merge=="Fontanta city, California"] <- "Fontana city, California"
elec$loc2merge[elec$loc2merge=="Foster city, California"] <- "Foster City city, California"
elec$loc2merge[elec$loc2merge=="Grand Terrance city, California"] <- "Grand Terrace city, California"
elec$loc2merge[elec$loc2merge=="Hillsborough city, California"] <- "Hillsborough town, California"
elec$loc2merge[elec$loc2merge=="Ingelwood city, California"] <- "Inglewood city, California"
elec$loc2merge[elec$loc2merge=="Jurupa Valley - Proposed city, California"] <- "Jurupa Valley city, California"
elec$loc2merge[elec$loc2merge=="King city, California"] <- "King City city, California"
elec$loc2merge[elec$loc2merge=="Kinsburg city, California"] <- "Kingsburg city, California"
elec$loc2merge[elec$loc2merge=="Laguna Nigel city, California"] <- "Laguna Niguel city, California"
elec$loc2merge[elec$loc2merge=="Lancaster City city, California"] <- "Lancaster city, California"
elec$loc2merge[elec$loc2merge=="Lompos city, California"] <- "Lompoc city, California"
elec$loc2merge[elec$loc2merge=="Loomis city, California"] <- "Loomis town, California"
elec$loc2merge[elec$loc2merge=="Los Alos city, California"] <- "Los Altos city, California"
elec$loc2merge[elec$loc2merge=="Los Altos Hills city, California"] <- "Los Altos Hills town, California"
elec$loc2merge[elec$loc2merge=="Los Gatos city, California"] <- "Los Gatos town, California"
elec$loc2merge[elec$loc2merge=="Los Gattos city, California"] <- "Los Gatos town, California"
elec$loc2merge[elec$loc2merge=="Los Angeles-San Fernando city, California"] <- "San Fernando city, California"
elec$loc2merge[elec$loc2merge=="Mammoth Lakes city, California"] <- "Mammoth Lakes town, California"
elec$loc2merge[elec$loc2merge=="Mcfarland city, California"] <- "McFarland city, California"
elec$loc2merge[elec$loc2merge=="Milipitas city, California"] <- "Milpitas city, California"
elec$loc2merge[elec$loc2merge=="Moraga city, California"] <- "Moraga town, California"
elec$loc2merge[elec$loc2merge=="Mt. Shasta city, California"] <- "Mount Shasta city, California"
elec$loc2merge[elec$loc2merge=="Mt.shasta city, California"] <- "Mount Shasta city, California"
elec$loc2merge[elec$loc2merge=="National city, California"] <- "National City city, California"
elec$loc2merge[elec$loc2merge=="Nevada city, California"] <- "Nevada City city, California"
elec$loc2merge[elec$loc2merge=="Oakhurst city, California"] <- "Oakhurst CDP, California"
elec$loc2merge[elec$loc2merge=="Oakland - Rcv city, California"] <- "Oakland city, California"
elec$loc2merge[elec$loc2merge=="Oakland-At Large city, California"] <- "Oakland city, California"
elec$loc2merge[elec$loc2merge=="Palos Verdes city, California"] <- "Rancho Palos Verdes city, California"
elec$loc2merge[elec$loc2merge=="Paradise city, California"] <- "Paradise town, California"
elec$loc2merge[elec$loc2merge=="Paso De Robles city, California"] <- "El Paso de Robles (Paso Robles) city, California"
elec$loc2merge[elec$loc2merge=="Paso Robles city, California"] <- "El Paso de Robles (Paso Robles) city, California"
elec$loc2merge[elec$loc2merge=="Pasa Robles city, California"] <- "El Paso de Robles (Paso Robles) city, California"
elec$loc2merge[elec$loc2merge=="Portersville city, California"] <- "Porterville city, California"
elec$loc2merge[elec$loc2merge=="Portola Valley city, California"] <- "Portola Valley town, California"
elec$loc2merge[elec$loc2merge=="Redwood city, California"] <- "Redwood City city, California"
elec$loc2merge[elec$loc2merge=="Rancho Palos Verde city, California"] <- "Rancho Palos Verdes city, California"
elec$loc2merge[elec$loc2merge=="Ranch Santa Margarita city, California"] <- "Rancho Santa Margarita city, California"
elec$loc2merge[elec$loc2merge=="Ross city, California"] <- "Ross town, California"
elec$loc2merge[elec$loc2merge=="Rossmoor-P city, California"] <- "Rossmoor CDP, California"
elec$loc2merge[elec$loc2merge=="Salina city, California"] <- "Salinas city, California"
elec$loc2merge[elec$loc2merge=="San Anselmo city, California"] <- "San Anselmo town, California"
elec$loc2merge[elec$loc2merge=="San Bernadino city, California"] <- "San Bernardino city, California"
elec$loc2merge[elec$loc2merge=="San Buenaventuara city, California"] <- "San Buenaventura (Ventura) city, California"
elec$loc2merge[elec$loc2merge=="San Buenaventura city, California"] <- "San Buenaventura (Ventura) city, California"
elec$loc2merge[elec$loc2merge=="San Jaciinto city, California"] <- "San Jacinto city, California"
elec$loc2merge[elec$loc2merge=="San Leandro - Rcv city, California"] <- "San Leandro city, California"
elec$loc2merge[elec$loc2merge=="San Marino Hills city, California"] <- "San Marino city, California"
elec$loc2merge[elec$loc2merge=="San Rafeal city, California"] <- "San Rafael city, California"
elec$loc2merge[elec$loc2merge=="Santa Fee Springs city, California"] <- "Santa Fe Springs city, California"
elec$loc2merge[elec$loc2merge=="Santa Barabara city, California"] <- "Santa Barbara city, California"
elec$loc2merge[elec$loc2merge=="Satatoga city, California"] <- "Saratoga city, California"
elec$loc2merge[elec$loc2merge=="Sartoga city, California"] <- "Saratoga city, California"
elec$loc2merge[elec$loc2merge=="Sebastopool city, California"] <- "Sebastopol city, California"
elec$loc2merge[elec$loc2merge=="Slovang city, California"] <- "Solvang city, California"
elec$loc2merge[elec$loc2merge=="Smi Valley city, California"] <- "Simi Valley city, California"
elec$loc2merge[elec$loc2merge=="St Helena city, California"] <- "St. Helena city, California"
elec$loc2merge[elec$loc2merge=="Suisun city, California"] <- "Suisun City city, California"
elec$loc2merge[elec$loc2merge=="Tentynine Palms city, California"] <- "Twentynine Palms city, California"
elec$loc2merge[elec$loc2merge=="Techachapi city, California"] <- "Tehachapi city, California"
elec$loc2merge[elec$loc2merge=="Tiburon city, California"] <- "Tiburon town, California"
elec$loc2merge[elec$loc2merge=="Town Of Apple Valley city, California"] <- "Apple Valley town, California"
elec$loc2merge[elec$loc2merge=="Town Of Yucca Valley city, California"] <- "Yucca Valley town, California"
elec$loc2merge[elec$loc2merge=="Truckee city, California"] <- "Truckee town, California"
elec$loc2merge[elec$loc2merge=="West Lake Village city, California"] <- "Westlake Village city, California"
elec$loc2merge[elec$loc2merge=="Westminister city, California"] <- "Westminster city, California"
elec$loc2merge[elec$loc2merge=="Whittier City city, California"] <- "Whittier city, California"
elec$loc2merge[elec$loc2merge=="Williams City city, California"] <- "Williams city, California"
elec$loc2merge[elec$loc2merge=="Willlows city, California"] <- "Willows city, California"
elec$loc2merge[elec$loc2merge=="Wildomar-P city, California"] <- "Wildomar city, California"
elec$loc2merge[elec$loc2merge=="Windsor city, California"] <- "Windsor town, California"
elec$loc2merge[elec$loc2merge=="Woodside city, California"] <- "Woodside town, California"
elec$loc2merge[elec$loc2merge=="Yuba city, California"] <- "Yuba City city, California"
elec$loc2merge[elec$loc2merge=="Yucca Valley city, California"] <- "Yucca Valley town, California"

# load data for geoid
id <- read.csv("output/temp/city.csv")
names(id)[1:3] <- c("place", "year", "loc2merge")

# merge place onto election data
placefile <- unique(id[,c("loc2merge", "place")])
elec_place <- merge(elec, placefile, by = "loc2merge", all.x = TRUE)
prop.table(table(is.na(elec_place$place))) # looks good

# get rid of few cities that did not find a place match
elec_place <- elec_place[!is.na(elec_place$place),]

# merge rest of covariates (2010 onward) onto election data
elec2 <- merge(elec_place, id[,!names(id) %in% "place"], by = c("loc2merge", "year"), all.x = TRUE)
prop.table(table(is.na(elec2$switcher)))
prop.table(table(is.na(elec2$switcher[elec2$year>=2010]))) # looks good

# strip off first digit for wru
elec2$place <- substr(as.character(elec2$place), 2, 6)

# register census key
api.key <- "" # your api key here

# code ethnicity
out <- predict_race(voter.file = elec2, surname.only = FALSE, census.surname = TRUE, 
                    census.geo = "place", census.key = api.key)

# code candidates
out$latino <- ifelse(out$pred.his>.5, 1, 0)
out$asian <- ifelse(out$pred.asi>.5, 1, 0)
out$black <- ifelse(out$pred.bla>.5, 1, 0)
out$white <- ifelse(out$pred.whi>.5, 1, 0)

# manually code jurupa valley
out$latino[out$loc2merge=="Jurupa Valley city, California"] <- 0
out$latino[out$loc2merge=="Jurupa Valley city, California" & 
             out$surname %in% c("Castro Jr.", "Leja", "Barajas", "Calzada", "Enriquez", "Chavez")] <- 1

out$black[out$loc2merge=="Jurupa Valley city, California"] <- 0
out$black[out$loc2merge=="Jurupa Valley city, California" & 
             out$surname %in% c("Towels", "Kelly, Jr.", "Hagans" )] <- 1

out$white[out$loc2merge=="Jurupa Valley city, California"] <- 0
out$white[out$loc2merge=="Jurupa Valley city, California" & 
            out$surname %in% c("Scroggins", "Todd", "Tucker", "Hancock", "Goodland",
                               "Johnston", "Berkson", "Tourville", "Lauritzen", "Roughton",
                               "Skerbelis", "Shapiro", "Henderson")] <- 1

out$asian[out$loc2merge=="Jurupa Valley city, California"] <- 0

# recode elected dummy
# there are also runoffs 
out$win <- 0
out$win[out$elected==1] <- 1

# code winners

# latino
out$win_l <- 0
out$win_l[out$win==1 & out$latino==1] <- 1

# asian
out$win_a <- 0
out$win_a[out$win==1 & out$asian==1] <- 1

# white
out$win_w <- 0
out$win_w[out$win==1 & out$white==1] <- 1

# black
out$win_b <- 0
out$win_b[out$win==1 & out$black==1] <- 1

# aggregate to election level 
agg_elecyear <- out %>%
  group_by(raceid) %>%
  summarise(year = mean(year),
            candidates = n(),
            candidates_l = sum(latino),
            n_elected = sum(win),
            n_elected_l = sum(win_l),
            n_elected_a = sum(win_a),
            n_elected_b = sum(win_b),
            n_elected_w = sum(win_w),
            totalvotes = sum(votes),
            totalvotes_l = sum(votes[latino==1]))

# merge on location
loc <- unique(out[,c("raceid", "loc2merge", "place", "treat")])
agg_elecyear <- merge(agg_elecyear, loc, by = "raceid", all.x = TRUE)

# create a few additional variables on latino electoral performance

# before aggregating to city/year, want to compute certain variables 
# based on first election in case there was a runoff
# eliminating pre-runoff elections:
agg_elecyear$placeyear <- paste0(agg_elecyear$place, "_", agg_elecyear$year)
runoffs <- unique(agg_elecyear$placeyear[agg_elecyear$n_elected==0])
agg_elecyear_sub <- agg_elecyear[!agg_elecyear$placeyear %in% runoffs & agg_elecyear$n_elected>0 | (agg_elecyear$placeyear %in% runoffs & agg_elecyear$n_elected==0),]

# 1) number of latinos elected over number of seats 
# will want to sum up the number of seats won in total & won by Latinos in a city-year, then compute proportion

# 2) proportion of races where a latino can win (at least one latino candidate running)
# compute potential latino seats 
agg_elecyear_sub$pot.latino.seats <- apply(agg_elecyear_sub[,c("candidates_l", "n_elected")], 1, min)
# if at-large election, this is either the number of latino candidates or the number of seats (if even more latino candidates than seats) 
# divided by the number of available seats; thus the maximum seats latinos could in principle occupy
# if single-member, this is 0 (if no latinos) or 1 (if at least 1 latino) because n_elected is 1; also consistent with above 
# next we aggregate pot.latino.seats to city-year and divide by total number of seats 

# 3) vote share to latino candidates
# also compute this on city-year: total votes cast for Latinos in all elections in that city

# aggregate to location-year
agg_cityyear <- agg_elecyear %>%
  group_by(place, year) %>%
  summarise(n_elected = sum(n_elected),
            n_elected_l = sum(n_elected_l),
            n_elected_a = sum(n_elected_a),
            n_elected_b = sum(n_elected_b),
            n_elected_w = sum(n_elected_w))

# aggregate to location-year
agg_cityyear_sub <- agg_elecyear_sub %>%
  group_by(place, year) %>%
  summarise(candidates = sum(candidates),
            candidates_l = sum(candidates_l),
            seats_l = sum(pot.latino.seats),
            totalvotes = sum(totalvotes),
            totalvotes_l = sum(totalvotes_l))

# merge
agg_cityyear <- merge(agg_cityyear_sub, agg_cityyear[,c("place", "year", "n_elected", "n_elected_l", "n_elected_a",
                                                        "n_elected_b", "n_elected_w")], 
                      by = c("place", "year"))

# finalize latino electoral success variables

# 1) number of latinos elected over number of seats (calculated on full data)
agg_cityyear$prop_latino_elected <- agg_cityyear$n_elected_l/agg_cityyear$n_elected
agg_cityyear$prop_asian_elected <- agg_cityyear$n_elected_a/agg_cityyear$n_elected
agg_cityyear$prop_black_elected <- agg_cityyear$n_elected_b/agg_cityyear$n_elected
agg_cityyear$prop_white_elected <- agg_cityyear$n_elected_w/agg_cityyear$n_elected

# 2) proportion of races where a latino could win (at least one latino candidate running)
agg_cityyear$prop_latino_ran <- agg_cityyear$seats_l/agg_cityyear$n_elected

# 3) total vote share to latino candidates
agg_cityyear$latino_voteshare <- agg_cityyear$totalvotes_l/agg_cityyear$totalvotes

# sort by city, year
full <- agg_cityyear
full <- full[order(full$place, full$year),]

# write out data
write.csv(full, "output/temp/ceda_agg.csv", row.names = FALSE)
