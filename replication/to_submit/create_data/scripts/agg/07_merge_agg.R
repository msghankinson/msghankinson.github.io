##################################################################
# This script merges all temporary datasets to create final 
# aggregate housing dataset for analysis
##################################################################

rm(list = ls())

# install packages
#install.packages("stringr")
#install.packages("Amelia")
#install.packages("ggplot2")
#install.packages("plyr")
#install.packages("dplyr")

# load packages
library(stringr)
library(Amelia)
library(ggplot2)
library(plyr); library(dplyr)

# set working directory 
setwd("") # set to create_data

# set seed
set.seed(123)

# read census/institutions data
df <- read.csv("output/temp/city.csv")

# check for duplicates
check <- duplicated(df[,c("city", "year")])
table(check)

# read housing data
housing <- read.csv("output/temp/housing_apr.csv")
housing$location <- as.character(housing$location)

# check for duplicates
check <- duplicated(housing[,c("location", "year")])
table(check) # some duplicates here
dups <- unique(housing$location[check==1])
housing[housing$location %in% dups & housing$year>2010,]

# drop two duplicate observations (they have the same values)
housing <- housing[check==FALSE,]

# drop a few CDP observations (otherwise they will merge on instead of city observations)
df <- df[!df$city %in% c("Burbank CDP, California", "El Cerrito CDP, California", "Greenfield CDP, California",
                         "Live Oak CDP, California", "Mountain View CDP, California", "Rolling Hills CDP, California"),]

# create location variable for merge
df$city2 <- str_remove(df$city, " CDP, California")
df$city2 <- str_remove(df$city2, " city, California")
df$city2 <- str_remove(df$city2, ", California")
df$city2 <- toupper(df$city2)
df$location <- as.character(df$city2)

# manual recode of a few locations for merge
df$location[df$location=="EL PASO DE ROBLES (PASO ROBLES)"] <- "PASO ROBLES"
df$location[df$location=="SAN BUENAVENTURA (VENTURA)"] <- "SAN BUENAVENTURA"
df$location[df$location=="TRUCKEE TOWN"] <- "TRUCKEE"
df$location[df$location=="YOUNTVILLE"] <- "YOUNTVILLE TOWN"
df$location[df$location=="DANVILLE TOWN"] <- "DANVILLE"
df$location[df$city=="La Ca\xf1ada Flintridge city, California"] <- "LA CANADA FLINTRIDGE"

# merge housing and census/institutions data
housing.sub <- housing[housing$year %in% c(2010:2019),]
housing2 <- merge(housing.sub, df, by = c("location", "year"), all.x = TRUE)
unique(housing2$location[is.na(housing2$switcher)]) # only nonmerged obs are counties and unincorporated areas

# drop these observations 
housing2 <- housing2[!is.na(housing2$switcher),]

# check for duplicates
check <- duplicated(housing2[,c("location", "year")])
table(check) # no duplicates

# create variables for analysis
housing2$permits_sum <- housing2$permits_vli + housing2$permits_li + housing2$permits_mod + housing2$permits_abovemod
housing2$permits_propli <- (housing2$permits_vli + housing2$permits_li)/housing2$permits_sum
housing2$l.permits_sum <- log(housing2$permits_sum + 1)
housing2$l.permits_vli <- log(housing2$permits_vli + 1)
housing2$l.permits_li <- log(housing2$permits_li + 1)
housing2$l.permits_mod <- log(housing2$permits_mod + 1)
housing2$l.permits_abovemod <- log(housing2$permits_abovemod + 1)
housing2$l.total <- log(housing2$total+1)
housing2$l.multi <- log(housing2$multi+1)
housing2$l.single <- log(housing2$single+1)
housing2$l.permits_low <- log(housing2$permits_li + housing2$permits_vli + 1)
housing2$population.th <- housing2$population/1000
housing2$med_value.th <- housing2$med_value/1000
housing2$med_income.th <- housing2$med_income/1000

# restrict to relevant covariates and moderators 
regset <- housing2[, c("permits_propli", "l.permits_sum", "l.permits_low", "l.permits_mod", "l.permits_abovemod",
                      "H_citytract_NHW", "l.total", "l.single", "l.multi", "total", "single", "multi",
                      "pct_black", "pct_hisp", "pct_wnh", "pct_asian", "med_income.th", "med_value.th",
                      "own_rate", "vacancy_rate", "density", "population.th", "land_area",
                      "year", "location", "switcher", "treat", "treat2", "year_switch", "year_agree", "GEO_id")]

# save jurupa valley values to overwrite back afterward 
# (jurupa valley was unincorporated before 2011 and we don't want it imputed)
jurupa <- regset[regset$location=="JURUPA VALLEY",]

# impute missing values
bds <- matrix(c(which(colnames(regset)=="population.th"), 0, max(regset$population.th, na.rm = T)), nrow = 1)
imp <- amelia(regset, m = 5, bounds = bds, idvars = c("year", "location", "GEO_id", "switcher", "treat", "treat2", 
                                                      "l.total", "l.single", "l.multi", "total", "single", "multi",
                                                      "permits_propli", "l.permits_sum", "l.permits_low", "l.permits_mod", 
                                                      "l.permits_abovemod"))

regset$pct_black_imp <- rowMeans(data.frame(imp1 = imp$imputations$imp1$pct_black, imp2 = imp$imputations$imp2$pct_black,
                                            imp3 = imp$imputations$imp3$pct_black, imp4 = imp$imputations$imp4$pct_black, 
                                            imp5 = imp$imputations$imp1$pct_black))
regset$pct_hisp_imp <- rowMeans(data.frame(imp1 = imp$imputations$imp1$pct_hisp, imp2 = imp$imputations$imp2$pct_hisp,
                                           imp3 = imp$imputations$imp3$pct_hisp, imp4 = imp$imputations$imp4$pct_hisp, 
                                           imp5 = imp$imputations$imp1$pct_hisp))
regset$pct_wnh_imp <- rowMeans(data.frame(imp1 = imp$imputations$imp1$pct_wnh, imp2 = imp$imputations$imp2$pct_wnh,
                                          imp3 = imp$imputations$imp3$pct_wnh, imp4 = imp$imputations$imp4$pct_wnh, 
                                          imp5 = imp$imputations$imp1$pct_wnh))
regset$pct_asian_imp <- rowMeans(data.frame(imp1 = imp$imputations$imp1$pct_asian, imp2 = imp$imputations$imp2$pct_asian,
                                          imp3 = imp$imputations$imp3$pct_asian, imp4 = imp$imputations$imp4$pct_asian, 
                                          imp5 = imp$imputations$imp1$pct_asian))
regset$med_income.th_imp <- rowMeans(data.frame(imp1 = imp$imputations$imp1$med_income.th, imp2 = imp$imputations$imp2$med_income.th,
                                                imp3 = imp$imputations$imp3$med_income.th, imp4 = imp$imputations$imp4$med_income.th, 
                                                imp5 = imp$imputations$imp1$med_income.th))
regset$med_value.th_imp <- rowMeans(data.frame(imp1 = imp$imputations$imp1$med_value.th, imp2 = imp$imputations$imp2$med_value.th,
                                               imp3 = imp$imputations$imp3$med_value.th, imp4 = imp$imputations$imp4$med_value.th, 
                                               imp5 = imp$imputations$imp1$med_value.th))
regset$own_rate_imp <- rowMeans(data.frame(imp1 = imp$imputations$imp1$own_rate, imp2 = imp$imputations$imp2$own_rate,
                                           imp3 = imp$imputations$imp3$own_rate, imp4 = imp$imputations$imp4$own_rate, 
                                           imp5 = imp$imputations$imp1$own_rate))
regset$vacancy_rate_imp <- rowMeans(data.frame(imp1 = imp$imputations$imp1$vacancy_rate, imp2 = imp$imputations$imp2$vacancy_rate,
                                               imp3 = imp$imputations$imp3$vacancy_rate, imp4 = imp$imputations$imp4$vacancy_rate, 
                                               imp5 = imp$imputations$imp1$vacancy_rate))
regset$density_imp <- rowMeans(data.frame(imp1 = imp$imputations$imp1$density, imp2 = imp$imputations$imp2$density,
                                          imp3 = imp$imputations$imp3$density, imp4 = imp$imputations$imp4$density, 
                                          imp5 = imp$imputations$imp1$density))
regset$population.th_imp <- rowMeans(data.frame(imp1 = imp$imputations$imp1$population.th, imp2 = imp$imputations$imp2$population.th,
                                                imp3 = imp$imputations$imp3$population.th, imp4 = imp$imputations$imp4$population.th, 
                                                imp5 = imp$imputations$imp1$population.th))

summary(regset[,c("pct_black", "pct_black_imp", "pct_hisp", "pct_hisp_imp", "pct_wnh", "pct_wnh_imp",
                  "pct_asian", "pct_asian_imp",
                  "med_income.th", "med_income.th_imp", "med_value.th", "med_value.th_imp", 
                  "own_rate", "own_rate_imp", "vacancy_rate", "vacancy_rate_imp", "density", "density_imp",
                  "population.th", "population.th_imp")])

# values seem ok; replace original variables
regset$pct_black <- regset$pct_black_imp
regset$pct_hisp <- regset$pct_hisp_imp
regset$pct_wnh <- regset$pct_wnh_imp
regset$pct_asian <- regset$pct_asian_imp
regset$med_income.th <- regset$med_income.th_imp
regset$med_value.th <- regset$med_value.th_imp
regset$population.th <- regset$population.th_imp
regset$own_rate <- regset$own_rate_imp
regset$vacancy_rate <- regset$vacancy_rate_imp
regset$density <- regset$density_imp

# put jurupa valley back as it was
regset[regset$location=="JURUPA VALLEY",] <- jurupa

regset <- regset[,!names(regset) %in% c("pct_black_imp", "pct_hisp_imp", "pct_wnh_imp", "pct_asian_imp",
                                        "med_income.th_imp",  
                                        "med_value.th_imp", "population.th_imp", "own_rate_imp", 
                                        "vacancy_rate_imp", "density_imp")]

# create past latino electoral success variable (from ceda) 
ceda <- read.csv("output/temp/ceda_agg.csv")
ceda_sf <- read.csv("output/temp/ceda_agg_sf.csv")
ceda <- rbind(ceda, ceda_sf)

# get prop_l_elected_past_two variable from ceda dataset 
ceda$GEO_id <- as.numeric(paste0("6", str_pad(ceda$place, width = 5, pad = "0")))
ceda <- na.omit(ceda[,c("GEO_id", "year", "n_elected", "n_elected_l", "n_elected_a", "n_elected_b", "n_elected_w")])

# expand ceda dataset to yearly 
# function to assign average of last two elections to every year
var <- "prop_latino_elected"
stretch <- function(var) {
  base <- expand.grid(GEO_id = unique(ceda$GEO_id), year = c(1998:2019))
  base <- merge(base, ceda, by = c("GEO_id", "year"), all.x = TRUE)
  
  newvar <- paste0(var, "_past12")
  ids <- unique(base$GEO_id)
  for (i in c(1:length(ids))) {
    for (y in c(2010:2019)) {
      base[base$GEO_id==ids[i] & base$year==y, newvar] <- 
        sum(base[base$GEO_id==ids[i] & base$year %in% c((y-12):(y-1)), var], na.rm = TRUE)
    }
  }
  base <- base[,c("GEO_id", "year", var, newvar)]
  return(base)
}

tomerge1 <- stretch(var = "n_elected")
tomerge2 <- stretch(var = "n_elected_l")
tomerge3 <- stretch(var = "n_elected_a")
tomerge4 <- stretch(var = "n_elected_b")
tomerge5 <- stretch(var = "n_elected_w")

# merge
regset <- merge(regset, tomerge1[,c("GEO_id", "year", "n_elected_past12")], by = c("GEO_id", "year"), all.x = TRUE)
regset <- merge(regset, tomerge2[,c("GEO_id", "year", "n_elected_l_past12")], by = c("GEO_id", "year"), all.x = TRUE)
regset <- merge(regset, tomerge3[,c("GEO_id", "year", "n_elected_a_past12")], by = c("GEO_id", "year"), all.x = TRUE)
regset <- merge(regset, tomerge4[,c("GEO_id", "year", "n_elected_b_past12")], by = c("GEO_id", "year"), all.x = TRUE)
regset <- merge(regset, tomerge5[,c("GEO_id", "year", "n_elected_w_past12")], by = c("GEO_id", "year"), all.x = TRUE)

# make proportions
regset$prop_latino_elected_past12 <- regset$n_elected_l_past12/(regset$n_elected_past12) 
regset$prop_asian_elected_past12 <- regset$n_elected_a_past12/(regset$n_elected_past12) 
regset$prop_black_elected_past12 <- regset$n_elected_b_past12/(regset$n_elected_past12) 
regset$prop_white_elected_past12 <- regset$n_elected_w_past12/(regset$n_elected_past12) 

subset(regset, location == "LOS ANGELES")
subset(regset, location == "SAN FRANCISCO")

# code most underrepresented minority group 
ids <- unique(regset$GEO_id[!regset$location %in% c("BUELLTON", "EXETER")])
# exclude these cities because they break the loop -- no housing data over time 

# set to 2019 for purpose of pulling current city data
regset$year_switch2 <- ifelse(regset$year_switch>2019 | is.na(regset$year_switch) | regset$year_switch<2010, 2019, regset$year_switch) 
nrow(subset(regset, is.na(year_switch2)))

regset$min.group <- NA
for (i in c(1:length(ids))) {
  # past electoral success as of time of switch
  rep <- 100 * regset[regset$GEO_id==ids[i] & regset$year==(regset$year_switch2), c("prop_latino_elected_past12",
                                                                                 "prop_asian_elected_past12", "prop_black_elected_past12")]
  # population over pretreatment period up until that time
  pop <- regset[regset$GEO_id==ids[i] & regset$year==(regset$year_switch2), c("pct_hisp", "pct_asian", "pct_black")]
  df <- data.frame(pop = t(pop), rep = t(rep))
  names(df) <- c("pct_pop", "prop_council")
  row.names(df) <- c( "hisp", "asian", "black")
  keep <- df[df$pct_pop>=20,]
  keep$diff.pop.rep <- (keep$prop_council / keep$pct_pop) * 100
  min.group <- row.names(keep)[keep$diff.pop.rep==min(keep$diff.pop.rep, na.rm = TRUE) & keep$diff.pop.rep<=85]
  regset$min.group[regset$GEO_id==ids[i]] <- ifelse(length(min.group)>0, min.group, "none")
}
table(regset$min.group)

# check for duplicates where electoral success matches
flippers <- regset %>% group_by(location) %>%
  filter(length(unique(min.group)) > 1)

# define subset 
regsub <- regset[regset$switcher==1 & regset$treat==0,] 
means <- regsub %>% dplyr::group_by(GEO_id) %>%
  dplyr::summarise(population.th = mean(population.th, na.rm = TRUE),
            pct_hisp = mean(pct_hisp, na.rm = TRUE), 
            pct_wnh = mean(pct_wnh, na.rm = TRUE), 
            pct_asian = mean(pct_asian, na.rm = TRUE),
            pct_black = mean(pct_black, na.rm = TRUE))

# subset to places with >50k population 
samp1 <- means$GEO_id[means$population.th>=50]

regset$switcher2 <- 0
regset$switcher2[regset$GEO_id %in% samp1 & !is.na(regset$min.group) & !is.na(regset$year_switch) & regset$min.group!="none"] <- 1

vars <-                 c("location", "year", "pct_wnh", "pct_black", "pct_hisp", "pct_asian", 
                          "prop_white_elected_past12",  "prop_black_elected_past12",  "prop_latino_elected_past12",
                          "prop_asian_elected_past12", "min.group", "year_switch")
regset[regset$location=="ALHAMBRA", vars]

# code success of minority group
regset$prop_min_elected_past12 <- NA
regset$prop_min_elected_past12[regset$min.group=="asian" & !is.na(regset$min.group)] <- regset$prop_asian_elected_past12[regset$min.group=="asian" & !is.na(regset$min.group)]
regset$prop_min_elected_past12[regset$min.group=="black" & !is.na(regset$min.group)] <- regset$prop_black_elected_past12[regset$min.group=="black" & !is.na(regset$min.group)] 
regset$prop_min_elected_past12[regset$min.group=="hisp" & !is.na(regset$min.group)] <- regset$prop_latino_elected_past12[regset$min.group=="hisp" & !is.na(regset$min.group)]
regset$prop_min_elected_past12[regset$min.group=="white" & !is.na(regset$min.group)] <- regset$prop_white_elected_past12[regset$min.group=="white" & !is.na(regset$min.group)]

# check
check2 <- regset[regset$min.group=="white",] %>% dplyr::group_by(location) %>% dplyr::summarise(pct_hisp = mean(pct_hisp),
                                                      pct_wnh = mean(pct_wnh),
                                                      pct_asian = mean(pct_asian),
                                                      pct_black = mean(pct_black),
                                                      prop_latino_elected_past12 = mean(prop_latino_elected_past12),
                                                      prop_white_elected_past12 = mean(prop_white_elected_past12),
                                                      prop_asian_elected_past12 = mean(prop_asian_elected_past12),
                                                      prop_black_elected_past12 = mean(prop_black_elected_past12))



# code dominant group on council 
regset$max.group <- NA
regset$max.group_2019 <- NA
regset$max.group_2010 <- NA

for (i in 1:length(ids)) {
  # past electoral success as of time of switch
  rep <- 100 * regset[regset$GEO_id==ids[i] & regset$year==(regset$year_switch2), c("prop_latino_elected_past12", "prop_white_elected_past12",
                                                                                       "prop_asian_elected_past12", "prop_black_elected_past12")]
  # population over pretreatment period up until that time
  pop <- regset[regset$GEO_id==ids[i] & regset$year==(regset$year_switch2), c("pct_hisp", "pct_wnh", "pct_asian", "pct_black")]
  df <- data.frame(pop = t(pop), rep = t(rep))
  names(df) <- c("pct_pop", "prop_council")
  row.names(df) <- c("hisp", "white", "asian", "black")
  regset$max.group[regset$GEO_id==ids[i]] <- row.names(df)[df$prop_council==max(df$prop_council)]
}

for (i in 1:length(ids)) {
  # past electoral success as of time of switch
  rep <- 100 * regset[regset$GEO_id==ids[i] & regset$year==2019, c("prop_latino_elected_past12", "prop_white_elected_past12",
                                                                                    "prop_asian_elected_past12", "prop_black_elected_past12")]
  # population over pretreatment period up until that time
  pop <- regset[regset$GEO_id==ids[i] & regset$year==2019, c("pct_hisp", "pct_wnh", "pct_asian", "pct_black")]
  df <- data.frame(pop = t(pop), rep = t(rep))
  names(df) <- c("pct_pop", "prop_council")
  row.names(df) <- c("hisp", "white", "asian", "black")
  regset$max.group_2019[regset$GEO_id==ids[i]] <- row.names(df)[df$prop_council==max(df$prop_council)]
}


for (i in 1:length(ids)) {
  # past electoral success as of time of switch
  rep <- 100 * regset[regset$GEO_id==ids[i] & regset$year==2010, c("prop_latino_elected_past12", "prop_white_elected_past12",
                                                                   "prop_asian_elected_past12", "prop_black_elected_past12")]
  # population over pretreatment period up until that time
  pop <- regset[regset$GEO_id==ids[i] & regset$year==2010, c("pct_hisp", "pct_wnh", "pct_asian", "pct_black")]
  df <- data.frame(pop = t(pop), rep = t(rep))
  names(df) <- c("pct_pop", "prop_council")
  row.names(df) <- c("hisp", "white", "asian", "black")
  regset$max.group_2010[regset$GEO_id==ids[i]] <- row.names(df)[df$prop_council==max(df$prop_council)]
}

length(unique(subset(regset, max.group_2010 != max.group_2019)$location))/length(unique(regset$location)) # .11
length(unique(subset(regset, max.group_2010 != max.group_2019 & switcher==1 & year_switch<2019)$location))/length(unique(subset(regset, switcher==1 & year_switch<2019)$location)) # .9
length(unique(subset(regset, max.group_2010 != max.group_2019 & switcher==0 )$location))/length(unique(subset(regset, switcher==0)$location)) # .12


table(subset(regset, switcher==1)$year_switch)

sort(table(regset$location))
unique(subset(regset, is.na(max.group_2010))$location)
length(unique(subset(regset, is.na(max.group_2019))$location))

table(regset$max.group)
table(regset$max.group_2019)
table(regset$max.group_2010)


# check for duplicates where electoral success matches
flippers <- regset %>% group_by(location) %>%
  filter(length(unique(max.group)) > 1)
subset(flippers, year == year_switch2)[, c("location", "prop_white_elected_past12", "prop_latino_elected_past12",
                                         "prop_black_elected_past12","prop_asian_elected_past12", "max.group")]
subset(regset, location=="WESTMORLAND")

# code majority group based on average dominance over panel
# subgroup cities
regset[regset$location=="GARDEN GROVE",]$max.group <- "white" 
regset[regset$location=="SANTA MARIA",]$max.group <- "white"
regset[regset$location=="UNION CITY",]$max.group <- "hisp"

# remaining cities
regset[regset$location=="BARSTOW",]$max.group <- "white" 
regset[regset$location=="DIAMOND BAR",]$max.group <- "white" 
regset[regset$location=="FREMONT",]$max.group <- "white" 
regset[regset$location=="HERCULES",]$max.group <- "black" 
regset[regset$location=="HILLSBOROUGH TOWN",]$max.group <- "white" 
regset[regset$location=="KERMAN",]$max.group <- "white" 
regset[regset$location=="LA HABRA",]$max.group <- "white" 
regset[regset$location=="LEMON GROVE",]$max.group <- "white" 
regset[regset$location=="MILPITAS",]$max.group <- "white"
regset[regset$location=="SAN JOSE",]$max.group <- "white" 
regset[regset$location=="WESTMORLAND",]$max.group <- "white" 


# check
prop.table(table(regset$max.group))

# print unique min & max combinations
unique(regset[regset$switcher2==1, c("min.group", "max.group")])
unique(regset$location[regset$min.group=="asian" & regset$max.group=="asian" & regset$switcher2==1])

# remove alhambra from subset because both majority and minority get assigned asian by our decision rules
# (there is no clear dominant majority in this city)
regset$switcher2[regset$location=="ALHAMBRA"] <- 0

# recode a few values to 0 when cities were not incorporated
regset[regset$location=="JURUPA VALLEY" & regset$year %in% c(2010, 2011), c("multi", "single", "total", "l.multi", "l.single", "l.total")] <- NA
regset[regset$location=="EASTVALE" & regset$year %in% c(2010), c("multi", "single", "total", "l.multi", "l.single", "l.total")] <- NA


# Table 1
# all switchers
prop.table(table(subset(regset, switcher==1)$max.group))
prop.table(table(subset(regset, switcher==1)$min.group))

# subgroup
prop.table(table(subset(regset, switcher2==1)$max.group))
prop.table(table(subset(regset, switcher2==1)$min.group))

sub<-subset(regset, switcher2==1& year==2019)

sub[order(sub$year_switch),"location"]
sub$year_switch
order(subset(regset, switcher2==1)$location, subset(regset, switcher2==1)$year_switch2)

# keep only useful variables 
keeps <- c("location", "GEO_id", "year", "treat", "switcher", "switcher2", "year_switch",
           "multi", "single", "l.multi", "l.single", "l.total",
           "pct_asian", "pct_black", "pct_hisp", "pct_wnh", "max.group", "min.group", 
           "prop_asian_elected_past12", "prop_black_elected_past12", "prop_latino_elected_past12",
           "prop_white_elected_past12", "prop_min_elected_past12",
           "H_citytract_NHW", "population.th", "own_rate", "vacancy_rate", "med_value.th", "med_income.th",
           "density")
regset <- regset[,keeps]

 # save dataset
write.csv(regset, "output/final/housing_agg_fresh.csv", row.names = FALSE)



