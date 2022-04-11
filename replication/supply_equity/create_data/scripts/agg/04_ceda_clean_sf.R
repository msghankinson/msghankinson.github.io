############################################################################
# This script cleans and combines data on California city council
# elections
# Processing San Francisco separately because it is both city and
# county 
# Downloaded from: http://csus-dspace.calstate.edu/handle/10211.3/210187
############################################################################

rm(list = ls())

# install packages
# install.packages("readxl")

# load packages
library("readxl")

# set working directory 
setwd("") # set to create_data
setwd("raw/ceda/")

# load datasets
df98 <- data.frame(read_excel("CEDA1998Data.xls", sheet = "Candidates1998"))
df99 <- data.frame(read_excel("CEDA1999Data.xls", sheet = "Candidates1999"))
df00 <- data.frame(read_excel("CEDA2000Data.xls", sheet = "Candidates2000"))
df01 <- data.frame(read_excel("CEDA2001Data.xls", sheet = "Candidates2001"))
df02 <- data.frame(read_excel("CEDA2002Data.xls", sheet = "Candidates2002"))
df03 <- data.frame(read_excel("CEDA2003Data.xls", sheet = "Candidates2003"))
df04 <- data.frame(read_excel("CEDA2004Data.xls", sheet = "Candidates2004"))
df05 <- data.frame(read_excel("CEDA2005Data.xls", sheet = "Candidates2005"))
df06 <- data.frame(read_excel("CEDA2006Data.xls", sheet = "Candidates2006"))
df07 <- data.frame(read_excel("CEDA2007Data.xls", sheet = "Candidates2007"))
df08 <- data.frame(read_excel("CEDA2008Data.xls", sheet = "Candidates2008"))
df09 <- data.frame(read_excel("CEDA2009Data.xls", sheet = "Candidates2009"))
df10 <- data.frame(read_excel("CEDA2010Data.xls", sheet = "Candidates2010"))
df11 <- data.frame(read_excel("CEDA2011Data.xlsx", sheet = "Candidates2011"))
df12 <- data.frame(read_excel("CEDA2012Data.xlsx", sheet = "Candidates2012"))
df13 <- data.frame(read_excel("CEDA2013Data.xlsx", sheet = "Candidates2013"))
df14 <- data.frame(read_excel("CEDA2014Data.xlsx", sheet = "candidates2014"))
df15 <- data.frame(read_excel("CEDA2015Data.xlsx", sheet = "candidates2015"))
df16 <- data.frame(read_excel("CEDA2016Data.xlsx", sheet = "Candidates_2016"))
df17 <- data.frame(read_excel("CEDA2017Data.xlsx", sheet = "Candidates_2017"))
df18 <- data.frame(read_excel("CEDA2018Data.xlsx", sheet = "Candidates_2018"))
df19 <- data.frame(read_excel("CEDA2019Data.xlsx", sheet = "Candidates_2019"))

# keep useful variables, standardize and stack
keepvars <- c("RecordID", "CNTYNAME", "YEAR", "DATE", "OFFICE", 
              "LAST", "BALDESIG", "CAND.", "VOTES", "PLACE",
              "WRITEIN", "TOTVOTES", "Multi_RaceID")
df98 <- df98[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df99 <- df99[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df00 <- df00[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df01 <- df01[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df02 <- df02[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df03 <- df03[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df04 <- df04[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df05 <- df05[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df06 <- df06[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df07 <- df07[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df08 <- df08[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df09 <- df09[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df10 <- df10[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df11 <- df11[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df12 <- df12[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df13 <- df13[,c(keepvars, "INC", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df14 <- df14[,c(keepvars, "INC", "elected", "FIRST", "VOTES_sum", "Percent")]  
df15 <- df15[,c(keepvars, "INC", "elected", "FIRST", "SUMVOTES", "PERCENT")]  
df16 <- df16[,c(keepvars, "INCUMB", "ELECTED", "FIRST", "SUMVOTES", "PERCENT")]  
df17 <- df17[,c(keepvars, "INC", "elected", "FIRST", "SUMVOTES", "PERCENT")]  
df18 <- df18[,c(keepvars, "INC", "ELECTED", "First", "VOTES_sum", "PERCENT")]  
df19 <- df19[,c(keepvars, "INCUMB", "ELECTED", "First", "SUMVOTES", "PERCENT")]  

newnames <- c("recordid", "county", "year", "date", "office", "surname", 
              "prof", "ncand", "votes", "location", "writein", "totalvotes", 
              "raceid", "incumb", "elected", "first", "sumvotes", "percent")

names(df98) <- names(df99) <- names(df00) <- names(df01) <- names(df02) <-
  names(df03) <- names(df04) <- names(df05) <- names(df06) <- names(df07) <-
  names(df08) <- names(df09) <- names(df10) <- names(df11) <- names(df12) <- names(df13) <- 
  names(df14) <- names(df15) <- names(df16) <- names(df17) <- names(df18) <- names(df19) <- newnames

df <- rbind(df98, df99, df00, df01, df02, df03, df04, df05, df06, df07,
  df08, df09, df10, df11, df12, df13, df14, df15, df16, df17, df18, df19)

# restrict to city council races
df <- df[df$county == "SAN FRANCISCO", ]
table(df$office)

df <- df[df$office %in% c("COUNTY SUPERVISOR", "COUNTY COUNCIL", "County Supervisor"),]
head(df)
table(df$year)

# save data
write.csv(df, file = "../../output/temp/ceda_sf.csv", row.names = FALSE)
