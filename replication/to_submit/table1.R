##############################################################################
# This program generates Table 1 in Hankinson and Magazinnik (2022)
##############################################################################

# clear workspace 
rm(list = ls())

# set working directory 
setwd("") # your working directory here 

# load data 
housing <- read.csv("housing_agg.csv")

# convert to factors
housing$max.group <- factor(housing$max.group, levels = c("asian", "black", "hisp", "white", "none"))
housing$min.group <- factor(housing$min.group, levels = c("asian", "black", "hisp", "white", "none"))

# subset down
switcher_sub <- housing[housing$switcher==1 & housing$treat==0,] # all switchers
switcher2_sub <- housing[housing$switcher2==1 & housing$treat==0,] # causally identified sample 

# all switchers: council-dominant majority 
formatC(prop.table(table(switcher_sub$max.group))["asian"], digits = 2, format = "f")
formatC(prop.table(table(switcher_sub$max.group))["black"], digits = 2, format = "f")
formatC(prop.table(table(switcher_sub$max.group))["hisp"], digits = 2, format = "f")
formatC(prop.table(table(switcher_sub$max.group))["white"], digits = 2, format = "f")
formatC(prop.table(table(switcher_sub$max.group))["none"], digits = 2, format = "f")

# all switchers: most underrepresented minority 
formatC(prop.table(table(switcher_sub$min.group))["asian"], digits = 2, format = "f")
formatC(prop.table(table(switcher_sub$min.group))["black"], digits = 2, format = "f")
formatC(prop.table(table(switcher_sub$min.group))["hisp"], digits = 2, format = "f")
formatC(prop.table(table(switcher_sub$min.group))["white"], digits = 2, format = "f")
formatC(prop.table(table(switcher_sub$min.group))["none"], digits = 2, format = "f")

# causally identified sample: council-dominant majority
formatC(prop.table(table(switcher2_sub$max.group))["asian"], digits = 2, format = "f")
formatC(prop.table(table(switcher2_sub$max.group))["black"], digits = 2, format = "f")
formatC(prop.table(table(switcher2_sub$max.group))["hisp"], digits = 2, format = "f")
formatC(prop.table(table(switcher2_sub$max.group))["white"], digits = 2, format = "f")
formatC(prop.table(table(switcher2_sub$max.group))["none"], digits = 2, format = "f")

# causally identified sample: most underrepresented minority 
formatC(prop.table(table(switcher2_sub$min.group))["asian"], digits = 2, format = "f")
formatC(prop.table(table(switcher2_sub$min.group))["black"], digits = 2, format = "f")
formatC(prop.table(table(switcher2_sub$min.group))["hisp"], digits = 2, format = "f")
formatC(prop.table(table(switcher2_sub$min.group))["white"], digits = 2, format = "f")
formatC(prop.table(table(switcher2_sub$min.group))["none"], digits = 2, format = "f")

