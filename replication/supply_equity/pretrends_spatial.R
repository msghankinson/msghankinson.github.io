####################################################################################
# This script generates the following tables and figures in Hankinson and Magazinnik (2022)

### Main Text
# Figure 2: Difference in Logged Total Units Approved (High Minority Block Groups Minus Low Minority Block Groups), by Treatment Status and Year Relative to First District Election 

### Appendix
# Figure C-10: Logged Total Units Approved by Treatment Status and Year Relative to First District Election, white and minority block groups
####################################################################################

# clear workspace 
rm(list = ls())

# install libraries
#install.packages("ggplot2")
#install.packages("tidyverse")

# load libraries
library("ggplot2")
library("tidyverse")

# set working directory 
setwd("") # your working directory here 

# load data 
df <- read.csv("housing_spatial.csv")

####################################################
# prepare data
####################################################

df <- na.omit(regset[,c("switcher", "nunits_total_z", "nunits_single_z", "nunits_multi_z", "treat", "city", "geoid", "year", 
                        "n_households", "pct_nlw", "pct_black", "pct_hisp", "own_rate", "vacancy_rate", 
                        "med_value_imp", "med_income_imp")])

# fill in year of switch
df$year_switch <- NA
df$year_switch[df$city=="Anaheim"] <- 2016
df$year_switch[df$city=="Escondido"] <- 2014
df$year_switch[df$city=="Santa Barbara"] <- 2015
df$year_switch[df$city=="Ventura"] <- 2018

# create terciles according to each city's relative standards
df$wnh.high.third <- df$wnh.low.third <- 0
cities <- unique(df$city)
for (i in 1:length(cities)) {
  city.means <- tapply(df$pct_nlw[df$city==cities[i] & df$treat==0], 
                       as.character(df$geoid[df$city==cities[i] & df$treat==0]), 
                       mean, na.rm = TRUE)
  high_third_city <- names(city.means)[city.means > quantile(city.means, .67)]
  low_third_city <- names(city.means)[city.means < quantile(city.means, .33)]
  df$wnh.high.third[df$geoid %in% high_third_city] <- 1
  df$wnh.low.third[df$geoid %in% low_third_city] <- 1
}

# aggregate by city: sum of housing in high and low minority block groups
df_agg <- df[df$wnh.high.third==1 | df$wnh.low.third==1,] %>% 
  group_by(city, year, wnh.high.third) %>% 
  summarise(nunits = sum(nunits_total_z),
            treat = mean(treat),
            year_switch = mean(year_switch))

df_agg_white <- df_agg[df_agg$wnh.high.third==1, c("city", "year", "nunits", "treat", "year_switch")]       
df_agg_minority <- df_agg[df_agg$wnh.high.third==0, c("city", "year", "nunits")]       

names(df_agg_white) <- c("city", "year", "nunits_white", "treat", "year_switch")
names(df_agg_minority) <- c("city", "year", "nunits_minority")

df_merged <- merge(df_agg_white, df_agg_minority, by = c("city", "year"), all.x = TRUE)

# log variables
df_merged$lnunits_minority <- log(df_merged$nunits_minority + 1)
df_merged$lnunits_white <- log(df_merged$nunits_white + 1)

# compute difference
df_merged$diff <- df_merged$nunits_minority - df_merged$nunits_white
df_merged$ldiff <- df_merged$lnunits_minority - df_merged$lnunits_white

####################################################
# functions
####################################################

# construct treatment and control sets 
construct.sets <- function(df, pre.years, post.years) {
  treat.set <- NULL
  control.set <- NULL
  treats <- unique(df$city[df$treat==1])
  for (i in c(1:length(treats))) {
    # treatment set 
    treat.year <- unique(df$year_switch[df$city==treats[i]])
    treat.set.i <- df[df$city==treats[i] & df$year %in% c((treat.year - pre.years):(treat.year + post.years)),]
    treat.set.i$t <- treat.set.i$year - treat.year
    treat.set <- rbind(treat.set, treat.set.i)
    # control set 
    control.set.i <- df[(is.na(df$year_switch) | df$year_switch>=(treat.year + post.years + 1)) & df$year %in% c((treat.year - pre.years):(treat.year + post.years)),]
    control.set.i$t <- control.set.i$year - treat.year
    control.set <- rbind(control.set, control.set.i)
  }
  return(list(treat.set = treat.set, control.set = control.set))
}

trendplot1 <- function(byvar, treat.set, control.set, labels, ylab, ylim = NULL) {
  t <- data.frame(y = treat.set[,byvar], x = treat.set$t) 
  t$t <- "t"
  
  c <- data.frame(y = control.set[,byvar], x = control.set$t) 
  c$t <- "c"
  
  # put them together
  toplot <- rbind(t, c)
  
  # plot
  toplot$t <- factor(toplot$t, levels = c("c", "t"))
  toplot$x <- as.numeric(as.character(toplot$x))
  alpha <- ifelse(toplot$t=="c", .5, .5)
  
  g <- ggplot(data = toplot, aes(x = x, y = y, colour = t, group = t)) + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + 
    stat_summary(fun.data = mean_cl_boot, geom = "line") + 
    xlab("Years to first district election") + 
    ylab(ylab) + 
    theme_minimal() + 
    theme(legend.position = "bottom") + 
    labs(color = "") +
    scale_color_manual(labels = labels, values = c("c" = "gray30", "t" = "red3")) +
    coord_cartesian(ylim = ylim) 
  print(g)
}

################################################################################
# Figure 2: Difference in Logged Total Units Approved (High Minority Block 
# Groups Minus Low Minority Block Groups), by Treatment Status and Year Relative 
# to First District Election 
################################################################################

out <- construct.sets(df = df_merged, pre.years = 4, post.years = 4)
trendplot1(byvar = "ldiff", 
           treat.set = out$treat.set, 
           control.set = out$control.set, 
           labels = c("Control", "Treated"),
           ylab = "Difference in logged total units approved",
           ylim = c(-5, 6))

################################################################################
# Figure C-10: Logged Total Units Approved by Treatment Status and Year Relative 
# to First District Election, white and minority block groups
################################################################################

trendplot1(byvar = "lnunits_white", 
                treat.set = out$treat.set, 
                control.set = out$control.set, 
                labels = c("Control", "Treated"),
                ylab = "Logged total units approved",
                ylim = c(0, 7.3))

trendplot1(byvar = "lnunits_minority", 
                treat.set = out$treat.set, 
                control.set = out$control.set, 
                labels = c("Control", "Treated"),
                ylab = "Logged total units approved",
                ylim = c(0, 7.3))