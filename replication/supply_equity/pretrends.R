##############################################################################
# This program generates the following figures in Hankinson and Magazinnik (2022)
# 1. Main text, Figure 1
# 2. Appendix Figure B-5
##############################################################################

# clear workspace 
rm(list = ls())

# install libraries
#install.packages("ggplot2")

# load libraries
library("ggplot2")

# set working directory
setwd("") # your working directory here 

# load data 
regset <- read.csv("housing_agg.csv")

# define subgroups: segregation
seg_ave <- tapply(regset$H_citytract_NHW[regset$switcher2==1 & regset$treat==0], 
                  as.character(regset$location[regset$switcher2==1 & regset$treat==0]), 
                  mean, na.rm = TRUE)
seg_high_third <- names(seg_ave)[seg_ave > quantile(seg_ave, probs = .67, na.rm = TRUE)]
seg_low_third <- names(seg_ave)[seg_ave < quantile(seg_ave, probs = .33, na.rm = TRUE)]
regset$seg.high.third <- ifelse(regset$location %in% seg_high_third, 1, 0) 
regset$seg.low.third <- ifelse(regset$location %in% seg_low_third, 1, 0) 

# define subgroups: majority population size   
regset$pct_max <- ifelse(regset$max.group=="asian", regset$pct_asian, 
                         ifelse(regset$max.group=="white", regset$pct_wnh,
                                ifelse(regset$max.group=="hisp", regset$pct_hisp,
                                       regset$pct_black)))

max_ave <- tapply(regset$pct_max[regset$switcher2==1 & regset$treat==0],  
                  as.character(regset$location[regset$switcher2==1 & regset$treat==0]), 
                  mean, na.rm = TRUE)
max_high_third <- names(max_ave)[max_ave > quantile(max_ave, probs = .67, na.rm = TRUE)]
max_low_third <- names(max_ave)[max_ave < quantile(max_ave, probs = .33, na.rm = TRUE)]
regset$max.high.third <- ifelse(regset$location %in% max_high_third, 1, 0)   
regset$max.low.third <- ifelse(regset$location %in% max_low_third, 1, 0)  

# define subgroups: majority's political power
regset$maj_elect <- (ifelse(regset$max.group=="asian", regset$prop_asian_elected_past12/regset$pct_asian, 
                            ifelse(regset$max.group=="white", regset$prop_white_elected_past12/regset$pct_wnh,
                                   ifelse(regset$max.group=="hisp", regset$prop_latino_elected_past12/regset$pct_hisp,
                                          regset$prop_black_elected_past12/regset$pct_black)))) * 100 

maj_elect_ave <- regset$maj_elect[regset$switcher2==1 & !is.na(regset$year_switch) & regset$year==(regset$year_switch - 1)]
names(maj_elect_ave) <- regset$location[regset$switcher2==1 & !is.na(regset$year_switch) & regset$year==(regset$year_switch - 1)]
maj_elect_high_third <- names(maj_elect_ave)[maj_elect_ave > quantile(maj_elect_ave, probs = .67, na.rm = TRUE)]
maj_elect_low_third <- names(maj_elect_ave)[maj_elect_ave < quantile(maj_elect_ave, probs = .33, na.rm = TRUE)]
regset$maj_elect.high.third <- ifelse(regset$location %in% maj_elect_high_third, 1, 0)   
regset$maj_elect.low.third <- ifelse(regset$location %in% maj_elect_low_third, 1, 0)  

####################################################
# prepare data 
####################################################

# remove observations with missing values 
df <- na.omit(regset[,c("switcher", "l.total", "l.single", "l.multi", "treat", "location", "year", "H_citytract_NHW",
                        "population.th", "pct_wnh", "pct_black", "pct_hisp", "own_rate", "vacancy_rate", "prop_min_elected_past12",
                        "med_value.th", "med_income.th", "switcher2", "max.low.third", "max.high.third", "seg.low.third", 
                        "seg.high.third", "maj_elect.high.third", "maj_elect.low.third", "year_switch")])

# sort data by city and year 
df <- df[order(df$location, df$year),]

####################################################
# define functions
####################################################

# construct treatment and control sets 
construct.sets <- function(df, pre.years, post.years) {
  treat.set <- NULL
  control.set <- NULL
  treats <- unique(df$location[df$treat==1])
  for (i in c(1:length(treats))) {
    # treatment set 
    treat.year <- unique(df$year_switch[df$location==treats[i]])
    treat.set.i <- df[df$location==treats[i] & df$year %in% c((treat.year - pre.years):(treat.year + post.years)),]
    treat.set.i$t <- treat.set.i$year - treat.year
    treat.set <- rbind(treat.set, treat.set.i)
    # control set 
    control.set.i <- df[df$year_switch>=(treat.year + post.years + 1) & df$year %in% c((treat.year - pre.years):(treat.year + post.years)),]
    control.set.i$t <- control.set.i$year - treat.year
    control.set <- rbind(control.set, control.set.i)
  }
  return(list(treat.set = treat.set, control.set = control.set))
}

# generate plots 
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
  alpha <- ifelse(toplot$t=="c", .1, .5)
  
  # compute ylim
  if (is.null(ylim)) {
    ylim <- c(0,(max(toplot$y) + .02))
  }  
  
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

####################################################
# generate plots 
####################################################

###### Figure 1: entire causally identified sample ######
out <- construct.sets(df[df$switcher2==1,], pre.years = 3, post.years = 3)
trendplot1(byvar = "l.multi", 
          treat.set = out$treat.set, 
          control.set = out$control.set, 
          labels = c("Control", "Treated"),
          ylab = "Logged multifamily units permitted",
          ylim = c(0, 4.5))


###### Figure B-5: top and bottom terciles, segregation ######
out <- construct.sets(df[df$switcher2==1 & df$seg.low.third==1,], pre.years = 3, post.years = 3)
trendplot1(byvar = "l.multi", 
          treat.set = out$treat.set, 
          control.set = out$control.set, 
          labels = c("Control", "Treated"),
          ylab = "Logged multifamily units permitted",
          ylim = c(0, 4.5))

out <- construct.sets(df[df$switcher2==1 & df$seg.high.third==1,], pre.years = 3, post.years = 3)
trendplot1(byvar = "l.multi", 
                treat.set = out$treat.set, 
                control.set = out$control.set, 
                labels = c("Control", "Treated"),
                ylab = "Logged multifamily units permitted",
                ylim = c(0, 4.5))

###### Figure B-5: top and bottom terciles, majority population ######
out <- construct.sets(df[df$switcher2==1 & df$max.low.third==1,], pre.years = 3, post.years = 3)
trendplot1(byvar = "l.multi", 
          treat.set = out$treat.set, 
          control.set = out$control.set, 
          labels = c("Control", "Treated"),
          ylab = "Logged multifamily permits",
          ylim = c(0, 4.5))

out <- construct.sets(df[df$switcher2==1 & df$max.high.third==1,], pre.years = 3, post.years = 3)
trendplot1(byvar = "l.multi", 
                treat.set = out$treat.set, 
                control.set = out$control.set, 
                labels = c("Control", "Treated"),
                ylab = "Logged multifamily units permitted",
                ylim = c(0, 4.5))

###### Figure B-5: top and bottom terciles, majority council control ######
out <- construct.sets(df[df$switcher2==1 & df$maj_elect.low.third==1,], pre.years = 3, post.years = 3)
trendplot1(byvar = "l.multi", 
          treat.set = out$treat.set, 
          control.set = out$control.set, 
          labels = c("Control", "Treated"),
          ylab = "Logged multifamily units permitted",
          ylim = c(0, 4.5))

out <- construct.sets(df[df$switcher2==1 & df$maj_elect.high.third==1,], pre.years = 3, post.years = 3)
trendplot1(byvar = "l.multi", 
                treat.set = out$treat.set, 
                control.set = out$control.set, 
                labels = c("Control", "Treated"),
                ylab = "Logged multifamily units permitted",
                ylim = c(0, 4.5))

