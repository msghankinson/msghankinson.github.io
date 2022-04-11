####################################################################################
# This script generates the following tables and figures in Hankinson and Magazinnik (2022)

### Main Text
# Table 2: Effect of Conversion to Single-Member Districts on Logged Multifamily Units Permitted

### Appendix
# Figure A-4: Distributions of Variables Used to Assess Conditional Effects
# Figure A-1: Proportion of California Cities with District Elections over Time
# Figure A-3: Treatment Status over Time
# Table B-8: Effect of Conversion to Single-Member Districts on Multifamily Units Permitted Scaled by Lagged Population
# Table B-9: Effect of Conversion to Single-Member Districts on Binary Outcome (Any Multifamily Units Permitted = 1)
# Figure B-8: Effect of Conversion to Single-Member Districts on Logged Multifamily Units Permitted, Estimated Using Fixed Effects Counterfactual Estimator (Liu, Wang, and Xu 2020)
# Table B-4: Effect of Conversion to Single-Member Districts on Logged Units Permitted, By Housing Type
# Table B-5: Effect of Conversion to Single-Member Districts on Logged Multifamily Units Permitted, Interacted with Segregation, Robustness to Alternative Specifications
# Table B-6: Effect of Conversion to Single-Member Districts on Logged Multifamily Units Permitted, Interacted with Majority Population, Robustness to Alternative Specifications
# Table B-7: Effect of Conversion to Single-Member Districts on Logged Multifamily Units Permitted, Interacted with Majority Control, Robustness to Alternative Specifications
# Figure B-7: Event Study Plots of Treatment Effects and Confidence Intervals
# Figure B-9: Goodman-Bacon Decomposition of the Effect of Single-Member Districts on Logged Multifamily Units Permitted
# Table A-2: Characteristics of Cities in Aggregate Analysis by Type 
####################################################################################

# clear workspace 
rm(list = ls())

# install libraries
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("panelView")
#install.packages("multiwayvcov")
#install.packages("lmtest")
#install.packages("stargazer")
#install.packages("bacondecomp")

# install fect package from github
#install.packages("devtools") 
#library("devtools")
#devtools::install_github('xuyiqing/fastplm') # dependency
#devtools::install_github('xuyiqing/fect')

# load libraries
library("ggplot2")
library("tidyverse")
library("panelView")
library("multiwayvcov")
library("lmtest")
library("stargazer")
library("fect")
library("bacondecomp")

# set working directory 
setwd("") # your working directory here 

# load data 
regset <- read.csv("housing_agg.csv")

# define colors 
red_mit = '#A31F34'
red_light = '#A9606C'
blue_mit = '#315485'
grey_light= '#C2C0BF'
grey_dark = '#8A8B8C'
black = '#353132'

####################################################################################
# prepare data
####################################################################################

# generate leading and lagging indicators for Granger plots
treats <- regset %>% arrange(location, year) %>% filter(treat==1) # get all treated observations
nonswitcher <- unique(regset$location[regset$switcher==0])
treats <- treats[!treats$location %in% nonswitcher,]
years <- treats %>% group_by(location) %>% summarise(treat_year = min(year)) # compute min by group -- should give us first treatment year
years$treat_pre3 <- years$treat_year - 3 # construct other years from treated year
years$treat_pre2 <- years$treat_year - 2
years$treat_pre1 <- years$treat_year - 1
years$treat_post1 <- years$treat_year + 1
years$treat_post2 <- years$treat_year + 2
years$treat_post3 <- years$treat_year + 3

# loop through dataset, filling in relevant years
regset$treat_pre3 <- regset$treat_pre2 <- regset$treat_pre1 <- regset$treat_granger <- regset$treat_post1 <-
  regset$treat_post2 <- regset$treat_post2plus <- regset$treat_post3plus <- 0
for (i in c(1:nrow(years))) {
  regset$treat_pre3[regset$location==years$location[i] & regset$year==years$treat_pre3[i]] <- 1
  regset$treat_pre2[regset$location==years$location[i] & regset$year==years$treat_pre2[i]] <- 1
  regset$treat_pre1[regset$location==years$location[i] & regset$year==years$treat_pre1[i]] <- 1
  regset$treat_granger[regset$location==years$location[i] & regset$year==years$treat_year[i]] <- 1
  regset$treat_post1[regset$location==years$location[i] & regset$year==years$treat_post1[i]] <- 1
  regset$treat_post2[regset$location==years$location[i] & regset$year==years$treat_post2[i]] <- 1
  regset$treat_post2plus[regset$location==years$location[i] & regset$year>=years$treat_post2[i]] <- 1
  regset$treat_post3plus[regset$location==years$location[i] & regset$year>=years$treat_post3[i]] <- 1
}

# Create subgroups based on the pre-treatment average of the time series

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

regset$prop_max_elected_past12 <- ifelse(regset$max.group=="asian", regset$prop_asian_elected_past12, 
                                         ifelse(regset$max.group=="white", regset$prop_white_elected_past12,
                                                ifelse(regset$max.group=="hisp", regset$prop_latino_elected_past12,
                                                       regset$prop_black_elected_past12)))

maj_elect_ave <- regset$maj_elect[regset$switcher2==1 & !is.na(regset$year_switch) & regset$year==(regset$year_switch - 1)]
names(maj_elect_ave) <- regset$location[regset$switcher2==1 & !is.na(regset$year_switch) & regset$year==(regset$year_switch - 1)]
maj_elect_high_third <- names(maj_elect_ave)[maj_elect_ave > quantile(maj_elect_ave, probs = .67, na.rm = TRUE)]
maj_elect_low_third <- names(maj_elect_ave)[maj_elect_ave < quantile(maj_elect_ave, probs = .33, na.rm = TRUE)]
regset$maj_elect.high.third <- ifelse(regset$location %in% maj_elect_high_third, 1, 0)   
regset$maj_elect.low.third <- ifelse(regset$location %in% maj_elect_low_third, 1, 0)  

##################################################################################
# Figure A-4: Distributions of variables used to assess conditional effects
##################################################################################

seg_ave <- data.frame(x = seg_ave)
max_ave <- data.frame(x = max_ave)
maj_elect_ave <- data.frame(x = maj_elect_ave)

ggplot(seg_ave, aes(x = x)) +
  geom_histogram(colour = "black", fill = "grey85") + 
  geom_vline(xintercept = quantile(seg_ave$x, probs = .67, na.rm = T), col = "blue", lwd = 1) + 
  geom_vline(xintercept = quantile(seg_ave$x, probs = .33, na.rm = T), col = "blue", lwd = 1) + 
  xlab("Theil index") + 
  ylab("Frequency")

ggplot(max_ave, aes(x = x)) +
  geom_histogram(colour = "black", fill = "grey85") + 
  geom_vline(xintercept = quantile(max_ave$x, probs = .67, na.rm = T), col = "blue", lwd = 1) + 
  geom_vline(xintercept = quantile(max_ave$x, probs = .33, na.rm = T), col = "blue", lwd = 1) + 
  xlab("Percent of population") + 
  ylab("Frequency")

ggplot(maj_elect_ave, aes(x = x)) +
  geom_histogram(colour = "black", fill = "grey85") + 
  geom_vline(xintercept = quantile(maj_elect_ave$x, probs = .67, na.rm = T), col = "blue", lwd = 1) + 
  geom_vline(xintercept = quantile(maj_elect_ave$x, probs = .33, na.rm = T), col = "blue", lwd = 1) + 
  xlab(expression(paste("%", " of seats won ", "/", " % ", "of population"))) + 
  ylab("Frequency")

##################################################################################
# Figure A-1: District elections over time
##################################################################################

# compute means by year
year.means <- regset %>% group_by(year) %>% summarise(prop.sm = mean(treat, na.rm = TRUE))

# plot 
ggplot(data = year.means, aes(x = year, y = prop.sm)) +
  geom_line() + 
  geom_point() + 
  ylim(c(0, .4)) + 
  scale_x_continuous(breaks = c(2010:2019)) + 
  xlab("Year") + 
  ylab("Prop. of cities with single-member districts") + 
  theme_minimal()

##################################################################################
# Figure A-3: Treatment Status Over Time
##################################################################################

regsub <- regset[regset$switcher2==1 & !is.na(regset$switcher2),]

panelview(l.multi ~ treat, 
          data = regsub, 
          index = c("location", "year"), 
          xlab = "Year", 
          ylab = "City",
          background = "white",
          cex.axis.x = 14, 
          cex.lab = 14,
          cex.legend = 14,
          leave.gap = TRUE)

###########################################################################
# Table 2: Effect of Conversion to Single-Member Districts on Logged 
# Multifamily Units Permitted
###########################################################################

lm3.m <- lm(l.multi ~ treat + population.th + own_rate + vacancy_rate + pct_wnh + pct_hisp + pct_black +
                    med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
                  data = regsub)

lm3.m.max.s <- lm(l.multi ~ treat + treat:max.high.third + population.th  + own_rate + vacancy_rate +
                    med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
                  data = regsub,
                  subset = max.high.third==1 | max.low.third==1)

lm3.m.seg.s <- lm(l.multi ~ treat + treat:seg.low.third + population.th  + own_rate + vacancy_rate +
                    med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
                  data = regsub,
                  subset = seg.high.third==1 | seg.low.third==1)

lm3.m.maj_elect.s <- lm(l.multi ~ treat + treat:maj_elect.low.third + population.th  + own_rate + vacancy_rate +
                    med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
                  data = regsub,
                  subset = maj_elect.low.third==1 | maj_elect.high.third==1)

# variance-covariance matrix, clustered on city
vcov.lm3.m <- cluster.vcov(lm3.m, regsub$location)
vcov.lm3.m.max.s <- cluster.vcov(lm3.m.max.s, regsub$location[regsub$max.high.third==1 | regsub$max.low.third==1])
vcov.lm3.m.seg.s <- cluster.vcov(lm3.m.seg.s, regsub$location[regsub$seg.high.third==1 | regsub$seg.low.third==1])
vcov.lm3.m.maj_elect.s <- cluster.vcov(lm3.m.maj_elect.s, regsub$location[regsub$maj_elect.high.third==1 | regsub$maj_elect.low.third==1])

# standard errors, clustered on city
se.lm3.m <- coeftest(lm3.m, vcov = vcov.lm3.m)
se.lm3.m.max.s <- coeftest(lm3.m.max.s, vcov = vcov.lm3.m.max.s)
se.lm3.m.seg.s <- coeftest(lm3.m.seg.s, vcov = vcov.lm3.m.seg.s)
se.lm3.m.maj_elect.s <- coeftest(lm3.m.maj_elect.s, vcov = vcov.lm3.m.maj_elect.s)

# create table 
stargazer(lm3.m, lm3.m.seg.s, lm3.m.max.s, lm3.m.maj_elect.s, 
          keep.stat = c("n", "rsq"),
          keep = c("treat", "treat:seg.low.third", "treat:max.high.third", "treat:maj_elect.low.third", "pct_wnh", "pct_black", "pct_hisp", "population.th", "vacancy_rate", "own_rate", "med_value.th", "med_income.th", "prop_min_elected_past12"),
          order = c("treat", "treat:seg.low.third", "treat:max.high.third", "treat:maj_elect.low.third", "pct_wnh", "pct_black", "pct_hisp", "population.th", "vacancy_rate", "own_rate", "med_value.th", "med_income.th", "prop_min_elected_past12"),
          se = list(se.lm3.m[,2], se.lm3.m.seg.s[,2], se.lm3.m.max.s[,2], se.lm3.m.maj_elect.s[,2]),
          p = list(se.lm3.m[,4], se.lm3.m.seg.s[,4], se.lm3.m.max.s[,4], se.lm3.m.maj_elect.s[,4]),
          intercept.top = FALSE, intercept.bottom = TRUE,
          title = "Effect of Conversion to Single-Member Districts on Logged Multifamily Units Permitted, Interacted with City Characteristics",
          covariate.labels = c("Single-member districts", "SMD$*$Low segregation", "SMD$*$High majority population", "SMD$*$Low majority control",
                               "Percent non-Hispanic white", "Percent Black", "Percent Hispanic", "Population (thousands)", "Vacancy rate", "Home ownership rate", "Median home value (thousands)",
                               "Median income (thousands)", "Past minority representation"),
          align = TRUE, type = "text",
          dep.var.labels.include = F,
          column.labels = c("\\textit{H1}", "\\textit{H2}", "\\textit{H3}", "\\textit{H4}"),
          dep.var.caption = "",
          star.cutoffs = c(.05, .01, .001),
          no.space = TRUE,
          add.lines = list(c("City FE", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("Year FE", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("City-specific Trends", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}")))

# convert log to pct (for paper)
(exp(coef(se.lm3.m)["treat"]) - 1) * 100 
(exp(coef(se.lm3.m.seg.s)["treat"]) - 1) * 100 
(exp(coef(se.lm3.m.max.s)["treat"]) - 1) * 100 
(exp(coef(se.lm3.m.maj_elect.s)["treat"]) - 1) * 100 

# p-values 
se.lm3.m["treat",]
se.lm3.m.seg.s["treat",]
se.lm3.m.max.s["treat",]
se.lm3.m.maj_elect.s["treat",]

###########################################################################
# Table B-8: Effect of Conversion to Single-Member Districts on 
# Multifamily Units Permitted Scaled by Lagged Population
###########################################################################

regsub <- regsub %>%
  group_by(GEO_id) %>%
  mutate(lag.pop.th = dplyr::lag(population.th, n = 1, default = NA))

regsub$multi_cap <- regsub$multi/regsub$lag.pop.th
summary(regsub$multi_cap)

lm3.m.cap <- lm(multi_cap ~ treat + own_rate + vacancy_rate + pct_wnh + pct_hisp + pct_black +
              med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
            data = regsub)

lm3.m.max.s.cap <- lm(multi_cap ~ treat + treat:max.high.third + own_rate + vacancy_rate +
                    med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
                  data = regsub,
                  subset = max.high.third==1 | max.low.third==1)

lm3.m.seg.s.cap <- lm(multi_cap ~ treat + treat:seg.low.third + own_rate + vacancy_rate +
                    med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
                  data = regsub,
                  subset = seg.high.third==1 | seg.low.third==1)

lm3.m.maj_elect.s.cap <- lm(multi_cap ~ treat + treat:maj_elect.low.third + own_rate + vacancy_rate +
                          med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
                        data = regsub,
                        subset = maj_elect.low.third==1 | maj_elect.high.third==1)

# variance-covariance matrix, clustered on city
vcov.lm3.m.cap <- cluster.vcov(lm3.m.cap, regsub$location)
vcov.lm3.m.max.s.cap <- cluster.vcov(lm3.m.max.s.cap, regsub$location[regsub$max.high.third==1 | regsub$max.low.third==1])
vcov.lm3.m.seg.s.cap <- cluster.vcov(lm3.m.seg.s.cap, regsub$location[regsub$seg.high.third==1 | regsub$seg.low.third==1])
vcov.lm3.m.maj_elect.s.cap <- cluster.vcov(lm3.m.maj_elect.s.cap, regsub$location[regsub$maj_elect.high.third==1 | regsub$maj_elect.low.third==1])

# standard errors, clustered on city
se.lm3.m.cap <- coeftest(lm3.m.cap, vcov = vcov.lm3.m.cap)
se.lm3.m.max.s.cap <- coeftest(lm3.m.max.s.cap, vcov = vcov.lm3.m.max.s.cap)
se.lm3.m.seg.s.cap <- coeftest(lm3.m.seg.s.cap, vcov = vcov.lm3.m.seg.s.cap)
se.lm3.m.maj_elect.s.cap <- coeftest(lm3.m.maj_elect.s.cap, vcov = vcov.lm3.m.maj_elect.s.cap)

# create table
stargazer(lm3.m.cap, lm3.m.seg.s.cap, lm3.m.max.s.cap, lm3.m.maj_elect.s.cap, 
          keep.stat = c("n", "rsq"),
          keep = c("treat", "treat:seg.low.third", "treat:max.high.third", "treat:maj_elect.low.third", "pct_wnh", "pct_black", "pct_hisp", "vacancy_rate", "own_rate", "med_value.th", "med_income.th", "prop_min_elected_past12"),
          order = c("treat", "treat:seg.low.third", "treat:max.high.third", "treat:maj_elect.low.third", "pct_wnh", "pct_black", "pct_hisp", "vacancy_rate", "own_rate", "med_value.th", "med_income.th", "prop_min_elected_past12"),
          se = list(se.lm3.m.cap[,2], se.lm3.m.seg.s.cap[,2], se.lm3.m.max.s.cap[,2], se.lm3.m.maj_elect.s.cap[,2]),
          p = list(se.lm3.m.cap[,4], se.lm3.m.seg.s.cap[,4], se.lm3.m.max.s.cap[,4], se.lm3.m.maj_elect.s.cap[,4]),
          intercept.top = FALSE, intercept.bottom = TRUE,
          title = "Effect of Conversion to Single-Member Districts on Logged Multifamily Units Permitted, Interacted with City Characteristics",
          covariate.labels = c("Single-member districts", "SMD$*$Low segregation", "SMD$*$High majority population", "SMD$*$Low majority control",
                               "Percent non-Hispanic white", "Percent Black", "Percent Hispanic",  "Vacancy rate", "Home ownership rate", "Median home value (thousands)",
                               "Median income (thousands)", "Past minority representation"),
          align = TRUE, type = "text",
          dep.var.labels.include = F,
          column.labels = c("\\textit{H1}", "\\textit{H2}", "\\textit{H3}", "\\textit{H4}"),
          dep.var.caption = "",
          star.cutoffs = c(.05, .01, .001),
          no.space = TRUE,
          add.lines = list(c("City FE", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("Year FE", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("City-specific Trends", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}")))

###########################################################################
# Table B-9: Effect of Conversion to Single-Member Districts on 
# Binary Outcome (Any Multifamily Units Permitted = 1)
###########################################################################

regsub$permits.bin <- ifelse(regsub$multi > 0, 1, 0)

lm4.m <- lm(permits.bin ~ treat + population.th + own_rate + vacancy_rate + pct_wnh + pct_hisp + pct_black +
              med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
            data = regsub)

lm4.m.max.s <- lm(permits.bin ~ treat + treat:max.high.third + population.th  + own_rate + vacancy_rate +
                    med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
                  data = regsub,
                  subset = max.high.third==1 | max.low.third==1)

lm4.m.seg.s <- lm(permits.bin ~ treat + treat:seg.low.third + population.th  + own_rate + vacancy_rate +
                    med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
                  data = regsub,
                  subset = seg.high.third==1 | seg.low.third==1)

lm4.m.maj_elect.s <- lm(permits.bin ~ treat + treat:maj_elect.low.third + population.th  + own_rate + vacancy_rate +
                          med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
                        data = regsub,
                        subset = maj_elect.low.third==1 | maj_elect.high.third==1)

# variance-covariance matrix, clustered on city
vcov.lm4.m <- cluster.vcov(lm4.m, regsub$location)
vcov.lm4.m.max.s <- cluster.vcov(lm4.m.max.s, regsub$location[regsub$max.high.third==1 | regsub$max.low.third==1])
vcov.lm4.m.seg.s <- cluster.vcov(lm4.m.seg.s, regsub$location[regsub$seg.high.third==1 | regsub$seg.low.third==1])
vcov.lm4.m.maj_elect.s <- cluster.vcov(lm4.m.maj_elect.s, regsub$location[regsub$maj_elect.high.third==1 | regsub$maj_elect.low.third==1])

# standard errors, clustered on city
se.lm4.m <- coeftest(lm4.m, vcov = vcov.lm4.m)
se.lm4.m.max.s <- coeftest(lm4.m.max.s, vcov = vcov.lm4.m.max.s)
se.lm4.m.seg.s <- coeftest(lm4.m.seg.s, vcov = vcov.lm4.m.seg.s)
se.lm4.m.maj_elect.s <- coeftest(lm4.m.maj_elect.s, vcov = vcov.lm4.m.maj_elect.s)

# create table
stargazer(lm4.m, lm4.m.seg.s, lm4.m.max.s, lm4.m.maj_elect.s, 
          keep.stat = c("n", "rsq"),
          keep = c("treat", "treat:seg.low.third", "treat:max.high.third", "treat:maj_elect.low.third", "pct_wnh", "pct_black", "pct_hisp", "population.th", "vacancy_rate", "own_rate", "med_value.th", "med_income.th", "prop_min_elected_past12"),
          order = c("treat", "treat:seg.low.third", "treat:max.high.third", "treat:maj_elect.low.third", "pct_wnh", "pct_black", "pct_hisp", "population.th", "vacancy_rate", "own_rate", "med_value.th", "med_income.th", "prop_min_elected_past12"),
          se = list(se.lm4.m[,2], se.lm4.m.seg.s[,2], se.lm4.m.max.s[,2], se.lm4.m.maj_elect.s[,2]),
          p = list(se.lm4.m[,4], se.lm4.m.seg.s[,4], se.lm4.m.max.s[,4], se.lm4.m.maj_elect.s[,4]),
          intercept.top = FALSE, intercept.bottom = TRUE,
          title = "Effect of Conversion to Single-Member Districts on Logged Multifamily Units Permitted, Interacted with City Characteristics (Binary Outcome: Any Permits=1)",
          covariate.labels = c("Single-member districts", "SMD$*$Low segregation", "SMD$*$High majority population", "SMD$*$Low majority control",
                               "Percent non-Hispanic white", "Percent Black", "Percent Hispanic", "Population (thousands)", "Vacancy rate", "Home ownership rate", "Median home value (thousands)",
                               "Median income (thousands)", "Past minority representation"),
          align = TRUE, type = "text",
          dep.var.labels.include = F,
          column.labels = c("\\textit{H1}", "\\textit{H2}", "\\textit{H3}", "\\textit{H4}"),
          dep.var.caption = "",
          star.cutoffs = c(.05, .01, .001),
          no.space = TRUE,
          add.lines = list(c("City FE", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("Year FE", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("City-specific Trends", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}")))

##################################################################################
# Figure B-8: Effect of Conversion to Single-Member Districts on Logged 
# Multifamily Units Permitted, Estimated Using Fixed Effects Counterfactual Estimator 
# (Liu, Wang, and Xu 2020)
##################################################################################

# high segregation 
out.fect.seg <- fect(l.multi ~ treat + population.th + own_rate + vacancy_rate + pct_wnh + pct_hisp + pct_black +
                       med_value.th + med_income.th + prop_min_elected_past12, 
                     data = regsub[regsub$seg.high.third==1,], 
                     index = c("location", "year"), 
                     method = "fe", force = "two-way",
                     se = TRUE, parallel = TRUE, nboots = 500)

plot(out.fect.seg, main = "Estimated ATT (FEct)", ylab = "Effect of D on Y", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

# low majority population
out.fect.max <- fect(l.multi ~ treat + population.th + own_rate + vacancy_rate + pct_wnh + pct_hisp + pct_black +
                       med_value.th + med_income.th + prop_min_elected_past12, 
                     data = regsub[regsub$max.low.third==1,], 
                     index = c("location", "year"), 
                     method = "fe", force = "two-way",
                     se = TRUE, parallel = TRUE, nboots = 500)

plot(out.fect.max, main = "Estimated ATT (FEct)", ylab = "Effect of D on Y", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

# high majority control 
out.fect.maj_elect <- fect(l.multi ~ treat + population.th + own_rate + vacancy_rate + pct_wnh + pct_hisp + pct_black +
                             med_value.th + med_income.th + prop_min_elected_past12, 
                           data = regsub[regsub$maj_elect.high.third==1,], 
                           index = c("location", "year"), 
                           method = "fe", force = "two-way",
                           se = TRUE, parallel = TRUE, nboots = 500)

plot(out.fect.maj_elect, main = "Estimated ATT (FEct)", ylab = "Effect of D on Y", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

############################################################################
# Table B-4: Effect of Conversion to Single-Member Districts on Logged 
# Units Permitted, By Housing Type
############################################################################

## Total units
lm3.t.s <- lm(l.total ~ treat + population.th + pct_wnh + pct_black + pct_hisp + own_rate + vacancy_rate +
                med_value.th + med_income.th + prop_min_elected_past12 +  as.factor(year) + year*as.factor(location),
              data = regsub)

## Single-family units
lm3.s.s <- lm(l.single ~ treat +  population.th + pct_wnh + pct_black + pct_hisp + own_rate + vacancy_rate +
                med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
              data = regsub)

## Multifamily units
lm3.m.s <- lm(l.multi ~ treat + population.th + pct_wnh + pct_black + pct_hisp + own_rate + vacancy_rate +
                med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
              data = regsub)

# variance-covariance matrix, clustered on city
vcov.lm3.t.s <- cluster.vcov(lm3.t.s, regsub$location)
vcov.lm3.s.s <- cluster.vcov(lm3.s.s, regsub$location)
vcov.lm3.m.s <- cluster.vcov(lm3.m.s, regsub$location)

# standard errors, clustered on city
se.lm3.t.s <- coeftest(lm3.t.s, vcov = vcov.lm3.t.s)
se.lm3.s.s <- coeftest(lm3.s.s, vcov = vcov.lm3.s.s)
se.lm3.m.s <- coeftest(lm3.m.s, vcov = vcov.lm3.m.s)

# create table 
stargazer(lm3.t.s, lm3.s.s, lm3.m.s, 
          keep.stat = c("n", "rsq"),
          keep = c("treat", "pct_wnh", "pct_black", "pct_hisp", "population.th", "vacancy_rate", "own_rate", "med_value.th", "med_income.th", "prop_min_elected_past12"),
          order = c("treat", "pct_wnh", "pct_black", "pct_hisp", "population.th", "vacancy_rate", "own_rate", "med_value.th", "med_income.th", "prop_min_elected_past12"),
          se = list(se.lm3.t.s[,2], se.lm3.s.s[,2], se.lm3.m.s[,2]),
          p = list(se.lm3.t.s[,4], se.lm3.s.s[,4], se.lm3.m.s[,4]),
          intercept.top = FALSE, intercept.bottom = TRUE,
          title = "Effect of Conversion to Single-Member Districts on Logged Units Permitted, By Housing Type (Causally Identified Sample)",
          covariate.labels = c("Single-member districts", "Percent non-Hispanic white", "Percent Black", "Percent Hispanic",
                               "Population (thousands)",   "Vacancy rate", "Home ownership rate", "Median home value (thousands)",
                               "Median income (thousands)", "Past minority representation"),
          align = TRUE, type = "text",
          dep.var.labels.include = T, dep.var.labels = c("Total", "Single-Family", "Multifamily"),
          dep.var.caption = "",
          star.cutoffs = c( .05, .01, .001),
          no.space = TRUE,
          add.lines = list(c("City FE", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("Year FE", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("City-specific Trends", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}")))

############################################################################
# Table B-5: Effect of Conversion to Single-Member Districts on Logged 
# Multifamily Units Permitted, Interacted with Segregation, Robustness 
# to Alternative Specifications
############################################################################

# bivariate
lm3.m.seg.s.bi <- lm(l.multi ~ treat + treat:seg.low.third,
                     data = regsub,
                     subset = seg.high.third==1 | seg.low.third==1)

# fe
lm3.m.seg.s.fe <- lm(l.multi ~ treat + treat:seg.low.third + as.factor(year) + as.factor(location),
                     data = regsub,
                     subset = seg.high.third==1 | seg.low.third==1)

# tt no controls
lm3.m.seg.s.tt <- lm(l.multi ~ treat + treat:seg.low.third + as.factor(year) + year*as.factor(location),
                     data = regsub,
                     subset = seg.high.third==1 | seg.low.third==1)

# controls
lm3.m.seg.s.c <- lm(l.multi ~ treat + treat:seg.low.third + population.th  + own_rate + vacancy_rate +
                      med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + as.factor(location),
                    data = regsub,
                    subset = seg.high.third==1 | seg.low.third==1)

# variance-covariance matrix, clustered on city
vcov.lm3.m.seg.s.bi <- cluster.vcov(lm3.m.seg.s.bi, regsub$location[regsub$seg.high.third==1 | regsub$seg.low.third==1])
vcov.lm3.m.seg.s.fe <- cluster.vcov(lm3.m.seg.s.fe, regsub$location[regsub$seg.high.third==1 | regsub$seg.low.third==1])
vcov.lm3.m.seg.s.tt <- cluster.vcov(lm3.m.seg.s.tt, regsub$location[regsub$seg.high.third==1 | regsub$seg.low.third==1])
vcov.lm3.m.seg.s.c <- cluster.vcov(lm3.m.seg.s.c, regsub$location[regsub$seg.high.third==1 | regsub$seg.low.third==1])

# standard errors, clustered on city
se.lm3.m.seg.s.bi <- coeftest(lm3.m.seg.s.bi, vcov = vcov.lm3.m.seg.s.bi)
se.lm3.m.seg.s.fe <- coeftest(lm3.m.seg.s.fe, vcov = vcov.lm3.m.seg.s.fe)
se.lm3.m.seg.s.tt <- coeftest(lm3.m.seg.s.tt, vcov = vcov.lm3.m.seg.s.fe)
se.lm3.m.seg.s.c <- coeftest(lm3.m.seg.s.c, vcov = vcov.lm3.m.seg.s.c)

# create table 
stargazer(lm3.m.seg.s.bi, lm3.m.seg.s.fe, lm3.m.seg.s.tt, lm3.m.seg.s.c, lm3.m.seg.s,
          keep.stat = c("n", "rsq"),
          keep = c("treat", "treat:seg.low.third", "population.th", "vacancy_rate", "own_rate", "med_value.th", "med_income.th", "prop_min_elected_past12"),
          order = c("treat", "treat:seg.low.third", "population.th", "vacancy_rate", "own_rate", "med_value.th", "med_income.th", "prop_min_elected_past12"),
          se = list(se.lm3.m.seg.s.bi[,2], se.lm3.m.seg.s.fe[,2], se.lm3.m.seg.s.tt[,2], se.lm3.m.seg.s.c[,2], se.lm3.m.seg.s[,2]),
          p = list(se.lm3.m.seg.s.bi[,4], se.lm3.m.seg.s.fe[,4], se.lm3.m.seg.s.tt[,4], se.lm3.m.seg.s.c[,4], se.lm3.m.seg.s[,4]),
          intercept.top = FALSE, intercept.bottom = TRUE,
          title = "Effect of Conversion to Single-Member Districts on Logged Multifamily Units Permitted, Interacted with Segregation (Causally Identified Sample), Robustness to Alternative Specifications",
          covariate.labels = c("Single-member districts", "SMD$*$Low segregation",
                               "Population (thousands)", "Vacancy rate", "Home ownership rate", "Median home value (thousands)",
                               "Median income (thousands)", "Past minority representation"),
          align = TRUE, type = "text",
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          star.cutoffs = c(.05, .01, .001),
          no.space = TRUE,
          add.lines = list(c("City FE", "\\mc{No}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("Year FE", "\\mc{No}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("City-specific Trends", "\\mc{No}", "\\mc{No}", "\\mc{Yes}", "\\mc{No}", "\\mc{Yes}")))

############################################################################
# Table B-6: Effect of Conversion to Single-Member Districts on Logged 
# Multifamily Units Permitted, Interacted with Majority Population, 
# Robustness to Alternative Specifications
############################################################################

# bivariate
lm3.m.max.s.bi <- lm(l.multi ~ treat + treat:max.high.third,
                     data = regsub,
                     subset = max.high.third==1 | max.low.third==1)

# fe
lm3.m.max.s.fe <- lm(l.multi ~ treat + treat:max.high.third + as.factor(year) + as.factor(location),
                     data = regsub,
                     subset = max.high.third==1 | max.low.third==1)

# tt no controls
lm3.m.max.s.tt <- lm(l.multi ~ treat + treat:max.high.third + as.factor(year) + year*as.factor(location),
                     data = regsub,
                     subset = max.high.third==1 | max.low.third==1)

# controls
lm3.m.max.s.c <- lm(l.multi ~ treat + treat:max.high.third + population.th  + own_rate + vacancy_rate +
                      med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + as.factor(location),
                    data = regsub,
                    subset = max.high.third==1 | max.low.third==1)

# variance-covariance matrix, clustered on city
vcov.lm3.m.max.s.bi <- cluster.vcov(lm3.m.max.s.bi, regsub$location[regsub$max.high.third==1 | regsub$max.low.third==1])
vcov.lm3.m.max.s.fe <- cluster.vcov(lm3.m.max.s.fe, regsub$location[regsub$max.high.third==1 | regsub$max.low.third==1])
vcov.lm3.m.max.s.tt <- cluster.vcov(lm3.m.max.s.tt, regsub$location[regsub$max.high.third==1 | regsub$max.low.third==1])
vcov.lm3.m.max.s.c <- cluster.vcov(lm3.m.max.s.c, regsub$location[regsub$max.high.third==1 | regsub$max.low.third==1])

# standard errors, clustered on city
se.lm3.m.max.s.bi <- coeftest(lm3.m.max.s.bi, vcov = vcov.lm3.m.max.s.bi)
se.lm3.m.max.s.fe <- coeftest(lm3.m.max.s.fe, vcov = vcov.lm3.m.max.s.fe)
se.lm3.m.max.s.tt <- coeftest(lm3.m.max.s.tt, vcov = vcov.lm3.m.max.s.fe)
se.lm3.m.max.s.c <- coeftest(lm3.m.max.s.c, vcov = vcov.lm3.m.max.s.c)

# create table 
stargazer(lm3.m.max.s.bi, lm3.m.max.s.fe, lm3.m.max.s.tt, lm3.m.max.s.c, lm3.m.max.s,
          keep.stat = c("n", "rsq"),
          keep = c("treat", "treat:max.high.third",  "population.th", "vacancy_rate", "own_rate", "med_value.th", "med_income.th", "prop_min_elected_past12"),
          order = c("treat", "treat:max.high.third", "population.th", "vacancy_rate", "own_rate", "med_value.th", "med_income.th", "prop_min_elected_past12"),
          se = list(se.lm3.m.max.s.bi[,2], se.lm3.m.max.s.fe[,2], se.lm3.m.max.s.tt[,2], se.lm3.m.max.s.c[,2], se.lm3.m.max.s[,2]),
          p = list(se.lm3.m.max.s.bi[,4], se.lm3.m.max.s.fe[,4], se.lm3.m.max.s.tt[,4], se.lm3.m.max.s.c[,4], se.lm3.m.max.s[,4]),
          intercept.top = FALSE, intercept.bottom = TRUE,
          title = "Effect of Conversion to Single-Member Districts on Logged Multifamily Units Permitted, Interacted with Majority Population (Causally Identified Sample), Robustness to Alternative Model Specifications",
          covariate.labels = c("Single-member districts", "SMD$*$High majority population",
                               "Population (thousands)", "Vacancy rate", "Home ownership rate", "Median home value (thousands)",
                               "Median income (thousands)", "Past minority representation"),
          align = TRUE, type = "text",
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          star.cutoffs = c(.05, .01, .001),
          no.space = TRUE,
          add.lines = list(c("City FE", "\\mc{No}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("Year FE", "\\mc{No}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("City-specific Trends", "\\mc{No}", "\\mc{No}", "\\mc{Yes}", "\\mc{No}", "\\mc{Yes}")))

############################################################################
# Table B-7: Effect of Conversion to Single-Member Districts on Logged 
# Multifamily Units Permitted, Interacted with Majority Control, 
# Robustness to Alternative Specifications
############################################################################

# bivariate
lm3.m.maj_elect.s.bi <- lm(l.multi ~ treat + treat:maj_elect.low.third,
                     data = regsub,
                     subset = maj_elect.high.third==1 | maj_elect.low.third==1)

# fe
lm3.m.maj_elect.s.fe <- lm(l.multi ~ treat + treat:maj_elect.low.third + as.factor(year) + as.factor(location),
                     data = regsub,
                     subset = maj_elect.high.third==1 | maj_elect.low.third==1)

# tt no controls
lm3.m.maj_elect.s.tt <- lm(l.multi ~ treat + treat:maj_elect.low.third + as.factor(year) + year*as.factor(location),
                     data = regsub,
                     subset = maj_elect.high.third==1 | maj_elect.low.third==1)

# controls
lm3.m.maj_elect.s.c <- lm(l.multi ~ treat + treat:maj_elect.low.third + population.th  + own_rate + vacancy_rate +
                      med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + as.factor(location),
                    data = regsub,
                    subset = maj_elect.high.third==1 | maj_elect.low.third==1)

# variance-covariance matrix, clustered on city
vcov.lm3.m.maj_elect.s.bi <- cluster.vcov(lm3.m.maj_elect.s.bi, regsub$location[regsub$maj_elect.high.third==1 | regsub$maj_elect.low.third==1])
vcov.lm3.m.maj_elect.s.fe <- cluster.vcov(lm3.m.maj_elect.s.fe, regsub$location[regsub$maj_elect.high.third==1 | regsub$maj_elect.low.third==1])
vcov.lm3.m.maj_elect.s.tt <- cluster.vcov(lm3.m.maj_elect.s.tt, regsub$location[regsub$maj_elect.high.third==1 | regsub$maj_elect.low.third==1])
vcov.lm3.m.maj_elect.s.c <- cluster.vcov(lm3.m.maj_elect.s.c, regsub$location[regsub$maj_elect.high.third==1 | regsub$maj_elect.low.third==1])

# standard errors, clustered on city
se.lm3.m.maj_elect.s.bi <- coeftest(lm3.m.maj_elect.s.bi, vcov = vcov.lm3.m.maj_elect.s.bi)
se.lm3.m.maj_elect.s.fe <- coeftest(lm3.m.maj_elect.s.fe, vcov = vcov.lm3.m.maj_elect.s.fe)
se.lm3.m.maj_elect.s.tt <- coeftest(lm3.m.maj_elect.s.tt, vcov = vcov.lm3.m.maj_elect.s.fe)
se.lm3.m.maj_elect.s.c <- coeftest(lm3.m.maj_elect.s.c, vcov = vcov.lm3.m.maj_elect.s.c)

# create table 
stargazer(lm3.m.maj_elect.s.bi, lm3.m.maj_elect.s.fe, lm3.m.maj_elect.s.tt, lm3.m.maj_elect.s.c, lm3.m.maj_elect.s,
          keep.stat = c("n", "rsq"),
          keep = c("treat", "treat:maj_elect.low.third", "population.th", "vacancy_rate", "own_rate", "med_value.th", "med_income.th", "prop_min_elected_past12"),
          order = c("treat", "treat:maj_elect.low.third", "population.th", "vacancy_rate", "own_rate", "med_value.th", "med_income.th", "prop_min_elected_past12"),
          se = list(se.lm3.m.maj_elect.s.bi[,2], se.lm3.m.maj_elect.s.fe[,2], se.lm3.m.maj_elect.s.tt[,2], se.lm3.m.maj_elect.s.c[,2], se.lm3.m.maj_elect.s[,2]),
          p = list(se.lm3.m.maj_elect.s.bi[,4], se.lm3.m.maj_elect.s.fe[,4], se.lm3.m.maj_elect.s.tt[,4], se.lm3.m.maj_elect.s.c[,4], se.lm3.m.maj_elect.s[,4]),
          intercept.top = FALSE, intercept.bottom = TRUE,
          title = "Effect of Conversion to Single-Member Districts on Logged Multifamily Units Permitted, Interacted with Majority Control (Causally Identified Sample), Robustness to Alternative Specifications",
          covariate.labels = c("Single-member districts", "SMD$*$Low majority control",
                               "Population (thousands)", "Vacancy rate", "Home ownership rate", "Median home value (thousands)",
                               "Median income (thousands)", "Past minority representation"),
          align = TRUE, type = "text",
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          star.cutoffs = c(.05, .01, .001),
          no.space = TRUE,
          add.lines = list(c("City FE", "\\mc{No}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("Year FE", "\\mc{No}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("City-specific Trends", "\\mc{No}", "\\mc{No}", "\\mc{Yes}", "\\mc{No}", "\\mc{Yes}")))

############################################################################
# Figure B-7: Event Study Plots of Treatment Effects and Confidence Intervals
############################################################################

# segregation 
lm3.m.seg.g <- lm(l.multi ~ treat_pre3*seg.low.third + treat_pre2*seg.low.third + treat_pre1*seg.low.third +
                    treat_granger*seg.low.third + treat_post1*seg.low.third + treat_post2*seg.low.third + treat_post3plus*seg.low.third +
                    population.th  + own_rate + vacancy_rate + med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
                  data = regsub,
                  subset = seg.high.third==1 | seg.low.third==1)

# standard errors 
se.lm3.m.seg.g <- coeftest(lm3.m.seg.g, vcov=function(x) cluster.vcov(lm3.m.seg.g, subset(regsub, seg.high.third==1 | seg.low.third==1)$location))

vcov <- cluster.vcov(lm3.m.seg.g, subset(regsub, seg.high.third==1 | seg.low.third==1)$location)
vari <- c("treat_pre3", "treat_pre2" , "treat_pre1", "treat_granger", "treat_post1", "treat_post2", "treat_post3plus")
estimates <- coef(lm3.m.seg.g)[vari]
vari <- factor(vari, levels = c("treat_pre3", "treat_pre2" , "treat_pre1", "treat_granger", "treat_post1", "treat_post2", "treat_post3plus"), 
               ordered = TRUE)
sds <- sqrt(c( vcov["treat_pre3", "treat_pre3"], vcov["treat_pre2", "treat_pre2"], vcov["treat_pre1", "treat_pre1"],
               vcov["treat_granger", "treat_granger"], vcov["treat_post1", "treat_post1"], vcov["treat_post2", "treat_post2"], 
               vcov["treat_post3plus", "treat_post3plus"]))
toplot <- data.frame(vari = vari, estimates = estimates, sds = sds)

# compute confidence intervals
toplot <- mutate(toplot,
                 ci.lo.95 = estimates + qnorm(0.025) * sds,
                 ci.hi.95 = estimates + qnorm(0.975) * sds,
                 ci.lo.90 = estimates + qnorm(0.05) * sds,
                 ci.hi.90 = estimates + qnorm(0.95) * sds)

ggplot(toplot, aes(x = vari)) +
  geom_errorbar(aes(ymin = ci.lo.95, ymax = ci.hi.95), position = position_dodge(width = 0.2),
                data = toplot,
                width = .0, size = 0.35) +
  geom_errorbar(aes(ymin = ci.lo.90, ymax = ci.hi.90), position = position_dodge(width = 0.2),
                data = toplot,
                width = .0, size = 1.5) +
  geom_point(aes(y = estimates), position = position_dodge(width = 0.2),
             data = toplot, size = 4) + 
  geom_hline(aes(yintercept = 0), lty = 2) +
  ylab("Change in logged multifamily units permitted") +
  scale_x_discrete("", labels = c( "treat_pre3"="t-3", "treat_pre2"="t-2", "treat_pre1"="t-1", 
                                   "treat_granger"="t", "treat_post1"="t+1", "treat_post2"="t+2", "treat_post3plus"=">=t+3")) +
  theme_minimal() + 
  theme(text = element_text(size = 15), legend.position = "bottom")

# majority population 
lm3.m.max.g <- lm(l.multi ~ treat_pre3*max.high.third + treat_pre2*max.high.third + treat_pre1*max.high.third +
                    treat_granger*max.high.third + treat_post1*max.high.third + treat_post2*max.high.third + treat_post3plus*max.high.third +
                    population.th  + own_rate + vacancy_rate + med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
                  data = regsub,
                  subset = max.high.third==1 | max.low.third==1)

# standard errors 
se.lm3.m.max.g <- coeftest(lm3.m.max.g, vcov=function(x) cluster.vcov(lm3.m.max.g, subset(regsub, max.high.third==1 | max.low.third==1)$location))

vcov <- cluster.vcov(lm3.m.max.g, subset(regsub, max.high.third==1 | max.low.third==1)$location)
vari <- c("treat_pre3", "treat_pre2" , "treat_pre1", "treat_granger", "treat_post1", "treat_post2", "treat_post3plus")
estimates <- coef(lm3.m.max.g)[vari]
vari <- factor(vari, levels = c("treat_pre3", "treat_pre2" , "treat_pre1", "treat_granger", "treat_post1", "treat_post2", "treat_post3plus"), 
               ordered = TRUE)
sds <- sqrt(c( vcov["treat_pre3", "treat_pre3"], vcov["treat_pre2", "treat_pre2"], vcov["treat_pre1", "treat_pre1"],
               vcov["treat_granger", "treat_granger"], vcov["treat_post1", "treat_post1"], vcov["treat_post2", "treat_post2"], 
               vcov["treat_post3plus", "treat_post3plus"]))
toplot <- data.frame(vari = vari, estimates = estimates, sds = sds)

# compute confidence intervals
toplot <- mutate(toplot,
                 ci.lo.95 = estimates + qnorm(0.025) * sds,
                 ci.hi.95 = estimates + qnorm(0.975) * sds,
                 ci.lo.90 = estimates + qnorm(0.05) * sds,
                 ci.hi.90 = estimates + qnorm(0.95) * sds)

ggplot(toplot, aes(x = vari)) +
  geom_errorbar(aes(ymin = ci.lo.95, ymax = ci.hi.95), position = position_dodge(width = 0.2),
                data = toplot,
                width = .0, size = 0.35) +
  geom_errorbar(aes(ymin = ci.lo.90, ymax = ci.hi.90), position = position_dodge(width = 0.2),
                data = toplot,
                width = .0, size = 1.5) +
  geom_point(aes(y = estimates), position = position_dodge(width = 0.2),
             data = toplot, size = 4) + 
  geom_hline(aes(yintercept = 0), lty = 2) +
  ylab("Change in logged multifamily units permitted") +
  scale_x_discrete("", labels = c( "treat_pre3"="t-3", "treat_pre2"="t-2", "treat_pre1"="t-1", 
                                   "treat_granger"="t", "treat_post1"="t+1", "treat_post2"="t+2", "treat_post3plus"=">=t+3")) +
  theme_minimal() + 
  theme(text = element_text(size = 15), legend.position = "bottom")

# majority control 
lm3.m.maj_elect.g <- lm(l.multi ~ treat_pre3*maj_elect.low.third + treat_pre2*maj_elect.low.third + treat_pre1*maj_elect.low.third +
                    treat_granger*maj_elect.low.third + treat_post1*maj_elect.low.third + treat_post2*maj_elect.low.third + treat_post3plus*maj_elect.low.third +
                    population.th  + own_rate + vacancy_rate + med_value.th + med_income.th + prop_min_elected_past12 + as.factor(year) + year*as.factor(location),
                  data = regsub,
                  subset = maj_elect.high.third==1 | maj_elect.low.third==1)

# standard errors 
se.lm3.m.maj_elect.g <- coeftest(lm3.m.maj_elect.g, vcov=function(x) cluster.vcov(lm3.m.maj_elect.g, subset(regsub, maj_elect.high.third==1 | maj_elect.low.third==1)$location))

vcov <- cluster.vcov(lm3.m.maj_elect.g, subset(regsub, maj_elect.high.third==1 | maj_elect.low.third==1)$location)
vari <- c("treat_pre3", "treat_pre2" , "treat_pre1", "treat_granger", "treat_post1", "treat_post2", "treat_post3plus")
estimates <- coef(lm3.m.maj_elect.g)[vari]
vari <- factor(vari, levels = c("treat_pre3", "treat_pre2" , "treat_pre1", "treat_granger", "treat_post1", "treat_post2", "treat_post3plus"), 
               ordered = TRUE)
sds <- sqrt(c( vcov["treat_pre3", "treat_pre3"], vcov["treat_pre2", "treat_pre2"], vcov["treat_pre1", "treat_pre1"],
               vcov["treat_granger", "treat_granger"], vcov["treat_post1", "treat_post1"], vcov["treat_post2", "treat_post2"], 
               vcov["treat_post3plus", "treat_post3plus"]))
toplot <- data.frame(vari = vari, estimates = estimates, sds = sds)

# compute confidence intervals
toplot <- mutate(toplot,
                 ci.lo.95 = estimates + qnorm(0.025) * sds,
                 ci.hi.95 = estimates + qnorm(0.975) * sds,
                 ci.lo.90 = estimates + qnorm(0.05) * sds,
                 ci.hi.90 = estimates + qnorm(0.95) * sds)

ggplot(toplot, aes(x = vari)) +
  geom_errorbar(aes(ymin = ci.lo.95, ymax = ci.hi.95), position = position_dodge(width = 0.2),
                data = toplot,
                width = .0, size = 0.35) +
  geom_errorbar(aes(ymin = ci.lo.90, ymax = ci.hi.90), position = position_dodge(width = 0.2),
                data = toplot,
                width = .0, size = 1.5) +
  geom_point(aes(y = estimates), position = position_dodge(width = 0.2),
             data = toplot, size = 4) + 
  geom_hline(aes(yintercept = 0), lty = 2) +
  ylab("Change in logged multifamily units permitted") +
  scale_x_discrete("", labels = c( "treat_pre3"="t-3", "treat_pre2"="t-2", "treat_pre1"="t-1", 
                                   "treat_granger"="t", "treat_post1"="t+1", "treat_post2"="t+2", "treat_post3plus"=">=t+3")) +
  theme_minimal() + 
  theme(text = element_text(size = 15), legend.position = "bottom")

############################################################################
# Figure B-9: Goodman-Bacon Decomposition of the Effect of Single-Member 
# Districts on Logged Multifamily Units Permitted
############################################################################

# make balanced panel 
bdc <- na.omit(regset[,c("l.multi", "treat", "population.th", "pct_wnh", "pct_black", "pct_hisp", "location", "year", "GEO_id", "switcher2", 
                         "own_rate", "vacancy_rate", "med_value.th", "med_income.th", "prop_min_elected_past12", 
                         "max.high.third", "max.low.third", "seg.high.third", "seg.low.third", "maj_elect.high.third", 
                         "maj_elect.low.third")])
keeps <- unique(bdc$GEO_id[bdc$year==2010])
bdc <- bdc[bdc$GEO_id %in% keeps & bdc$switcher2==1,]

# for high segregation group
df_bacon_sub <- bacon(l.multi ~ treat + population.th + own_rate + vacancy_rate + med_value.th + med_income.th + 
                        prop_min_elected_past12,
                      data = bdc[bdc$seg.high.third==1,], 
                      id_var = "location",
                      time_var = "year")

bd <- df_bacon_sub$two_by_twos
bd <- bd[order(bd$weight, decreasing = TRUE),]
bd$type[bd$type=="Treated vs Untreated"] <- "Treated-Untreated"

ggplot(bd) +
  aes(x = weight, y = estimate) +
  labs(color = "", x = "Weight", y = "Estimate", shape = "Type") +
  scale_color_manual(values = c("grey65", "red")) + 
  theme_minimal() +
  theme(legend.position = "bottom") + 
  geom_hline(yintercept = 0) +
  ylim(-4, 4.3) + 
  xlim(0, .41) + 
  geom_point()

# for low majority group
df_bacon_sub <- bacon(l.multi ~ treat + population.th + own_rate + vacancy_rate + med_value.th + med_income.th + 
                        prop_min_elected_past12,
                      data = bdc[bdc$max.low.third==1,], 
                      id_var = "location",
                      time_var = "year")

bd <- df_bacon_sub$two_by_twos
bd <- bd[order(bd$weight, decreasing = TRUE),]
bd$type[bd$type=="Treated vs Untreated"] <- "Treated-Untreated"

ggplot(bd) +
  aes(x = weight, y = estimate) +
  labs(color = "", x = "Weight", y = "Estimate", shape = "Type") +
  scale_color_manual(values = c("grey65", "red")) + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  geom_hline(yintercept = 0) +
  ylim(-4, 4.3) + 
  xlim(0, .41) + 
  geom_point()

# for high majority control group
df_bacon_sub <- bacon(l.multi ~ treat + population.th + own_rate + vacancy_rate + med_value.th + med_income.th + 
                        prop_min_elected_past12,
                      data = bdc[bdc$maj_elect.high.third==1,], 
                      id_var = "location",
                      time_var = "year")

bd <- df_bacon_sub$two_by_twos
bd <- bd[order(bd$weight, decreasing = TRUE),]
bd$type[bd$type=="Treated vs Untreated"] <- "Treated-Untreated"

ggplot(bd) +
  aes(x = weight, y = estimate) +
  labs(color = "", x = "Weight", y = "Estimate", shape = "Type") +
  scale_color_manual(values=c("grey65", "red")) + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  geom_hline(yintercept = 0) +
  ylim(-4, 4.3) + 
  xlim(0, .41) + 
  geom_point()

############################################################################
# Table A-2: Characteristics of Cities in Aggregate Analysis by Type 
############################################################################

# eliminate treated observations from data
housing <- regset[regset$treat==0,]

# scale back variables in thousands
housing$population <- housing$population.th*1000
housing$med_value <- housing$med_value.th*1000
housing$med_income <- housing$med_income.th*1000

# aggregate to city-year means 
housing_agg <- housing %>% group_by(GEO_id) %>%
  summarise(population = mean(population, na.rm = TRUE),
            pct_wnh = mean(pct_wnh, na.rm = TRUE),
            pct_black = mean(pct_black, na.rm = TRUE),
            pct_asian = mean(pct_asian, na.rm = TRUE),
            pct_hisp = mean(pct_hisp, na.rm = TRUE),
            med_value = mean(med_value, na.rm = TRUE),
            vacancy_rate = mean(vacancy_rate, na.rm = TRUE),
            own_rate = mean(own_rate, na.rm = TRUE),
            density = mean(density, na.rm = TRUE),
            H_citytract_NHW = mean(H_citytract_NHW, na.rm = TRUE),
            med_income = mean(med_income, na.rm = TRUE),
            single = mean(single, na.rm = TRUE),
            multi = mean(multi, na.rm = TRUE),
            prop_latino_elected_past12 = mean(prop_latino_elected_past12, na.rm = TRUE),
            prop_black_elected_past12 = mean(prop_black_elected_past12, na.rm = TRUE),
            prop_asian_elected_past12 = mean(prop_asian_elected_past12, na.rm = TRUE),
            prop_white_elected_past12 = mean(prop_white_elected_past12, na.rm = TRUE),
            switcher = mean(switcher, na.rm = TRUE),
            subsamp = mean(switcher2, na.rm = TRUE))

# create subsets
nonswitchers <- housing_agg[housing_agg$switcher==0,] # nonswitchers, not always treated
switchers <- housing_agg[housing_agg$switcher==1,] # switchers in pre-treatment years
subsamp <- housing_agg[housing_agg$subsamp==1,] # analysis sample

fileConn <- file("pretreat.tex")
writeLines(c("{", "\n",
             "\\begin{tabular}{ l c c c c c}", "\n",
             "& Mean & Mean &  Mean & p-value of & p-value of \\\\", "\n",
             "& (Untreated) & (All  &  (Subgroup) & difference, & difference, \\\\", "\n",
             "& & switchers) & & all switchers & subgroup \\\\", "\n",
             "& & & & vs. untreated & vs. untreated \\\\", "\n",
             "\\hline", "\n",
             "\\textbf{Population} & ", "\\\\", "\n", 
             "\\hspace{2mm} Number of people & ", 
             prettyNum(round(mean(nonswitchers$population, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             prettyNum(round(mean(switchers$population, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             prettyNum(round(mean(subsamp$population, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             format(round(t.test(switchers$population, nonswitchers$population)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$population, nonswitchers$population)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\hspace{2mm} Percent non-Hispanic & ", 
             format(round(mean(nonswitchers$pct_wnh, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(mean(switchers$pct_wnh, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(mean(subsamp$pct_wnh, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(t.test(switchers$pct_wnh, nonswitchers$pct_wnh)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$pct_wnh, nonswitchers$pct_wnh)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\hspace{2mm} Percent Black & ", 
             format(round(mean(nonswitchers$pct_black, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(mean(switchers$pct_black, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(mean(subsamp$pct_black, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(t.test(switchers$pct_black, nonswitchers$pct_black)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$pct_black, nonswitchers$pct_black)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\hspace{2mm} Percent Asian & ", 
             format(round(mean(nonswitchers$pct_asian, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(mean(switchers$pct_asian, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(mean(subsamp$pct_asian, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(t.test(switchers$pct_asian, nonswitchers$pct_asian)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$pct_asian, nonswitchers$pct_asian)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\hspace{2mm} Percent Latino & ", 
             format(round(mean(nonswitchers$pct_hisp, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(mean(switchers$pct_hisp, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(mean(subsamp$pct_hisp, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(t.test(switchers$pct_hisp, nonswitchers$pct_hisp)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$pct_hisp, nonswitchers$pct_hisp)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\textbf{Past electoral success} & ", "\\\\", "\n", 
             "\\hspace{2mm} Prop. of seats w/Latino candidate elected & ", 
             format(round(mean(nonswitchers$prop_latino_elected_past12, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(switchers$prop_latino_elected_past12, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(subsamp$prop_latino_elected_past12, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(t.test(switchers$prop_latino_elected_past12, nonswitchers$prop_latino_elected_past12)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$prop_latino_elected_past12, nonswitchers$prop_latino_elected_past12)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\hspace{2mm} Prop. of seats w/Black candidate elected & ", 
             format(round(mean(nonswitchers$prop_black_elected_past12, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(switchers$prop_black_elected_past12, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(subsamp$prop_black_elected_past12, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(t.test(switchers$prop_black_elected_past12, nonswitchers$prop_black_elected_past12)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$prop_black_elected_past12, nonswitchers$prop_black_elected_past12)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\hspace{2mm} Prop. of seats w/Asian candidate elected & ", 
             format(round(mean(nonswitchers$prop_asian_elected_past12, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(switchers$prop_asian_elected_past12, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(subsamp$prop_asian_elected_past12, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(t.test(switchers$prop_asian_elected_past12, nonswitchers$prop_asian_elected_past12)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$prop_asian_elected_past12, nonswitchers$prop_asian_elected_past12)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\hspace{2mm} Prop. of seats w/white candidate elected & ", 
             format(round(mean(nonswitchers$prop_white_elected_past12, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(switchers$prop_white_elected_past12, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(subsamp$prop_white_elected_past12, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(t.test(switchers$prop_white_elected_past12, nonswitchers$prop_white_elected_past12)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$prop_white_elected_past12, nonswitchers$prop_white_elected_past12)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\textbf{Income and land use} & ", "\\\\", "\n", 
             "\\hspace{2mm} Median household income (\\$) & ", 
             prettyNum(round(mean(nonswitchers$med_income, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             prettyNum(round(mean(switchers$med_income, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             prettyNum(round(mean(subsamp$med_income, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             format(round(t.test(switchers$med_income, nonswitchers$med_income)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$med_income, nonswitchers$med_income)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\hspace{2mm} Median home value (\\$) & ", 
             prettyNum(round(mean(nonswitchers$med_value, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             prettyNum(round(mean(switchers$med_value, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             prettyNum(round(mean(subsamp$med_value, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             format(round(t.test(switchers$med_value, nonswitchers$med_value)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$med_value, nonswitchers$med_value)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\hspace{2mm} Home vacancy rate & ", 
             format(round(mean(nonswitchers$vacancy_rate, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(switchers$vacancy_rate, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(subsamp$vacancy_rate, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(t.test(switchers$vacancy_rate, nonswitchers$vacancy_rate)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$vacancy_rate, nonswitchers$vacancy_rate)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\hspace{2mm} Home ownership rate & ", 
             format(round(mean(nonswitchers$own_rate, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(switchers$own_rate, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(subsamp$own_rate, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(t.test(switchers$own_rate, nonswitchers$own_rate)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$own_rate, nonswitchers$own_rate)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\hspace{2mm} Density (population per sq. mile)& ", 
             prettyNum(round(mean(nonswitchers$density, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             prettyNum(round(mean(switchers$density, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             prettyNum(round(mean(subsamp$density, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             format(round(t.test(switchers$density, nonswitchers$density)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$density, nonswitchers$density)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\hspace{2mm} Residential segregation (Theil index) & ", 
             format(round(mean(nonswitchers$H_citytract_NHW, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(switchers$H_citytract_NHW, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(subsamp$H_citytract_NHW, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(t.test(switchers$H_citytract_NHW, nonswitchers$H_citytract_NHW)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$H_citytract_NHW, nonswitchers$H_citytract_NHW)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\textbf{Housing outcomes} & ", "\\\\", "\n", 
             "\\hspace{2mm} Units permitted annually, single-family & ", 
             prettyNum(round(mean(nonswitchers$single, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             prettyNum(round(mean(switchers$single, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             prettyNum(round(mean(subsamp$single, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             format(round(t.test(switchers$single, nonswitchers$single)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$single, nonswitchers$single)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\hspace{2mm} Units permitted annually, multifamily & ", 
             prettyNum(round(mean(nonswitchers$multi, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             prettyNum(round(mean(switchers$multi, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             prettyNum(round(mean(subsamp$multi, na.rm = TRUE), digits = 0), big.mark = ","), " & ", 
             format(round(t.test(switchers$multi, nonswitchers$multi)$p.value, digits = 2), nsmall = 2), " & ", 
             format(round(t.test(subsamp$multi, nonswitchers$multi)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\hline", "\n",
             "\\textbf{N} & ", length(unique(nonswitchers$GEO_id)), " & ", length(unique(switchers$GEO_id)),
             " & ", length(unique(subsamp$GEO_id)), "\\\\",  "\n",
             "\\end{tabular}", "\n",
             "}"), 
           sep = "", fileConn)
close(fileConn)


