####################################################################################
# This script generates the following tables and figures in Hankinson and Magazinnik (2022)

### Main Text
# Table 3: Effect of Conversion to Single-Member Districts on Logged Total Units Approved 
# Figure 3: Difference in Logged Total Units Approved between Minority Block Groups and White Block Groups, At-Large vs. District

### Appendix
# Figure C-11: Logged Total Units Approved, by Block Group Composition (Minority or White) and Year Relative to First District Election
# Table C-10: Effect of Conversion to Single-Member Districts on Logged Total Units Approved (Decomposition)
# Table C-11: Effect of Conversion to Single-Member Districts on Logged Total Units Approved, Robustness to Exclusion of One City
# Figure C-12: Event Study Plot of Spatial Diff-in-Diff Interaction
# Table C-12: Effect of Conversion to Single-Member Districts on Logged Units Approved, Terciles Defined Over All Treated Cities (Minority block groups: less than 38 percent white, white block groups: more than 67 percent white)
# Table C-13: Effect of Conversion to Single-Member Districts on Logged Units Approved, Alternative Clustering Approaches
# Table A-3: Characteristics of Cities in Distributive Analysis by Type
####################################################################################

# clear workspace 
rm(list = ls())

# install libraries
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("HMisc")
#install.packages("clusterSEs")
#install.packages("stargazer")
#install.packages("multiwayvcov")
#install.packages("lmtest")

# load libraries
library("ggplot2")
library("tidyverse")
library("Hmisc")
library("clusterSEs")
library("stargazer")
library("multiwayvcov")
library("lmtest")

# set working directory 
setwd("") # your working directory here 

# load data 
df <- read.csv("housing_spatial.csv")

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

# change city from factor to character
df$city <- as.character(df$city)

# assign high and low race BG. by each city's relative standards
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

# create log permits including zoning outcome
df$lnunits_multi_z <- log(df$nunits_multi_z+1)
df$lnunits_total_z <- log(df$nunits_total_z+1)
df$lnunits_single_z <- log(df$nunits_single_z+1)

# generate leading and lagging indicators for Granger Test (Autor 2003)
treats <- df %>% arrange(city, year) %>% filter(treat==1) # get all treated observations
nonswitcher <- unique(df$city[df$switcher==0])
treats <- treats[!treats$city %in% nonswitcher,]
years <- treats %>% group_by(city) %>% summarise(treat_year = min(year)) # compute min by group -- should give us first treatment year
years$treat_pre3 <- years$treat_year - 3 
years$treat_pre2 <- years$treat_year - 2 
years$treat_pre1 <- years$treat_year - 1
years$treat_post1 <- years$treat_year + 1
years$treat_post2 <- years$treat_year + 2
years$treat_post3 <- years$treat_year + 3

# loop through dataset, filling in relevant years
df$treat_pre3 <- df$treat_pre2 <- df$treat_pre1 <- df$treat_granger <- df$treat_post1 <-
  df$treat_post2 <- df$treat_post3plus <- 0
for (i in c(1:nrow(years))) {
  df$treat_pre3[df$city==years$city[i] & df$year==years$treat_pre3[i]] <- 1
  df$treat_pre2[df$city==years$city[i] & df$year==years$treat_pre2[i]] <- 1
  df$treat_pre1[df$city==years$city[i] & df$year==years$treat_pre1[i]] <- 1
  df$treat_granger[df$city==years$city[i] & df$year==years$treat_year[i]] <- 1
  df$treat_post1[df$city==years$city[i] & df$year==years$treat_post1[i]] <- 1
  df$treat_post2[df$city==years$city[i] & df$year==years$treat_post2[i]] <- 1
  df$treat_post3plus[df$city==years$city[i] & df$year>=years$treat_post3[i]] <- 1
}

# subset to top and bottom terciles 
df2 <- subset(df, wnh.low.third==1 | wnh.high.third==1)

####################################################################################
# Figure C-11: Logged Total Units Approved, by Block Group Composition 
# (Minority or White) and Year Relative to First District Election
####################################################################################

df2$wnh.cat <- as.factor(ifelse(df2$wnh.low.third==1, "Minority", "White"))

vline.data <- data.frame(city = unique(df2$city), year_treat = NA)
vline.data$year_treat[vline.data$city=="Anaheim"] <- 2016
vline.data$year_treat[vline.data$city=="Escondido"] <- 2014
vline.data$year_treat[vline.data$city=="Ventura"] <- 2018
vline.data$year_treat[vline.data$city=="Santa Barbara"] <- 2015

# create figure
ggplot(df2, aes(x = year, y = lnunits_total_z, group = wnh.cat, colour = wnh.cat)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + 
  stat_summary(fun.data = mean_cl_boot, geom = "line") + 
  facet_wrap(. ~ city, ncol = 2) + 
  geom_vline(aes(xintercept = year_treat), vline.data, colour = "black", linetype = "dashed") + 
  ylab("Logged total units approved") +
  xlab("Year") + 
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom") +
  scale_color_manual(values = c("Minority" = "deepskyblue2", "White" = "gray30"))

####################################################################################
# Table 3: Effect of Conversion to Single-Member Districts on Logged Total Units Approved 
####################################################################################

total_int <- glm(formula = lnunits_total_z ~ treat*wnh.low.third + n_households + med_income_imp + med_value_imp + vacancy_rate +
                   own_rate + pct_black + pct_hisp + as.factor(year) + year*as.factor(city), data = df2)

multi_int <- glm(formula = lnunits_multi_z ~ treat*wnh.low.third + n_households + med_income_imp + med_value_imp + vacancy_rate +
                   own_rate + pct_black + pct_hisp + as.factor(year) + year*as.factor(city), data = df2)

single_int <- glm(formula = lnunits_single_z ~ treat*wnh.low.third + n_households + med_income_imp + med_value_imp + vacancy_rate +
                    own_rate + pct_black + pct_hisp + as.factor(year) + year*as.factor(city), data = df2)

# wild bootstrap, city
cluster.total <- cluster.wild.glm(total_int, 
                                  dat = df2,
                                  cluster = ~city,
                                  boot.reps = 1000,
                                  output.replicates = TRUE,
                                  impose.null = FALSE,
                                  seed = 123)

cluster.multi <- cluster.wild.glm(multi_int,
                                  dat = df2,
                                  cluster = ~city,
                                  boot.reps = 1000,
                                  output.replicates = TRUE,
                                  impose.null = FALSE,
                                  seed = 123)

cluster.single <- cluster.wild.glm(single_int,
                                   dat = df2,
                                   cluster = ~city,
                                   boot.reps = 1000,
                                   output.replicates = TRUE,
                                   impose.null = FALSE,
                                   seed = 123)

# create table 
stargazer(total_int, multi_int, single_int,
          report = "vcp*",
          p = list(cluster.total$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),],
                   cluster.multi$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),],
                   cluster.single$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),]),
          keep.stat = c("n", "rsq"),
          keep = c("treat", "wnh.low.third", "treat:wnh.low.third"),
          intercept.top = FALSE, intercept.bottom=TRUE,
          title = "Effect of Conversion to Single-Member Districts on Logged Total Units Approved",
          covariate.labels = c("Single-member districts", "Minority block groups", "SMD$*$Minority block groups"),
          dep.var.labels.include = TRUE,
          dep.var.caption = "",
          dep.var.labels = c("Total Units", "Multifamily Units", "Single-family units"),
          star.cutoffs = c(.05, .01, .001),
          no.space = TRUE, multicolumn = T, type = "text",
          align = TRUE,
          notes.append = TRUE,
          add.lines = list(c("Controls", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("City FE", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("Year FE", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}" ),
                           c("City Trends", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}")))

####################################################################################
# Figure 3: Difference in Logged Total Units Approved between Minority Block Groups 
# and White Block Groups, At-Large vs. District
####################################################################################

prepare <- function(results) { 
  # institutional form 
  period <- c("At-Large", "District") 
  period <- factor(period, levels = c("At-Large", "District"), ordered = TRUE)
  
  # estimates
  estimates <- cbind(results$replicates[,"wnh.low.third"], 
                     results$replicates[,"wnh.low.third"] + results$replicates[,"treat:wnh.low.third"])
  
  # create output dataset
  toplot <- data.frame(period = period, estimates = colMeans(estimates))
  
  # confidence intervals (normal)
  sds <- apply(estimates, 2, function(x) sqrt(var(x)))
  toplot$ci.norm.lo.95 <- toplot$estimates + qnorm(0.025)*sds
  toplot$ci.norm.hi.95 <- toplot$estimates + qnorm(0.975)*sds
  toplot$ci.norm.lo.90 <- toplot$estimates + qnorm(0.05)*sds
  toplot$ci.norm.hi.90 <- toplot$estimates + qnorm(0.95)*sds  
  return(toplot)
}

toplot_total <- prepare(results = cluster.total)

# create figure
ggplot(toplot_total, aes(x = period)) +
  geom_errorbar(aes(ymin = ci.norm.lo.95, ymax = ci.norm.hi.95), position = position_dodge(width = 0.2),
                data = toplot_total,
                width = .0, size = 0.35) +
  geom_errorbar(aes(ymin = ci.norm.lo.90, ymax = ci.norm.hi.90), position = position_dodge(width = 0.2),
                data = toplot_total,
                width = .0, size = 1.5) +
  geom_point(aes(y = estimates), position = position_dodge(width = 0.2),
             data = toplot_total, size = 4) + 
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_vline(xintercept = 1.5 ) + 
  ylab("Difference in logged total units approved") +
  scale_x_discrete("") +
  coord_cartesian(xlim = NULL, ylim = c(-.5, .5), expand = TRUE,
                  default = FALSE, clip = "on") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 16))

####################################################################################
# Table C-10: Effect of Conversion to Single-Member Districts on Logged Total Units 
# Approved (Decomposition)
####################################################################################

total_int_c <- glm(formula = lnunits_total_z ~ treat*wnh.low.third + n_households + med_income_imp + med_value_imp + vacancy_rate +
                     own_rate + pct_black + pct_hisp  + as.factor(year) + as.factor(city), data = df2)
total_int_fe <- glm(formula = lnunits_total_z ~ treat*wnh.low.third + as.factor(year) + as.factor(city), data = df2)
total_int_tt <- glm(formula = lnunits_total_z ~ treat*wnh.low.third + year*as.factor(city), data = df2)
total_int_bi <- glm(formula = lnunits_total_z ~ treat*wnh.low.third, data = df2)

# wild bootstrap, city
cluster.total_c <- cluster.wild.glm(total_int_c,
                                    dat = df2,
                                    cluster = ~city,
                                    boot.reps = 1000,
                                    impose.null = FALSE,
                                    output.replicates = TRUE,
                                    seed = 123)

cluster.total_fe <- cluster.wild.glm(total_int_fe,
                                     dat = df2,
                                     cluster = ~city,
                                     boot.reps = 1000,
                                     impose.null = FALSE,
                                     output.replicates = TRUE,
                                     seed = 123)

cluster.total_tt <- cluster.wild.glm(total_int_tt,
                                     dat = df2,
                                     cluster = ~city,
                                     boot.reps = 1000,
                                     impose.null = FALSE,
                                     output.replicates = TRUE,
                                     seed = 123)

cluster.total_bi <- cluster.wild.glm(total_int_bi,
                                     dat = df2,
                                     cluster = ~city,
                                     boot.reps = 1000,
                                     impose.null = FALSE,
                                     output.replicates = TRUE,
                                     seed = 123)

# create table 
stargazer(total_int_bi, total_int_fe, total_int_tt, total_int_c, total_int,
          report = "vcp*",
          p = list(cluster.total_bi$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),],
                   cluster.total_fe$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),],
                   cluster.total_tt$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),],
                   cluster.total_c$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),],
                   cluster.total$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),]),
          keep.stat = c("n", "rsq"),
          keep = c("treat", "wnh.low.third", "treat:wnh.low.third"),
          intercept.top = FALSE, intercept.bottom=TRUE,
          title = "Effect of Conversion to Single-Member Districts on Logged Total Units Approved",
          covariate.labels = c("Single-member districts", "Minority block groups", "SMD$*$Minority block groups"),
          dep.var.labels.include = F,
          dep.var.caption = "",
          star.cutoffs = c(.05, .01, .001),
          no.space = TRUE, multicolumn = T, type = "text",           
          align = TRUE,
          notes.append = TRUE,
          add.lines = list(c("Controls", "\\mc{No}","\\mc{No}", "\\mc{No}","\\mc{Yes}", "\\mc{Yes}"),
                           c("City FE", "\\mc{No}","\\mc{Yes}", "\\mc{Yes}","\\mc{Yes}", "\\mc{Yes}"),
                           c("Year FE", "\\mc{No}","\\mc{Yes}", "\\mc{Yes}","\\mc{Yes}", "\\mc{Yes}"),
                           c("City Trends", "\\mc{No}","\\mc{No}", "\\mc{Yes}","\\mc{No}", "\\mc{Yes}")))

####################################################################################
# Table C-11: Effect of Conversion to Single-Member Districts on Logged Total 
# Units Approved, Robustness to Exclusion of One City
####################################################################################

# headline model dropping each city and recovering the resulting treatment effects
total_int_ana <- glm(formula = lnunits_total_z ~ treat*wnh.low.third + n_households + med_income_imp + med_value_imp + vacancy_rate +
                       own_rate + pct_black + pct_hisp  + as.factor(year) + year*as.factor(city), data = subset(df2, city!="Anaheim"))
total_int_esc <- glm(formula = lnunits_total_z ~ treat*wnh.low.third + n_households + med_income_imp + med_value_imp + vacancy_rate +
                       own_rate + pct_black + pct_hisp  + as.factor(year) + year*as.factor(city), data = subset(df2, city!="Escondido"))
total_int_gle <- glm(formula = lnunits_total_z ~ treat*wnh.low.third + n_households + med_income_imp + med_value_imp + vacancy_rate +
                       own_rate + pct_black + pct_hisp  + as.factor(year) + year*as.factor(city), data = subset(df2, city!="Glendale"))
total_int_sba <- glm(formula = lnunits_total_z ~ treat*wnh.low.third + n_households + med_income_imp + med_value_imp + vacancy_rate +
                       own_rate + pct_black + pct_hisp  + as.factor(year) + year*as.factor(city), data = subset(df2, city!="Santa Barbara"))
total_int_scr <- glm(formula = lnunits_total_z ~ treat*wnh.low.third + n_households + med_income_imp + med_value_imp + vacancy_rate +
                       own_rate + pct_black + pct_hisp  + as.factor(year) + year*as.factor(city), data = subset(df2, city!="Santa Cruz"))
total_int_ven <- glm(formula = lnunits_total_z ~ treat*wnh.low.third + n_households + med_income_imp + med_value_imp + vacancy_rate +
                       own_rate + pct_black + pct_hisp  + as.factor(year) + year*as.factor(city), data = subset(df2, city!="Ventura"))

#wild bootstrap
cluster.total_ana <- cluster.wild.glm(total_int_ana,
                                      dat = subset(df2, city!="Anaheim"),
                                      cluster = ~city,
                                      boot.reps = 1000,
                                      impose.null = FALSE,
                                      output.replicates = TRUE,
                                      seed = 123)

cluster.total_esc <- cluster.wild.glm(total_int_esc,
                                      dat = subset(df2, city!="Escondido"),
                                      cluster = ~city,
                                      boot.reps = 1000,
                                      impose.null = FALSE,
                                      output.replicates = TRUE,
                                      seed = 123)

cluster.total_gle <- cluster.wild.glm(total_int_gle,
                                      dat = subset(df2, city!="Glendale"),
                                      cluster = ~city,
                                      boot.reps = 1000,
                                      impose.null = FALSE,
                                      output.replicates = TRUE,
                                      seed = 123)

cluster.total_sba <- cluster.wild.glm(total_int_sba,
                                      dat = subset(df2, city!="Santa Barbara"),
                                      cluster = ~city,
                                      boot.reps = 1000,
                                      impose.null = FALSE,
                                      output.replicates = TRUE,
                                      seed = 123)

cluster.total_scr <- cluster.wild.glm(total_int_scr,
                                      dat = subset(df2, city!="Santa Cruz"),
                                      cluster = ~city,
                                      boot.reps = 1000,
                                      impose.null = FALSE,
                                      output.replicates = TRUE,
                                      seed = 123)

cluster.total_ven <- cluster.wild.glm(total_int_ven,
                                      dat = subset(df2, city!="Ventura"),
                                      cluster = ~city,
                                      boot.reps = 1000,
                                      impose.null = FALSE,
                                      output.replicates = TRUE,
                                      seed = 123)

# table 1/2
stargazer(total_int, total_int_ana, total_int_esc, total_int_gle,
          report = "vcp*",
          p = list(cluster.total$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),],
                   cluster.total_ana$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),],
                   cluster.total_esc$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),],
                   cluster.total_gle$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),]),
          keep.stat = c("n", "rsq"),
          keep = c("treat", "wnh.low.third", "treat:wnh.low.third"),
          intercept.top = FALSE, intercept.bottom=TRUE,
          title = "Effect of Conversion to Single-Member Districts on Logged Total Units Approved",
          covariate.labels = c("Single-member districts", "Minority block groups", "SMD$*$Minority block groups"),
          dep.var.labels.include = F,
          dep.var.caption = "",
          column.labels = c("Full", "No Anaheim", "No Escondido", "No Glendale"),
          star.cutoffs = c(.05, .01, .001),
          no.space = TRUE, multicolumn = T, type = "text",
          align = TRUE,
          notes.append = TRUE,
          add.lines = list(c("Controls", "\\mc{Yes}","\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("City FE", "\\mc{Yes}","\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("Year FE", "\\mc{Yes}","\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("City Trends", "\\mc{Yes}","\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}")))

# table 2/2
stargazer(total_int, total_int_sba, total_int_scr, total_int_ven,
          report = "vcp*",
          p = list(cluster.total$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),],
                   cluster.total_sba$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),],
                   cluster.total_scr$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),],
                   cluster.total_ven$p.values[c("treat", "wnh.low.third", "treat:wnh.low.third"),]),
          keep.stat = c("n", "rsq"),
          keep = c("treat", "wnh.low.third", "treat:wnh.low.third"),
          intercept.top = FALSE, intercept.bottom=TRUE,
          title = "Effect of Conversion to Single-Member Districts on Logged Total Units Approved",
          covariate.labels = c("Single-member districts", "Minority block groups", "SMD$*$Minority block groups"),
          dep.var.labels.include = F,
          dep.var.caption = "",
          column.labels = c("Full", "No Santa Barbara", "No Santa Cruz", "No Ventura"),
          star.cutoffs = c(.05, .01, .001),
          no.space = TRUE, multicolumn = T, type = "text",
          align = TRUE,
          notes.append = TRUE,
          add.lines = list(c("Controls", "\\mc{Yes}","\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("City FE", "\\mc{Yes}","\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("Year FE", "\\mc{Yes}","\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("City Trends", "\\mc{Yes}","\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}")))

####################################################################################
# Figure C-12: Event Study Plot of Spatial Diff-in-Diff Interaction
####################################################################################

# top v. bottom third interaction
total_int_g <- glm(formula = lnunits_total_z ~ treat_pre2*wnh.low.third + treat_pre1*wnh.low.third +
                     treat_granger*wnh.low.third + treat_post1*wnh.low.third + 
                     treat_post2*wnh.low.third + treat_post3plus*wnh.low.third + 
                     n_households + med_income_imp + med_value_imp + vacancy_rate + 
                     own_rate + pct_black + pct_hisp + as.factor(year) + year*as.factor(city), data = df2)

# standard errors 
se.total_int_g <- coeftest(total_int_g, vcov = function(x) cluster.vcov(total_int_g, df2$city))

vcov <- cluster.vcov(total_int_g, df2$city)
vari <- c( "treat_pre2:wnh.low.third", "wnh.low.third:treat_pre1", "wnh.low.third:treat_granger", 
            "wnh.low.third:treat_post1", "wnh.low.third:treat_post2", "wnh.low.third:treat_post3plus")
estimates <- coef(total_int_g)[vari]
vari <- factor(vari, levels = c("treat_pre2:wnh.low.third", "wnh.low.third:treat_pre1", 
                                "wnh.low.third:treat_granger", "wnh.low.third:treat_post1",
                                "wnh.low.third:treat_post2", "wnh.low.third:treat_post3plus"), 
               ordered = TRUE)
sds <- sqrt(c( vcov["treat_pre2:wnh.low.third", "treat_pre2:wnh.low.third"],  
               vcov["wnh.low.third:treat_pre1", "wnh.low.third:treat_pre1"],
               vcov["wnh.low.third:treat_granger", "wnh.low.third:treat_granger"], 
               vcov["wnh.low.third:treat_post1", "wnh.low.third:treat_post1"],
               vcov["wnh.low.third:treat_post2", "wnh.low.third:treat_post2"], 
               vcov["wnh.low.third:treat_post3plus", "wnh.low.third:treat_post3plus"]))
toplot <- data.frame(vari = vari, estimates = estimates, sds = sds)

# compute confidence intervals
toplot <- mutate(toplot,
                 ci.lo.95 = estimates + qnorm(0.025) * sds,
                 ci.hi.95 = estimates + qnorm(0.975) * sds,
                 ci.lo.90 = estimates + qnorm(0.05) * sds,
                 ci.hi.90 = estimates + qnorm(0.95) * sds)

# create figure
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
  ylab("Change in logged total units approved") +
  scale_x_discrete("", labels = c( "treat_pre2:wnh.low.third"="t-2", "wnh.low.third:treat_pre1"="t-1", 
                                   "wnh.low.third:treat_granger"="t", "wnh.low.third:treat_post1"="t+1",
                                   "wnh.low.third:treat_post2"="t+2","wnh.low.third:treat_post3plus"="t>=3")) +
  theme_minimal() + 
  theme(text = element_text(size = 15), legend.position = "bottom")

####################################################################################
# Table C-12: Effect of Conversion to Single-Member Districts on Logged Units 
# Approved, Terciles Defined Over All Treated Cities (Minority block groups: 
# less than 38 percent white, white block groups: more than 67 percent white)
####################################################################################

# assign high and low race BG. low == .38, high  = .67
wnh_ave_switcher <- tapply(df$pct_nlw[df$switcher==1 & df$treat==0], as.character(df$geoid[df$switcher==1 & df$treat==0]), mean, na.rm = TRUE)
wnh_ave <- tapply(df$pct_nlw, as.character(df$geoid), mean, na.rm = TRUE)
high_med <- names(wnh_ave)[wnh_ave > median(wnh_ave_switcher, na.rm = TRUE)]
high_third <- names(wnh_ave)[wnh_ave > quantile(wnh_ave_switcher, probs = .67, na.rm = TRUE)]
low_third <- names(wnh_ave)[wnh_ave < quantile(wnh_ave_switcher, probs = .33, na.rm = TRUE)]
df$wnh.high.third2 <- ifelse(df$geoid %in% high_third, 1, 0)
df$wnh.low.third2 <- ifelse(df$geoid %in% low_third, 1, 0)

df3 <- df[df$wnh.high.third2==1 | df$wnh.low.third2==1,]

# top v. bottom third interaction
total_int2 <- glm(formula = lnunits_total_z ~ treat*wnh.low.third2 + n_households + med_income_imp + med_value_imp + vacancy_rate +
                   own_rate + pct_black + pct_hisp + as.factor(year) + year*as.factor(city), data = df3)

multi_int2 <- glm(formula = lnunits_multi_z ~ treat*wnh.low.third2 + n_households + med_income_imp + med_value_imp + vacancy_rate +
                   own_rate + pct_black + pct_hisp + as.factor(year) + year*as.factor(city), data = df3)

single_int2 <- glm(formula = lnunits_single_z ~ treat*wnh.low.third2 + n_households + med_income_imp + med_value_imp + vacancy_rate +
                    own_rate + pct_black + pct_hisp + as.factor(year) + year*as.factor(city), data = df3)

# wild bootstrap, city
cluster.total2 <- cluster.wild.glm(total_int2,
                                  dat = df3,
                                  cluster = ~city,
                                  boot.reps = 1000,
                                  impose.null = FALSE,
                                  output.replicates = TRUE,
                                  seed = 123)

cluster.multi2 <- cluster.wild.glm(multi_int2,
                                  dat = df3,
                                  cluster = ~city,
                                  boot.reps = 1000,
                                  impose.null = FALSE,
                                  output.replicates = TRUE,
                                  seed = 123)

cluster.single2 <- cluster.wild.glm(single_int2,
                                   dat = df3,
                                   cluster = ~city,
                                   boot.reps = 1000,
                                   impose.null = FALSE,
                                   output.replicates = TRUE,
                                   seed = 123)

# create table 
stargazer(total_int2, multi_int2, single_int2,
          report = "vcp*",
          p = list(cluster.total2$p.values[c("treat", "wnh.low.third2", "treat:wnh.low.third2"),],
                   cluster.multi2$p.values[c("treat", "wnh.low.third2", "treat:wnh.low.third2"),],
                   cluster.single2$p.values[c("treat", "wnh.low.third2", "treat:wnh.low.third2"),]),
          keep.stat = c("n", "rsq"),
          keep = c("treat", "wnh.low.third2", "treat:wnh.low.third2"),
          intercept.top = FALSE, intercept.bottom=TRUE,
          title = "Effect of Conversion to Single-Member Districts on Logged Units Approved \\\\ Terciles Defined Over All Treated Cities (Minority block groups: less than 38 percent white, white block groups: more than 67 percent white)",
          covariate.labels = c("Single-member districts", "Minority block groups", "SMD$*$Minority block groups"),
          dep.var.labels.include = TRUE,
          dep.var.caption = "",
          dep.var.labels = c("Total Units", "Multifamily Units", "Single-family units"),
          star.cutoffs = c(.05, .01, .001),
          no.space = TRUE, multicolumn = T, type = "text",
          align = TRUE,
          notes.append = TRUE,
          add.lines = list(c("Controls", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("City FE", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}"),
                           c("Year FE", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}" ),
                           c("City Trends", "\\mc{Yes}", "\\mc{Yes}", "\\mc{Yes}")))

####################################################################################
# Table C-13: Effect of Conversion to Single-Member Districts on Logged Units 
# Approved, Alternative Clustering Approaches
####################################################################################

# block bootstrap
cluster.total.bs <- cluster.bs.glm(total_int,
                                   dat = df2,
                                   boot.reps = 10000,
                                   cluster = ~city,
                                   stratify = TRUE,
                                   seed = 123)

cluster.multi.bs <- cluster.bs.glm(multi_int,
                                   dat = df2,
                                   boot.reps = 10000,
                                   cluster = ~city,
                                   stratify = TRUE,
                                   seed = 123)

cluster.single.bs <- cluster.bs.glm(single_int,
                                    dat = df2,
                                    boot.reps = 10000,
                                    cluster = ~city,
                                    stratify = TRUE,
                                    seed = 123)

# cluster robust SEs, clustered on city
se.total <- coeftest(total_int, vcov = cluster.vcov(total_int, df2$city))
se.multi <- coeftest(multi_int, vcov = cluster.vcov(multi_int, df2$city))
se.single <- coeftest(single_int, vcov = cluster.vcov(single_int, df2$city))

# print p-values  
print(list("single-member districts: wild" = c(cluster.total$p.values["treat",], cluster.multi$p.values["treat",], cluster.single$p.values["treat",]),
           "single-member districts: block" = c(cluster.total.bs$p.values["treat",], cluster.multi.bs$p.values["treat",], cluster.single.bs$p.values["treat",]),
           "single-member districts: cluster" = c(se.total["treat", "Pr(>|z|)"], se.multi["treat", "Pr(>|z|)"], se.single["treat", "Pr(>|z|)"]),
           "minority block groups: wild" = c(cluster.total$p.values["wnh.low.third",], cluster.multi$p.values["wnh.low.third",], cluster.single$p.values["wnh.low.third",]),
           "minority block groups: block" = c(cluster.total.bs$p.values["wnh.low.third",], cluster.multi.bs$p.values["wnh.low.third",], cluster.single.bs$p.values["wnh.low.third",]),
           "minority block groups: cluster" = c(se.total["wnh.low.third", "Pr(>|z|)"], se.multi["wnh.low.third", "Pr(>|z|)"], se.single["wnh.low.third", "Pr(>|z|)"]),
           "interaction: wild" = c(cluster.total$p.values["treat:wnh.low.third",], cluster.multi$p.values["treat:wnh.low.third",], cluster.single$p.values["treat:wnh.low.third",]),
           "interaction: block" = c(cluster.total.bs$p.values["treat:wnh.low.third",], cluster.multi.bs$p.values["treat:wnh.low.third",], cluster.single.bs$p.values["treat:wnh.low.third",]),
           "interaction: cluster" = c(se.total["treat:wnh.low.third", "Pr(>|z|)"], se.multi["treat:wnh.low.third", "Pr(>|z|)"], se.single["treat:wnh.low.third", "Pr(>|z|)"])))

####################################################################################
# Table A-3: Characteristics of Cities in Distributive Analysis by Type
####################################################################################

sub <- df[(df$city %in% c("Santa Barbara", "Santa Cruz") & df$year %in% c(2011:2014)) |
            (df$city %in% c("Escondido", "Ventura") & df$year %in% c(2011:2013)) |
            (df$city %in% c("Anaheim", "Glendale") & df$year %in% c(2011:2015)),]
t <- sub[sub$switcher==1,]
c <- sub[sub$switcher==0,]

fileConn <- file("balance.tex")
writeLines(c("{", "\n",
             "\\begin{tabular}{ l c c c }", "\n",
             "& Mean & Mean & p-value of \\\\", "\n",
             "& (Treatment) & (Control) & difference \\\\", "\n",
             "\\hline", "\n",
             "Median income & ", 
             format(round(mean(t$med_income, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(mean(c$med_income, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(t.test(t$med_income, c$med_income)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "Median home value & ", 
             format(round(mean(t$med_value, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(mean(c$med_value, na.rm = TRUE), digits = 0), nsmall = 0), " & ", 
             format(round(t.test(t$med_value, c$med_value)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "Home ownership rate & ", 
             format(round(mean(t$own_rate, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(c$own_rate, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(t.test(t$own_rate, c$own_rate)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "Home vacancy rate & ", 
             format(round(mean(t$vacancy_rate, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(c$vacancy_rate, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(t.test(t$vacancy_rate, c$vacancy_rate)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "Proportion Black & ", 
             format(round(mean(t$pct_black, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(c$pct_black, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(t.test(t$pct_black, c$pct_black)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "Proportion non-Hispanic white & ", 
             format(round(mean(t$pct_nlw, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(c$pct_nlw, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(t.test(t$pct_nlw, c$pct_nlw)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "Proportion Hispanic & ", 
             format(round(mean(t$pct_hisp, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(mean(c$pct_hisp, na.rm = TRUE), digits = 2), nsmall = 2), " & ", 
             format(round(t.test(t$pct_hisp, c$pct_hisp)$p.value, digits = 2), nsmall = 2), "\\\\",  "\n",
             "\\end{tabular}", "\n",
             "}"), 
           sep = "", fileConn)
close(fileConn)