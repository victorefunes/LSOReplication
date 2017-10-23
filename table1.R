library(tidyverse)
library(broom)
library(Hmisc)
library(foreign)
library(MASS)
library(rdrobust)
library(statar)

options(prompt = "R> ", np.messages = FALSE, digits = 3, scipen = 3)

# table1.R

# open data
lso <- read.dta("Lindo-et-al.dta")

# choose observations with diff between GPA and Threshold < abs(6)
# Rectangular kernel (same assumption in alll estimations)

lso <- lso %>% filter(dist_from_cut < 0.6 & dist_from_cut > -0.6)

# choose variables
lso <- lso %>% dplyr::select(c(hsgrade_pct, totcredits_year1, age_at_entry, 
                        male, english, bpl_north_america,
                        loc_campus1, loc_campus2, loc_campus3, 
                        dist_from_cut, probation_year1, probation_ever, 
                        left_school, year2_dist_from_cut, suspended_ever,
                        gradin4, gradin5, gradin6))

tab1 <- lso %>% summarise_all(funs(mean(., na.rm=TRUE), sd(., na.rm=TRUE)))

table1 <- data.frame(matrix(NA, ncol=2, nrow=18))
colnames(table1) <- c("mean", "sd")
rownames(table1) <- c("hsgrade_pct", "totcredits_year1", "age_at_entry", 
                      "male", "english", "bpl_north_america",
                      "loc_campus1", "loc_campus2", "loc_campus3", 
                      "dist_from_cut", "probation_year1", "probation_ever", 
                      "left_school", "year2_dist_from_cut", "suspended_ever",
                      "gradin4", "gradin5", "gradin6")
for(i in 1:18){
  table1[i,1] <- tab1[i]
  table1[i,2] <- tab1[i+18]
}
table1