library(tidyverse)
library(broom)
library(statar)
library(Hmisc)
library(foreign)
library(MASS)
library(rdrobust)
library(lfe)

options(prompt = "R> ", np.messages = FALSE, digits = 3, scipen = 3)

# table2.R

# open data
lso <- read.dta("Lindo-et-al.dta")

# check discontinuity in observable characteristics 
# e. g. : hsgrade_pct = percentile of HS grade ranking 
reg1 <- felm(hsgrade_pct~gpalscutoff+gpaXgpalscutoff+gpaXgpagrcutoff|0|0|clustervar,
             data = subset(lso, dist_from_cut >= -0.6 & dist_from_cut <= 0.6))
summary(reg1)  

# table 2
table2 <- data.frame(matrix(NA, nrow=5, ncol=9))
colnames(table2) <- c("hsgrade_pct", "totcredits_year1", "age_at_entry", 
                      "male", "bpl_north_america", "english", "loc_campus1",
                      "loc_campus2", "loc_campus3")
rownames(table2) <- c("b(gpalscutoff)", "se(gpalscutoff)",
                      "b(const)", "se(const)", "N")

lso1 <- lso %>% 
  filter(dist_from_cut >= -0.6 & dist_from_cut <= 0.6)

for(i in 1:9){
  reg <- felm(lso1[, colnames(table2)[i]]~gpalscutoff+gpaXgpalscutoff+
                gpaXgpagrcutoff|0|0|clustervar, data = lso1)
  table2[1,i] <- tidy(reg)[2,2]
  table2[2,i] <- tidy(reg)[2,3] 
  table2[3,i] <- tidy(reg)[1,2]
  table2[4,i] <- tidy(reg)[1,3] 
  table2[5,i] <- summary(reg)$N
}
table2
