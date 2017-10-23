library(tidyverse)
library(broom)
library(Hmisc)
library(foreign)
library(MASS)
library(rdrobust)
library(lfe)
library(statar)

options(prompt = "R> ", np.messages = FALSE, digits = 3, scipen = 3)

# table3.R

# open data
lso <- read.dta("Lindo-et-al.dta")

# regression with bandwidth de 0.6 + rectangular kernel weights
reg1 <- felm(probation_year1~gpalscutoff+gpaXgpalscutoff+
               gpaXgpagrcutoff|0|0|clustervar, 
             data = subset(lso, dist_from_cut >= -0.6 & 
                           dist_from_cut <= 0.6 & male == 1))
summary(reg1)

reg2 <- felm(probation_ever~gpalscutoff+gpaXgpalscutoff+
               gpaXgpagrcutoff|0|0|clustervar, 
             data = subset(lso, dist_from_cut >= -0.6 & 
                             dist_from_cut <= 0.6 & male == 1))
summary(reg2)

###  Table 3
  
# student groups
groups <- c("all", "lowHS", "highHS", "male", "female", "english", "noenglish")

# matrix to store results
table3 <- data.frame(matrix(NA, nrow=10, ncol=7))

# column names
colnames(table3) <- groups

# row names
rownames(table3) <- c("b(gpalscutoff_1)", "se(gpalscutoff_1)", 
                      "b(cons_1)", "se(cons_1)", "N_1", 
                      "b(gpalscutoff_2)", "se(gpalscutoff_2)", 
                      "b(cons_2)", "se(cons_2)", "N_2")

lso1 <- lso %>% filter(dist_from_cut >= -0.6 & dist_from_cut <= 0.6)

for(i in 1:7){
  group <- groups[i]
  reg1 <- felm(probation_year1~gpalscutoff+gpaXgpalscutoff+
                 gpaXgpagrcutoff|0|0|clustervar, 
               data = lso1[ which(lso1[, group] == 1), ])
  print(summary(reg1))
  reg2 <- felm(probation_ever~gpalscutoff+gpaXgpalscutoff+
                 gpaXgpagrcutoff|0|0|clustervar, 
               data = lso1[ which(lso1[, group] == 1), ])
  print(summary(reg2))
  table3[1,i] <- tidy(reg1)[2,2]
  table3[2,i] <- tidy(reg1)[2,3]
  table3[3,i] <- tidy(reg1)[1,2]
  table3[4,i] <- tidy(reg1)[1,3]
  table3[5,i] <- summary(reg1)$N
  table3[6,i] <- tidy(reg2)[2,2]
  table3[7,i] <- tidy(reg2)[2,3]
  table3[8,i] <- tidy(reg2)[1,2]
  table3[9,i] <- tidy(reg2)[1,3]
  table3[10,i] <- summary(reg2)$N
}
table3

  