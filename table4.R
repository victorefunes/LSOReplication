library(tidyverse)
library(broom)
library(statar)
library(Hmisc)
library(foreign)
library(MASS)
library(rdrobust)
library(lfe)
library(stargazer)

options(prompt = "R> ", np.messages = FALSE, digits = 3, scipen = 3)

# table4.R

# open data
lso <- read.dta("Lindo-et-al.dta")

### subsample: men
reg_ex <- felm(left_school~gpalscutoff+gpaXgpalscutoff+
                 gpaXgpagrcutoff|0|0|clustervar,
               data = subset(lso, dist_from_cut >= -0.6 & 
                               dist_from_cut <= 0.6 & male == 1))
summary(reg_ex)

# student groups
groups <- c("all", "lowHS", "highHS", "male", "female", "english", "noenglish")

lso1 <- lso %>% filter(dist_from_cut >= -0.6 & dist_from_cut <= 0.6)
regs <- list()
for(i in 1:7){
  group <- groups[i]
  regs[[i]] <- felm(left_school~gpalscutoff+gpaXgpalscutoff+
                 gpaXgpagrcutoff|0|0|clustervar, 
               data = lso1[which(lso1[, group] == 1),])
}

stargazer(regs[[1]], regs[[2]], regs[[3]], regs[[4]], regs[[5]],
          regs[[6]], regs[[7]], out = "table4.tex")
