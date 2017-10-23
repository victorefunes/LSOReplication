library(tidyverse)
library(broom)
library(Hmisc)
library(statar)
library(foreign)
library(MASS)
library(rdrobust)
library(lfe)
library(stargazer)

options(prompt = "R> ", np.messages = FALSE, digits = 3, scipen = 3)

# table6.R

# open data
lso <- read.dta("Lindo-et-al.dta")

### subsample: men
reg_ex <- felm(gradin4~gpalscutoff+gpaXgpalscutoff+
                 gpaXgpagrcutoff|0|0|clustervar,
               data = subset(lso, dist_from_cut >= -0.6 & 
                               dist_from_cut <= 0.6 & male == 1))
summary(reg_ex)

# student groups
groups <- c("all", "lowHS", "highHS", "male", "female", "english", "noenglish")

lso1 <- lso %>% filter(dist_from_cut >= -0.6 & dist_from_cut <= 0.6)
regs4 <- list()
regs5 <- list()
regs6 <- list()

for(i in 1:7){
  group <- groups[i]
  regs4[[i]] <- felm(gradin4~gpalscutoff+gpaXgpalscutoff+
                      gpaXgpagrcutoff|0|0|clustervar, 
                    data = lso1[which(lso1[, group] == 1),])
  regs5[[i]] <- felm(gradin5~gpalscutoff+gpaXgpalscutoff+
                       gpaXgpagrcutoff|0|0|clustervar, 
                     data = lso1[which(lso1[, group] == 1),])
  regs6[[i]] <- felm(gradin6~gpalscutoff+gpaXgpalscutoff+
                       gpaXgpagrcutoff|0|0|clustervar, 
                     data = lso1[which(lso1[, group] == 1),])
}

stargazer(regs4[[1]], regs4[[2]], regs4[[3]], regs4[[4]], regs4[[5]],
          regs4[[6]], regs4[[7]], regs5[[1]], regs5[[2]], regs5[[3]], 
          regs5[[4]], regs5[[5]], regs5[[6]], regs5[[7]], regs6[[1]],
          regs6[[2]], regs6[[3]], regs6[[4]], regs6[[5]], regs6[[6]], 
          regs6[[7]], out = "table6.tex")
