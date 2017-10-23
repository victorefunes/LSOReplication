library(tidyverse)
library(broom)
library(Hmisc)
library(foreign)
library(MASS)
library(rdrobust)
library(lfe)
library(statar)

setwd("F:/Archivos/Computadora CEDLAS/Curso-Evaluacion-Impacto-CEDLAS-2015-Docentes/RDD")
options(prompt = "R> ", np.messages = FALSE, digits = 3, scipen = 3)

# fig3.R

# open db
lso <- read.dta("Lindo-et-al.dta")

# drop m outliers
lso <- lso %>% filter(dist_from_cut < 1.2 & dist_from_cut > -1.2)

# filter observations; low HS GPA
lso <- lso %>% filter(lowHS == 1)

vec <- seq(-1.2, 1.2, 0.05)
add <- rep(NA, 7)

lso <- lso %>% mutate(left_school_hat = NA)
lso <- lso %>% dplyr::select(c(left_school, dist_from_cut, gpalscutoff,
                               gpaXgpalscutoff, gpaXgpagrcutoff, 
                               left_school_hat, dist_from_cut_med10))

for(i in seq_along(vec)){
  reg <- lm(left_school~gpalscutoff+gpaXgpalscutoff+gpaXgpagrcutoff,
            data = subset(lso, dist_from_cut >= vec[i]-0.6 & 
                            dist_from_cut <= vec[i]+0.6))
  print(summary(reg))
  add[2] <- vec[i]
  add[3] <- as.numeric(add[2] < 0)
  add[4] <- add[3]*add[2]
  add[5] <- add[2]*(1-add[3])
  lso <- rbind(lso, add)
  lso[length(lso$dist_from_cut), 6] <- 
    as.numeric(predict(reg, lso)[length(lso$dist_from_cut)]) 
}

# collapse data using variable dist_from_cut_med10, by calculating
# averages for left_school for every dist_from_cut_med10
lso1 <- lso %>% group_by(dist_from_cut_med10) %>%
  summarise(left_school = mean(left_school)) %>% print

lso1 <- lso1 %>% rename(dist_from_cut = dist_from_cut_med10)
lso1 <- lso1 %>% mutate(gpalscutoff = NA, gpaXgpalscutoff = NA,
                        gpaXgpagrcutoff = NA, left_school_hat = NA,
                        dist_from_cut_med10 = NA)
lso1 <- lso1 %>% dplyr::select(c(left_school, dist_from_cut, gpalscutoff, 
                                 gpaXgpalscutoff, gpaXgpagrcutoff, 
                                 left_school_hat, dist_from_cut_med10))
lso_add <- lso[17208:17256,]
lso1 <- rbind(lso1, lso_add); rm(lso_add)

lso1 <- lso1 %>% mutate(group_freq = ifelse(dist_from_cut>0, 1, 0))

rdplot(lso1$left_school, lso1$dist_from_cut, c=0, p=2,
       x.label="1st Year GPA Minus Probation Cutoff", 
       y.label="Left University Voluntarily", title="Figure 3",
       numbinl=10, numbinr=10, lowerend = -1.5, upperend = 1.5,
       type.dots="o")

plot3 <- lso1 %>% 
  ggplot(aes(x=dist_from_cut, y=left_school, group=group_freq))+
  geom_point(size=1.5)+geom_smooth(method = "loess", linetype=2)+
  geom_vline(xintercept = 0, linetype = 3)+theme_bw()+
  xlab("1st Year GPA Minus Probation Cutoff")+
  ylab("Left University Voluntarily")+ggtitle("Figure 3")+
  scale_x_continuous(breaks=seq(-1.5, 1.5, 0.5))+
  theme(legend.position="none")
plot3