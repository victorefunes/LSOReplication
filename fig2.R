library(tidyverse)
library(broom)
library(statar)
library(Hmisc)
library(foreign)
library(MASS)
library(rdrobust)
library(lfe)

options(prompt = "R> ", np.messages = FALSE, digits = 3, scipen = 3)

# fig2.R

# open data
lso <- read.dta("Lindo-et-al.dta")

# eliminar outliers
lso <- lso %>% filter(dist_from_cut < 1.2 & dist_from_cut > -1.2)

vec <- seq(-1.2, 1.2, 0.05)
add <- rep(NA, 7)

lso <- lso %>% mutate(probation_year1_hat = NA)
lso <- lso %>% dplyr::select(c(probation_year1, dist_from_cut, gpalscutoff,
                               gpaXgpalscutoff, gpaXgpagrcutoff, 
                               probation_year1_hat, dist_from_cut_med10))

for(i in seq_along(vec)){
  reg <- lm(probation_year1~gpalscutoff+gpaXgpalscutoff+gpaXgpagrcutoff,
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
# averages for probation_year1 for every dist_from_cut_med10
lso1 <- lso %>% group_by(dist_from_cut_med10) %>%
  summarise(probation_year1 = mean(probation_year1)) %>% print

lso1 <- lso1 %>% rename(dist_from_cut = dist_from_cut_med10)
lso1 <- lso1 %>% mutate(gpalscutoff = NA, gpaXgpalscutoff = NA,
                        gpaXgpagrcutoff = NA, probation_year1_hat = NA,
                        dist_from_cut_med10 = NA)
lso1 <- lso1 %>% dplyr::select(c(probation_year1, dist_from_cut, gpalscutoff, 
                                 gpaXgpalscutoff, gpaXgpagrcutoff, 
                                 probation_year1_hat, dist_from_cut_med10))
lso_add <- lso[25390:25438,]
lso1 <- rbind(lso1, lso_add); rm(lso_add)

lso1 <- lso1 %>% mutate(group_freq = ifelse(dist_from_cut>0, 1, 0))

rdplot(lso1$probation_year1, lso1$dist_from_cut, c=0,
       x.label="1st Year GPA Minus Probation Cutoff", 
       y.label="Probation Status", title="Figure 2",
       numbinl=10, numbinr=10, lowerend = -1.5, upperend = 1.5,
       type.dots="o")

plot2 <- lso1 %>% 
  ggplot(aes(x=dist_from_cut, y=probation_year1, group=group_freq))+
  geom_point(size=1.5)+geom_smooth(method = "loess", linetype=2)+
  geom_vline(xintercept = 0, linetype = 3)+theme_bw()+
  xlab("1st Year GPA Minus Probation Cutoff")+
  ylab("Probation Status")+ggtitle("Figure 2")+
  scale_x_continuous(breaks=seq(-1.5, 1.5, 0.5))+
  theme(legend.position="none")
plot2