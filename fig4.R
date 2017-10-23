library(tidyverse)
library(broom)
library(Hmisc)
library(foreign)
library(MASS)
library(rdrobust)
library(lfe)
library(statar)

options(prompt = "R> ", np.messages = FALSE, digits = 3, scipen = 3)

# fig4.R

# open db
lso <- read.dta("Lindo-et-al.dta")

# drop outliers
lso <- lso %>% filter(dist_from_cut < 1.2 & dist_from_cut > -1.2)

vec <- seq(-1.2, 1.2, 0.05)
add <- rep(NA, 7)

lso <- lso %>% mutate(nextGPA_hat = NA)
lso <- lso %>% dplyr::select(c(nextGPA, dist_from_cut, gpalscutoff,
                               gpaXgpalscutoff, gpaXgpagrcutoff, 
                               nextGPA_hat, dist_from_cut_med10))

for(i in seq_along(vec)){
  reg <- lm(nextGPA~gpalscutoff+gpaXgpalscutoff+gpaXgpagrcutoff,
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
# averages for nextGPA for every dist_from_cut_med10
lso1 <- lso %>% group_by(dist_from_cut_med10) %>%
  summarise(nextGPA = mean(nextGPA, na.rm=TRUE)) %>% print

lso1 <- lso1 %>% rename(dist_from_cut = dist_from_cut_med10)
lso1 <- lso1 %>% mutate(gpalscutoff = NA, gpaXgpalscutoff = NA,
                        gpaXgpagrcutoff = NA, nextGPA_hat = NA,
                        dist_from_cut_med10 = NA)
lso1 <- lso1 %>% dplyr::select(c(nextGPA, dist_from_cut, gpalscutoff, 
                                 gpaXgpalscutoff, gpaXgpagrcutoff, 
                                 nextGPA_hat, dist_from_cut_med10))
lso_add <- lso[25390:25438,]
lso1 <- rbind(lso1, lso_add); rm(lso_add)

lso1 <- lso1 %>% mutate(group_freq = ifelse(dist_from_cut>0, 1, 0))

rdplot(lso1$nextGPA, lso1$dist_from_cut, c=0, p=1,
       x.label="1st Year GPA Minus Probation Cutoff", 
       y.label="Subsequent GPA Minus Cutoff", title="Figure 4",
       numbinl=20, numbinr=20, lowerend = -1.5, upperend = 1.5,
       type.dots=20, col.dots="gray20")

plot4 <- lso1 %>% 
  ggplot(aes(x=dist_from_cut, y=nextGPA, group=group_freq))+
  geom_point(size=1.5)+geom_smooth(method = "loess", linetype=2)+
  geom_vline(xintercept = 0, linetype = 3)+theme_bw()+
  xlab("1st Year GPA Minus Probation Cutoff")+
  ylab("Subsequent GPA Minus Cutoff")+ggtitle("Figure 4")+
  scale_x_continuous(breaks=seq(-1.5, 1.5, 0.5))+
  theme(legend.position="none")
plot4

