library(tidyverse)
library(broom)
library(Hmisc)
library(foreign)
library(MASS)
library(rdrobust)
library(lfe)
library(statar)

options(prompt = "R> ", np.messages = FALSE, digits = 3, scipen = 3)

# fig5.R

# open data
lso <- read.dta("Lindo-et-al.dta")

# drop outliers
lso <- lso %>% filter(dist_from_cut < 1.2 & dist_from_cut > -1.2)

vec <- seq(-1.2, 1.2, 0.05)
add <- rep(NA, 11)

lso <- lso %>% mutate(gradin4_hat = NA, gradin5_hat = NA, gradin6_hat = NA)
lso <- lso %>% dplyr::select(c(gradin4, gradin5, gradin6, dist_from_cut, 
                               gpalscutoff, gpaXgpalscutoff, gpaXgpagrcutoff, 
                               gradin4_hat, gradin5_hat, gradin6_hat, 
                               dist_from_cut_med10))

for(i in seq_along(vec)){
  reg_4 <- lm(gradin4~gpalscutoff+gpaXgpalscutoff+gpaXgpagrcutoff,
            data = subset(lso, dist_from_cut >= vec[i]-0.6 & 
                            dist_from_cut <= vec[i]+0.6))
  reg_5 <- lm(gradin5~gpalscutoff+gpaXgpalscutoff+gpaXgpagrcutoff,
              data = subset(lso, dist_from_cut >= vec[i]-0.6 & 
                              dist_from_cut <= vec[i]+0.6))
  reg_6 <- lm(gradin6~gpalscutoff+gpaXgpalscutoff+gpaXgpagrcutoff,
              data = subset(lso, dist_from_cut >= vec[i]-0.6 & 
                              dist_from_cut <= vec[i]+0.6))
  print(summary(reg_4))
  print(summary(reg_5))
  print(summary(reg_6))
  add[4] <- vec[i]
  add[5] <- as.numeric(add[4] < 0)
  add[6] <- add[5]*add[4]
  add[7] <- add[4]*(1-add[5])
  lso <- rbind(lso, add)
  lso[length(lso$dist_from_cut), 8] <- 
    as.numeric(predict(reg_4, lso)[length(lso$dist_from_cut)]) 
  lso[length(lso$dist_from_cut), 9] <- 
    as.numeric(predict(reg_5, lso)[length(lso$dist_from_cut)]) 
  lso[length(lso$dist_from_cut), 10] <- 
    as.numeric(predict(reg_6, lso)[length(lso$dist_from_cut)]) 
}

# collapse data using variable dist_from_cut_med10, by calculating
# averages for nextGPA for every dist_from_cut_med10
lso1 <- lso %>% group_by(dist_from_cut_med10) %>%
  summarise(gradin4 = mean(gradin4, na.rm=TRUE),
            gradin5 = mean(gradin5, na.rm=TRUE),
            gradin6 = mean(gradin6, na.rm=TRUE)) %>% 
  print

lso1 <- lso1 %>% rename(dist_from_cut = dist_from_cut_med10)
lso1 <- lso1 %>% mutate(gpalscutoff = NA, gpaXgpalscutoff = NA,
                        gpaXgpagrcutoff = NA, gradin4_hat = NA,
                        gradin5_hat = NA, gradin6_hat = NA,
                        dist_from_cut_med10 = NA)
lso1 <- lso1 %>% dplyr::select(c(gradin4, gradin5, gradin6, dist_from_cut, 
                                 gpalscutoff, gpaXgpalscutoff, 
                                 gpaXgpagrcutoff, gradin4_hat, 
                                 gradin5_hat, gradin6_hat,
                                 dist_from_cut_med10))
lso_add <- lso[25390:25438,]
lso1 <- rbind(lso1, lso_add); rm(lso_add)

lso1 <- lso1 %>% mutate(group_freq = ifelse(dist_from_cut>0, 1, 0))

rdplot(lso1$gradin4, lso1$dist_from_cut, c=0, p=3,
       x.label="1st Year GPA Minus Probation Cutoff", 
       y.label="Has graduated", title="Figure 5",
       y.lim = c(0, 1), col.lines="red",
       numbinl=20, numbinr=20, lowerend = -1.5, upperend = 1.5,
       type.dots=2, col.dots="red")
par(new=TRUE)
rdplot(lso1$gradin5, lso1$dist_from_cut, c=0, p=3,
       x.label="", y.label="", title="",
       y.lim = c(0, 1), col.lines="green",
       numbinl=20, numbinr=20, lowerend = -1.5, upperend = 1.5,
       type.dots=3, col.dots="green")
par(new=TRUE)
rdplot(lso1$gradin6, lso1$dist_from_cut, c=0, p=3,
       x.label="", y.label="", title="",
       y.lim = c(0, 1), col.lines="blue",
       numbinl=20, numbinr=20, lowerend = -1.5, upperend = 1.5,
       type.dots=4, col.dots="blue")
legend("topleft", c("Within 4 years", "Within 5 years", "Within 6 years"),
       pch=c(2, 3, 4), col=c("red", "green", "blue"), bty="n")

plot5 <- lso1 %>% 
  dplyr::select(c(dist_from_cut, group_freq, gradin4, gradin5, gradin6)) %>%
  gather(key=grad, value=perc, gradin4:gradin6) %>%
  ggplot(aes(x=dist_from_cut, y=perc, color=grad, group=group_freq))+
  geom_jitter(size=1.5)+
  geom_smooth(aes(group=grad), method = "loess", linetype=2, se=FALSE)+
  geom_vline(xintercept = 0, linetype = 3)+theme_bw()+
  xlab("1st Year GPA Minus Probation Cutoff")+
  ylab("Has graduated")+ggtitle("Figure 5")+
  scale_x_continuous(breaks=seq(-1.5, 1.5, 0.5))+
  scale_color_manual(name="", values=c("gray", "blue", "red"),
                     labels=c("Within 4 years", "Within 5 years", "Within 6 years"))
plot5


