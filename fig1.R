library(tidyverse)
library(broom)
library(Hmisc)
library(foreign)
library(MASS)
library(rdrobust)
library(statar)

options(prompt = "R> ", np.messages = FALSE, digits = 3, scipen = 3)

# figure1.R

# open data
lso <- read.dta("Lindo-et-al.dta")

# drop outliers (comment in order to use full sample)

lso <- lso %>% filter(dist_from_cut < 1.2 & dist_from_cut > -1.2)

# additional: hist dist from cutoff
# scatter plot makes no sense
truehist(lso$dist_from_cut)

# generate variable: distance to cutoff (rounded with cell sizes of 0.1 grade points)
# dist_from_cut_round10 = dist_from_cut_med10 db
lso <- lso %>% 
  mutate(dist_from_cut_round10 = round(dist_from_cut - 0.10/2 + 0.0001 , 1) + 0.10/2)

# freq = 1 for alll obs
lso <- lso %>% 
  mutate(freq = 1)

# replace variable freq by quantity in each group of dist_from_cut_round10
lso1 <- lso %>% 
  group_by(dist_from_cut_round10) %>% 
  summarise(freq = length(freq))

# dummy GPA cutoff
# gpalscutoff -- treat
lso1 <- lso1 %>% 
  mutate(gpalscutoff = as.numeric(dist_from_cut_round10 < 0))

# gpa = dist cutoff si <0; =0 alternativae
lso1 <- lso1 %>% 
  mutate(gpaXgpalscutoff = (dist_from_cut_round10)*gpalscutoff)

# gpa = dist cutoff si >0; =0 alternative
lso1 <- lso1 %>% 
  mutate(gpaXgpagrcutoff = (dist_from_cut_round10)*(1-gpalscutoff))


vec <- seq(-1.2, 1.2, 0.05)
add <- rep(NA, 7)

lso1 <- lso1 %>% mutate(dist_from_cut = NA, 
                        graph_freq = NA)

for(i in seq_along(vec)){
  reg <- lm(freq~gpalscutoff+gpaXgpalscutoff+gpaXgpagrcutoff,
             data = subset(lso1, dist_from_cut_round10 >= vec[i]-0.6 & 
                             dist_from_cut_round10 <= vec[i]+0.6))
  print(summary(reg))
  add[6] <- vec[i]
  add[3] <- as.numeric(add[6] < 0)
  add[4] <- add[3]*add[6]
  add[5] <- add[6]*(1-add[3])
  lso1 <- rbind(lso1, add)
  lso1[length(lso1$dist_from_cut), 7] <- 
    as.numeric(predict(reg, lso1)[length(lso1$dist_from_cut)]) 
}
rm(add, i, reg, vec)

lso1 <- lso1 %>% mutate(group_freq = ifelse(dist_from_cut>0, 1, 0))
lso_plot <- lso1[26:74,]
rdplot(lso_plot$graph_freq, lso_plot$dist_from_cut, c=0,
       x.label="1st Year GPA Minus Probation Cutoff", 
       y.label="Frequency Count", title="Figure 1",
       lowerend = -1.5, upperend = 1.5)

plot1 <- lso_plot %>% 
  ggplot(aes(x=dist_from_cut, y=graph_freq, color=factor(group_freq)))+
  geom_point(size=1.5) + stat_smooth(method = "loess", linetype=2) +
  theme_bw()+geom_vline(xintercept = 0, linetype=3)+
  xlab("1st Year GPA Minus Probation Cutoff")+
  ylab("Frequency Count")+ggtitle("Figure 1")+
  scale_x_continuous(breaks=seq(-1.5, 1.5, 0.5))+
  theme(legend.position="none")
plot1
