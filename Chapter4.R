# Chapter 4 Exercises.

library(Lahman)
library(tidyverse)
# 1. Relations between winning percentage and run differential across decadese.

# a. Fit linear regression model for the 60s, 70s, 80s, 90s

# 60s 
my_60_teams <- Teams %>% filter(yearID > 1960, year < 1971) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA) %>% 
  mutate(RD = R - RA, Wpct = W / (W + L))

# 70s 
my_70_teams <- Teams %>% filter(yearID > 1970, yearID < 1981) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA) %>% 
  mutate(RD = R - RA, Wpct = W / (W + L))

# 80s 
my_80_teams <- Teams %>% filter(yearID > 1980, yearID < 1991) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA) %>% 
  mutate(RD = R - RA, Wpct = W / (W + L))

# 90s 
my_90_teams <- Teams %>% filter(yearID > 1990, yearID < 2001) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA) %>% 
  mutate(RD = R - RA, Wpct = W / (W + L))

lin60 <- lm(Wpct ~ RD, data = my_60_teams)
lin70 <- lm(Wpct ~ RD, data = my_70_teams)
lin80 <- lm(Wpct ~ RD, data = my_80_teams)
lin90 <- lm(Wpct ~ RD, data = my_90_teams)

lin60
lin70
lin80
lin90

predict(lin60, data.frame(RD = 10))
predict(lin70, data.frame(RD = 10))
predict(lin80, data.frame(RD = 10))
predict(lin90, data.frame(RD = 10))

#  This is the book's solution which is better.  In the future, keep this in mind.

Teams %>% filter(yearID >= 1961, yearID <= 2000) %>% 
  mutate(Era = ifelse(yearID <= 1970, "1961-1970",
                      ifelse(yearID <= 1980, "1971-1980",
                             ifelse(yearID <= 1990, "1981-1990", "1991-2000"))),
         WinPct = W / (W + L)) ->
  Eras

one_fit <- function(years){
  lm(WinPct ~ I(R - RA), 
     data = filter(Eras, Era == years))
}

the_eras <- c("1961-1970", "1971-1980", 
              "1981-1990", "1991-2000")
four_fits <- lapply(the_eras, one_fit)
names(four_fits) <- the_eras

sapply(four_fits, coef)

p10 <- function(fit){
  predict(fit, data.frame(R = 30, RA = 20))
}
sapply(four_fits, p10)

#### 2 Pythagorean Residuals for Poor and Great Teams in the 19th Century

# a. Fit a Pythagorean formula model to the run-diff, W/L for team in the 19th century
my_19th_teams <- Teams %>% filter(yearID <= 1900) %>%
  select(teamID, yearID, W, L, R, RA) %>%
  mutate(RD = R - RA, Wpct = W / (W + L))

my_19th_teams <- my_19th_teams %>% mutate(Wpct_pyt = R ^ 2 / (R ^ 2 + RA ^ 2))


lin19 <- lm(Wpct_pyt ~ RD, data = my_19th_teams)

library(broom)
res <- augment(lin19)

res %>% mutate(type = ifelse(Wpct_pyt > .7, "great",
                             ifelse(Wpct_pyt < .3, "bad", "other"))) -> res

ggplot(res, aes(Wpct_pyt, .resid, color=type)) +
  geom_point() +
  geom_hline(yintercept = 0)

# Book solution:

Teams %>% filter(yearID <= 1900) %>% 
  mutate(WinPct = W / (W + L)) ->
  D_19th

#(b) By inspecting the residual plot of your fitted model from (a), did the great and poor teams in the 19th century do better or worse than one would expect on the basis of their run differentials?
  
#  Below I construct a graph of the values of R - RA (horizontal) against the residual (vertical).  I color the point by the winning proportion (bad is WinPct < .3 and great is WinPct > .7).  We see some great teams with large positive residuals and bad teams with large negative residuals.  By exploring further, can find the identity of the teams with the large residuals.

# This code also doesn't plot

fit <- lm(WinPct ~ I(R - RA), data = D_19th)
fit
library(broom)
out <- augment(fit)
out %>% mutate(type = ifelse(WinPct > .7, "great",
                             ifelse(WinPct < .3, "bad", "other"))) -> out
ggplot(out, aes(I.R...RA., .resid, color=type)) +
  geom_point() +
  geom_hline(yintercept = 0)