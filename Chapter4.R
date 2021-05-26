# Chapter 4 Exercises.

library(Lahman)
library(tidyverse)
# 1. Relations between winning percentage and run differential across decadese.

# a. Fit linear regression model for the 60s, 70s, 80s, 90s

# 60s 
my_60_teams <- Teams %>% filter(yearID > 1960 && yearID < 1971) %>% 
  select(teamID, yearID, lgID, G, W, L, R, RA)

# 70s 
my_70_teams <- Teams %>% filter(yearID > 1970 && yearID < 1981) %>% 
  select(teamID, yearID, lgID, G, W, L, R, RA)

# 80s 
my_80_teams <- Teams %>% filter(yearID > 1980 && yearID < 1991) %>% 
  select(teamID, yearID, lgID, G, W, L, R, RA)

# 90s 
my_90_teams <- Teams %>% filter(yearID > 1990 && yearID < 2001) %>% 
  select(teamID, yearID, lgID, G, W, L, R, RA)
