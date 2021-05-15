# Chapter 2 exercises from Analyzing Baseball Data in R

# 1: Top base stealers in the hall of fame

# a. Create vectors
SB <- c(1406,938,897,741,738,689,506,504,474)
CS <- c(335,307,212,195,109,162,136,131,114)
G <- c(3081,2616,3034,2826,2476,2649,2599,2683,2379)

# b. compute the number of stolen base attempts

SB.Attempt <- SB + CS

# c. For all players, compute the success rate

Success.Rate = SB / SB.Attempt

# d. Compute the number of stolen bases per game

SB.Game <- SB / G

# e. Construct a scatter plot of the stolen bases per game against success rate

library(tidyverse)
ggplot(data.frame(SB.Game, Success.Rate),
       aes(SB.Game, Success.Rate)) +
  geom_point()
Player = c('Rickey Henderson', 'Lou Brock', 'Ty Cobb',
           'Eddie Collins', 'Max Carey', 'Joe Morgan',
           'Luis Aparicio', 'Paul Molitor',
           'Roberto Alomar')
df <- tibble(Player, Success.Rate, SB.Game)
df %>% arrange(desc(Success.Rate)) %>% head(1)
df %>% arrange(desc(SB.Game)) %>% select(Player) %>% 
  slice(1) %>% pull()
# Ricky Henderson had the most stolen bases per game.

# 2. Calculate performance based on 10 plate appearances

# a. Create outcomes vector

outcomes <- c("Single","Out","Out","Single","Out","Double","Out","Walk","Out","Single")

# b. Use the table() function to create a frequency of outcomes

t <- table(outcomes)
plot(t)

# c. Create factors and plot the frequency

f.outcomes <- factor(outcomes, levels = c("Out","Walk","Single","Double"))
t1 <- table(f.outcomes)
plot(t1)

# d. focus only on walks
outcomes == "Walk"
sum(outcomes == "Walk")

# 3. Pitchers in the 350 wins club

# a. Create vectors
W <- c(373,354,364,417,355,373,361,363,511)
L <- c(208,184,310,279,227,188,208,245,316)
Name <- c("Alexander","Clemens","Galvin","Johnson","Maddux","Mathewson","Nichols","Spahn","Young")

# b. Compute the winning percentage for all pitchers
Win.PCT <- 100 * (W/(W + L))

# c. Create a data frame
Wins.350 <- data.frame(Name, W, L, Win.PCT)

# d. Use the arrange function to sort the data frame
library(dplyr)
Wins.350 %>% arrange(desc(Win.PCT))
head(Wins.350, 1)
tail(Wins.350, 1)

# 4. Pitchers in the 350 wins club continued

# a. Create vectors
SO <- c(2198,4672,1806,3509,3371,2502,1868,2583,2803)
BB <- c(951,1580,745,1363,999,844,1268,1434,1217)

# b. Compute strikeout-walk ratio
SO.BB.Ratio <- SO/BB

# c. Create data frame
SO.BB <- data.frame(Name,SO,BB,SO.BB.Ratio)

# d. find pitchers with a strikeout rate greater than 2.8
SO.BB %>% filter(SO.BB.Ratio > 2.8)

# e, Arrange the data frame by walks
SO.BB %>% arrange(desc(BB))

# No, the pitcher with the most walks didn't have the best strikeout to bb rate.

# 5. Pitcher Strikeout/Walk ratios
# a. Import Lahman
library(Lahman)

# b. compute cumulative strikeouts, walks, mid career year and the total innings pitched for all pitchers
career.pitching <- Pitching %>%
  group_by(playerID) %>%
  summarize(SO = sum(SO, na.rm = TRUE),
            BB = sum(BB, na.rm = TRUE),
            IPouts = sum(IPouts, na.rm = TRUE),
            midYear = median(yearID, na.rm = TRUE))

# c. Fileter to find pitchers with >= 1000 IPouts
career.pitching %>% filter(IPouts >= 10000) ->
  career_10000

# d. create a scatter plot of mid career year and ratio of strikeouts to walks
library(tidyverse)
ggplot(career_10000, aes(midYear,SO/BB)) +
geom_point()