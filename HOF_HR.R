# Comparing Ruth, A-Rod, Bond, and Aaron HR trajectories

library(Lahman)
library(tidyverse)

# use a function to get birthyear.  June 30th is the day we use for season age.
get_birthyear <- function(Name) {
  Names <- unlist(strsplit(Name," "))
  Master %>%
    filter(nameFirst == Names[1], 
           nameLast == Names[2]) %>%
    mutate(birthyear = ifelse(birthMonth >= 7,
                              birthYear + 1, birthYear),
           Player = paste(nameFirst, nameLast)) %>%
    select(playerID, Player, birthyear)
}

# bind data on each player into a data frame
PlayerInfo <- bind_rows(get_birthyear("Babe Ruth"),
                        get_birthyear("Hank Aaron"),
                        get_birthyear("Barry Bonds"),
                        get_birthyear("Alex Rodriguez")
                        )

# Join the player info with the batting data to get their cummulative HR by age
Batting %>%
  inner_join(PlayerInfo, by = "playerID") %>%
  mutate(Age = yearID - birthyear) %>%
  select(Player, Age, HR) %>%
  group_by(Player) %>%
  mutate(CHR = cumsum(HR)) -> HRData

# Plot the career HR 
ggplot(HRData, aes(x = Age, y = CHR, linetype = Player)) + geom_line()