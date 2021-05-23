# Chapter 3 exercises from Analyzing Baseball Data in R

library(tidyverse)

# 1. read in csv

hofpitching <- read.csv("data/hofpitching.csv")

hofpitching <- hofpitching %>%
  mutate(BF.group = cut(BF,
                        c(0,10000,15000,20000,30000),
                        labels = c("Less than 10000","(10,000, 15,000)",
                                   "(15,000, 20,000)", "more than 20,000")))

# a construct a frequency table of BF.group using the summarize() function

hof_bf <- summarize(group_by(hofpitching, BF.group), N=n())

# graph the frequency chart
ggplot(hof_bf, aes(BF.group, N)) + geom_col() 

# c construct an alternative graph of BF.group. 
ggplot(hof_bf, aes(BF.group, N)) + 
  geom_point() + coord_flip()

# 2 a. Using teh geom_histogram() function, construct a histogram of WAR for the pitchers in hof
ggplot(hofpitching, aes(x = WAR)) + geom_histogram()

# b. Identify the two outliers
hofpitching %>% filter(WAR > 125) %>% select(X, WAR)

# Walter Johnson and Cy Young
# This is the book's way of doing it:

hofpitching %>% arrange(desc(WAR)) %>% 
  slice(1:2) %>% select(X, WAR)

# 3. Define WAR.Season var for pitcher's season contribution

hofpitching <- hofpitching %>%
  mutate(WAR.Season = WAR/Yrs)

# a. Use geom_point() to construct parallel on-D scatterplots of WAR.Season for the different levels 
# of BF.group

ggplot(hofpitching, aes(WAR.Season, BF.group)) + geom_point() 

# b Use the geom_boxplot() function to construct parallel boxplots 

ggplot(hofpitching, aes(WAR.Season, BF.group)) + geom_boxplot() 

# 4. Explore pitcher's mid-career that was after 1960

hofpitching <- hofpitching %>%
  mutate(MidYear = (From+To)/2)

hofpitching.recent <- hofpitching %>%
  filter(MidYear >= 1960)

# a Use arrange() order the rows of the df by WAR.Season

hofpitching.recent %>% arrange(desc(WAR.Season)) %>% head(5)

# b Construct a dot plot of the values of WAR.Season where the labels are the pitcher's names

ggplot(hofpitching.recent, aes(WAR.Season, y = 1, label= X)) + geom_text(angle = 45)

# c top 2 midyear war
hofpitching.recent %>% arrange(desc(WAR.Season)) %>% 
  slice(1:2) %>% select(X, WAR.Season)

# 5. a. Construct a scatterplot of MidYears horizontal against WAR.Season vertical

ggplot(hofpitching.recent, aes(MidYear,WAR.Season)) + geom_point() + geom_smooth()

# b. No, no general pattern. Points look pretty evenly distributed. With smoothing, it looks like
# a downward trend towards the end.

# c
ggplot(hofpitching, 
       aes(x = MidYear, y = WAR.Season, label=X)) +
  geom_point() +
  geom_text(data = filter(hofpitching, 
                          MidYear < 1900, WAR.Season < 2))

# 6 Working with Lahman Batting dataset a. load it
library(Lahman)

# b. Collect in a single df seeason batting stats for Cobb, Williams, and Rose

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
PlayerInfo <- bind_rows(get_birthyear("Ty Cobb"),
                        get_birthyear("Ted Williams"),
                        get_birthyear("Pete Rose")
)

# Join the player info with the batting data to get their cummulative HR by age
Batting %>%
  inner_join(PlayerInfo, by = "playerID") %>%
  mutate(Age = yearID - birthyear) %>%
  select(playerID, Player, Age, H) %>%
  group_by(Player) %>%
  mutate(CHR = cumsum(H)) -> HitData

# The issue with my solution was because there's more than one Pete Rose
# Remove wrong Pete Rose
HitData <- HitData %>% filter(playerID != "rosepe02")

# Plot the career Hits
ggplot(HitData, aes(x = Age, y = CHR, linetype = Player)) + geom_line()


# THIS IS THE BOOK SOLUTION:


# (b) Collect in a single data frame the season batting statistics for the great hitters Ty Cobb, Ted Williams, and Pete Rose.

Master %>% filter(nameLast == "Cobb", 
                  nameFirst == "Ty") %>% 
  select(playerID) %>% pull() -> cobb_id
Master %>% filter(nameLast == "Williams", 
                  nameFirst == "Ted") %>% 
  select(playerID) %>% pull() -> williams_id
Master %>% filter(nameLast == "Rose", 
                  nameFirst == "Pete",
                  birthYear == 1941) %>% 
  select(playerID) %>% pull() -> rose_id
Batting %>% filter(playerID %in% 
                     c(cobb_id, williams_id, rose_id)) -> df

# (c) Add the variable Age to each data frame corresponding to the ages of the three players.


get_birthyear <- function(pid) {
  Master %>%
    filter(playerID == pid) %>%
    mutate(birthyear = ifelse(birthMonth >= 7,
                              birthYear + 1, birthYear)) %>%
    select(playerID, birthyear)
}
bdates <- bind_rows(get_birthyear(cobb_id),
                    get_birthyear(williams_id),
                    get_birthyear(rose_id))
df %>%
  inner_join(bdates, by = "playerID") %>%
  mutate(Age = yearID - birthyear) %>%
  select(playerID, Age, H) %>%
  group_by(playerID) %>%
  mutate(CH = cumsum(H)) -> df


#(d) Using the geom_line function, construct a line graph of the cumulative hit totals against age for Pete Rose.


ggplot(filter(df, playerID == rose_id), 
       aes(Age, CH)) +
  geom_line()



# 7 a. Create Mark McGwire and Sammy Sosa df

# Read in data
fields <- read_csv("data/fields.csv")
data1998 <- read_csv("data/all1998.csv",
                     col_names = pull(fields, Header))

sosa_id <- Master %>%
  filter(nameFirst == "Sammy", nameLast == "Sosa") %>%
  pull(retroID)

mac_id <- Master %>%
  filter(nameFirst == "Mark", nameLast == "McGwire") %>%
  pull(retroID)

mac.data <- data1998 %>%
  filter(BAT_ID == mac_id)

sosa.data <- data1998 %>%
  filter(BAT_ID == sosa_id)

#. b restrict the 2 df to the plays where a batting event occurred

mac.data <- filter(mac.data, BAT_EVENT_FL == TRUE)
sosa.data <- filter(sosa.data, BAT_EVENT_FL == TRUE)

# c number plate appearances
mac.data <- mutate(mac.data, PA = 1:nrow(mac.data))
sosa.data <- mutate(sosa.data, PA = 1:nrow(sosa.data))

#d  return the numbers of the PAs where they hit HRs

mac.HR.PA <- mac.data %>%
  filter(EVENT_CD == 23) %>%
  pull(PA)

sosa.HR.PA <- sosa.data %>%
  filter(EVENT_CD == 23) %>%
  pull(PA)

# e use diff to compute spacings between hrs

mac.spacings <- diff(c(0, mac.HR.PA))
sosa.spacings <- diff(c(0, sosa.HR.PA))

HR_Spacing <- rbind(data.frame(Player = "Mark McGwire", Spacing = mac.spacings),
  data.frame(Player = "Sammy Sosa", Spacing = sosa.spacings))

ggplot(HR_Spacing, aes(Spacing)) +
  geom_histogram() + facet_wrap(~ Player, ncol = 1)

HR_Spacing %>% group_by(Player) %>% 
  summarize(M = median(Spacing))
