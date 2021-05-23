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

# Working with Lahman Batting dataset a. load it
library(Lahman)

