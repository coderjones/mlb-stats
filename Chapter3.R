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


