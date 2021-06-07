# Chapter 5 exercises

library(Lahman)
library(tidyverse)

# 1. a. Run value of hits. Find mean run values for a double, and a triple
fields <- read_csv("data/fields.csv")

data2016 <- read_csv("data/all2016.csv",
                     col_names = pull(fields, Header),
                     na = character())

data2016 %>%
  mutate(RUNS = AWAY_SCORE_CT + HOME_SCORE_CT,
         HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
         RUNS.SCORED =
           (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) +
           (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3)) ->
  data2016

data2016 %>%
  group_by(HALF.INNING) %>%
  summarize(Outs.Inning = sum(EVENT_OUTS_CT),
            Runs.Inning = sum(RUNS.SCORED),
            Runs.Start = first(RUNS),
            MAX.RUNS = Runs.Inning + Runs.Start) ->
  half_innings

data2016 %>%
  inner_join(half_innings, by = "HALF.INNING") %>%
  mutate(RUNS.ROI = MAX.RUNS - RUNS) ->
  data2016

data2016 %>%
  mutate(BASES = 
           paste(ifelse(BASE1_RUN_ID > '', 1, 0),
                 ifelse(BASE2_RUN_ID > '', 1, 0),
                 ifelse(BASE3_RUN_ID > '', 1, 0), sep = ""),
         STATE = paste(BASES, OUTS_CT)) -> data2016
    
data2016 %>%
  mutate(NRUNNER1 =
           as.numeric(RUN1_DEST_ID == 1 | BAT_DEST_ID == 1),
         NRUNNER2 =
           as.numeric(RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 | 
                        BAT_DEST_ID == 2),
         NRUNNER3 =
           as.numeric(RUN1_DEST_ID == 3 | RUN2_DEST_ID == 3 | 
                        BAT_DEST_ID == 3),
         NOUTS = OUTS_CT + EVENT_OUTS_CT,
         NEW.BASES = paste(NRUNNER1, NRUNNER2,
                           NRUNNER3, sep = ""),
         NEW.STATE = paste(NEW.BASES, NOUTS)
         ) -> data2016

data2016 %>% 
  filter((STATE != NEW.STATE) | (RUNS.SCORED > 0)) -> data2016

data2016 %>%
  filter(Outs.Inning == 3) -> data2016C

data2016C %>%
  group_by(STATE) %>%
  summarise(Mean = mean(RUNS.ROI)) %>%
  mutate(Outs = substr(STATE, 5, 5)) %>%
  arrange(Outs) -> RUNS

data2016 %>%
  left_join(select(RUNS, -Outs), by = "STATE") %>%
  rename(Runs.State = Mean) %>%
  left_join(select(RUNS, -Outs),
            by = c("NEW.STATE" = "STATE")) %>%
  rename(Runs.New.State = Mean) %>%
  replace_na(list(Runs.New.State = 0)) %>%
  mutate(run_value = Runs.New.State - Runs.State +
           RUNS.SCORED) -> data2016

# double
data2016 %>% filter(EVENT_CD == 21) -> doubles

mean_doubles <- doubles %>%
  summarise(mean_run_values = mean(run_value))

# triple
data2016 %>% filter(EVENT_CD == 22) -> triples

mean_triples <- triples %>%
  summarise(mean_run_values = mean(run_value))

# The book does it this way.  They used a pre-processed data set for d2016
#d2016 %>% filter(EVENT_CD == 21) %>% 
#  summarize(M = mean(RUNS.VALUE))
#d2016 %>% filter(EVENT_CD == 22) %>% 
#  summarize(M = mean(RUNS.VALUE))

