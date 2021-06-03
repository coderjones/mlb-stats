Chapter 5 Notes
================

Start with the basic imports

``` r
library(Lahman)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.1     ✓ dplyr   1.0.6
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

## 5.1 The Run Expectancy Matrix

An important concept in sabermetrics is the run expectancy matrix.

The matrix is made up of the average number of runs scored for a
specific half-inning state. For example, we could have runners on 1st,
2nd, 3rd and combinations of them. We also have0-3 outs. From there we
can build 3 X 8 matrix. We don’t include 3 outs becuse the half-inning
is over and would always be 0.

| Base runners | 0 Outs | 1 Out | 2 Outs |
|--------------|--------|-------|--------|
| 100          |        |       |        |
| 110          |        |       |        |
| 101          |        |       |        |
| 111          |        |       |        |
| 000          |        |       |        |
| 001          |        |       |        |
| 011          |        |       |        |
| 010          |        |       |        |

We denote the configuration of runners on base by the XXX values on the
rows. The first slot is if a runner is on first, 2nd if a runner is on
second, etc. Then we list the 8 possible combinations.

## 5.2 Runs Scored in the Remainder of the Inning

We will study the 2016 in this chapter and use the data supplied by the
author.

read the column names:

``` r
fields <- read_csv("../data/fields.csv")
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   `Field number` = col_double(),
    ##   Description = col_character(),
    ##   Header = col_character()
    ## )

Load the 2016 data and add the column names:

``` r
data2016 <- read_csv("../data/all2016.csv",
                    col_names = pull(fields, Header),
                    na = character())
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   .default = col_character(),
    ##   INN_CT = col_double(),
    ##   BAT_HOME_ID = col_double(),
    ##   OUTS_CT = col_double(),
    ##   BALLS_CT = col_double(),
    ##   STRIKES_CT = col_double(),
    ##   AWAY_SCORE_CT = col_double(),
    ##   HOME_SCORE_CT = col_double(),
    ##   LEADOFF_FL = col_logical(),
    ##   PH_FL = col_logical(),
    ##   BAT_FLD_CD = col_double(),
    ##   BAT_LINEUP_ID = col_double(),
    ##   EVENT_CD = col_double(),
    ##   BAT_EVENT_FL = col_logical(),
    ##   AB_FL = col_logical(),
    ##   H_FL = col_double(),
    ##   SH_FL = col_logical(),
    ##   SF_FL = col_logical(),
    ##   EVENT_OUTS_CT = col_double(),
    ##   DP_FL = col_logical(),
    ##   TP_FL = col_logical()
    ##   # ... with 42 more columns
    ## )
    ## ℹ Use `spec()` for the full column specifications.

    ## Warning: 182 parsing failures.
    ##  row                    col           expected   actual                  file
    ## 3347 REMOVED_FOR_PR_RUN2_ID 1/0/T/F/TRUE/FALSE navad002 '../data/all2016.csv'
    ## 5180 REMOVED_FOR_PR_RUN2_ID 1/0/T/F/TRUE/FALSE cronc002 '../data/all2016.csv'
    ## 5185 REMOVED_FOR_PR_RUN3_ID 1/0/T/F/TRUE/FALSE mazan001 '../data/all2016.csv'
    ## 6233 REMOVED_FOR_PR_RUN2_ID 1/0/T/F/TRUE/FALSE calhk001 '../data/all2016.csv'
    ## 7319 REMOVED_FOR_PR_RUN2_ID 1/0/T/F/TRUE/FALSE hollm001 '../data/all2016.csv'
    ## .... ...................... .................. ........ .....................
    ## See problems(...) for more details.

There’s potential to score at each plate appearance with the chance
being greater when runners are on base.

RUNS.ROI = Total Runs Scored in Inning - Current Runs Scored.

``` r
data2016 %>%
        mutate(RUNS = AWAY_SCORE_CT + HOME_SCORE_CT,
               HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
               RUNS.SCORED =
                       (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) +
                       (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3)) ->
        data2016
```

We want to compute the max score for each half inning

``` r
data2016 %>%
        group_by(HALF.INNING) %>%
        summarize(Outs.Inning = sum(EVENT_OUTS_CT),
                  Runs.Inning = sum(RUNS.SCORED),
                  Runs.Start = first(RUNS),
                  MAX.RUNS = Runs.Inning + Runs.Start) ->
        half_innings
```

Now we’ll merge the dataframes

``` r
data2016 %>%
        inner_join(half_innings, by = "HALF.INNING") %>%
        mutate(RUNS.ROI = MAX.RUNS - RUNS) ->
        data2016
```

## 5.3 Creating the Matrix

From here I will be reading and continue my work with the end of chapter
exercises.
