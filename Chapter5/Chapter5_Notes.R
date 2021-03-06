# Notes and presented code from Chapter 5

library(Lahman)
library(tidyverse)

# 5.1 The Run Expectancy Matrix

# An important concept in sabermetrics is the run expectancy matrix.

# The matrix is made up of the average number of runs scored for a specific half-inning state
# For example, we could have runners on 1st, 2nd, 3rd and combinations of them.  We also have
# 0-3 outs.  From there we can build 3 X 8 matrix. We don't include 3 outs becuse the half-inning
# is over and would always be 0.

#     0 outs  1 Out 2 Outs
# 100
# 110
# 101
# 111
# 000
# 001
# 011
# 010

# We denote the configuration of runners on base by the XXX values on the rows. The first slot
# is if a runner is on first, 2nd if a runner is on second, etc. Then we list the 8 possible
# combinations.

# 5.2 Runs Scored in the Remainder of the Inning

# We will study the 2016 in this chapter and use the data supplied by the author:

# read the column names:
fields <- read_csv("data/fields.csv")

# Load the 2016 data and add the column names:
data2016 <- read_csv("data/all2016.csv",
                     col_names = pull(fields, Header),
                     na = character())

# There's potential to score at each plate appearance with the chance being greater when runners are
# on base. 