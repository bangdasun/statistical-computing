# tidyr tutorial
# Reference:
#
#   https://blog.rstudio.org/2014/07/22/introducing-tidyr/
#

# tidyr is a package to get tidy data, which means:
#   (1) each column is a variable
#   (2) each row is an observation

# First: identify variables in the data, then move them into columns
# tidyr mainly use:
#   (1) gather(), like melt() in reshape2 package
#   (2) separate()
#   (3) spread()

library(dplyr)
library(tidyr)

# Data: an experiment with 3 people, take 2 different drugs and record heart rate
# original record form
messydata = data.frame(
  name = c("Wilbur", "Petunia", "Gregory"),
  a    = c(67, 80, 64),
  b    = c(56, 90, 50)
)

messydata
messydata %>%
  gather(drug, heartrate, a:b) # key - value pair

# Data: how much time people spend on phones at home and work at two times
messydata2 = data.frame(
  id      = 1:4,
  trt     = sample(rep(c("control", "treatment"), each = 2)),
  work_T1 = runif(4),
  home_T1 = runif(4),
  work_T2 = runif(4),
  home_T2 = runif(4)
)

messydata2
messydata2 %>%
  gather(key, time, -id, -trt) %>%                        # (data) - key - value - group
  separate(key, into = c("location", "time"), sep = "_")  # separate key

# wide -> long, long -> wide: comparison with reshape2 package
df <- data.frame(
  date = seq(ymd('20170710'), ymd('20170724'), by = 'day'),
  A = rnorm(15),
  B = rnorm(15),
  C = rnorm(15)
)

# reshape2 - specify key (id)
df_long <- melt(df, id.vars = c('date'))
df_long

df_wide <- dcast(df_long, date ~ variable)
df_wide

# tidyr - specify variables directly
df_long <- gather(df, 'type', 'value', 2:4)
df_long

df_wide <- spread(df_long, type, value)
df_wide
