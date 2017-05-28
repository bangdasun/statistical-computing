# dplyr tutorial
# 
# This package is expected to be widely used during my internship
# 
# Reference:
#
#   https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
#   http://genomicsclass.github.io/book/pages/dplyr_tutorial.html
#

library(dplyr)

### Example 1
# Data: mammals sleep, contains the sleep time and weights for mammals
setwd("C://Users//Bangda//Desktop//GR5206")
msleep = read.csv("msleep_ggplot2.csv")
head(msleep)

### Verbs in action
# 1. select()
# select columns
sleepDate = select(msleep, name, genus)
head(sleepDate)
head(select(msleep, -genus, -vore))
head(select(msleep, name, sleep_total:bodywt))

# 2. filter()
# select rows
head(filter(msleep, sleep_total >= mean(sleep_total)))
head(filter(msleep, sleep_total >= mean(sleep_total), order %in% c("Primates", "Rodentia")))

# 3. %>% 
# pipe operator, allows to pipe the output from one function to the input of another function
# without using nested functions
# without %>%
head(select(msleep, name, sleep_total))
# with %>%
msleep %>%
  select(name, sleep_total) %>%
  head

# 4. arrange()
# arrange (re-order) rows by particular columns
msleep %>%
  arrange(order) %>%
  head

# 5. mutate()
# add new columns to data frame
msleep %>%
  mutate(rem_prop = sleep_rem / sleep_total) %>%
  head

# 6. summarise()
# summaries statistics
msleep %>%
  summarise(avg_sleep = mean(sleep_total),
            var_sleep = var(sleep_total))

# group_by()
msleep %>%
  group_by(order) %>%
  summarise(ave_sleep = mean(sleep_total),
            min_sleep = min(sleep_total),
            max_sleep = max(sleep_total))

### Example 2
# Data: contains all 336776 flights that depart from NYC in 2013
# install.packages("nycflights13")
library(nycflights13)
dim(flights)
str(flights)
head(flights)

# For large data set, in dplyr, it's better to convert them into tbl_df form:
flights = tbl_df(flights)

# filter(): select all flights on Jan 1st
head(filter(flights, month == 1, day == 1))

# slice(): select rows by position
slice(flights, 1:10)

flights %>%
  filter(month == 1 | month == 2)
  slice(1:10)

# arrange(): can order the result by certain columns
flights %>%
  filter(month == 1, day == 28) %>%
  arrange(desc(dep_delay)) %>%
  slice(1:20)

# rename(): in select(), all variables not explicitly mentioned would be dropped
flights %>%
  rename(tail_num = tailnum)

# sample_n() and sample_frac()
flights %>%
  sample_n(10)

flights %>%
  sample_frac(.001)

# Ex: split data into individual planes and then summarise each plane 
#     by counting the number of flights and computing average distance
#     and arrival delay
delay = flights %>%
  group_by(tailnum) %>%
  summarise(count = n(),
            dist  = mean(distance, na.rm = TRUE),
            delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(count > 20, dist < 2000)

library(ggplot2)
ggplot(delay, aes(x = dist, y = delay)) + 
  geom_point(aes(size = count), alpha = .5) + 
  geom_smooth()
  scale_size_area()
