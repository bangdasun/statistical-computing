# dplyr tutorial
# Reference:
#
#   http://genomicsclass.github.io/book/pages/dplyr_tutorial.html
#   http://genomicsclass.github.io/book/pages/dplyr_tutorial.html
#

library(dplyr)

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