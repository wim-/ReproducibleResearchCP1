## Reproducible Research - course project 1
## by: github.com/wim-
## This script:
##    1) loads the required packages
##    2) reads the data, cleans it up, filters required dates
##    3) evaluate steps per day
##    4) evaluate daily pattern
##    5) handle NA values
##    6) evaluate week/weekend activity pattern
## nota bene:
##    The script below assumes the data is contained
##    in a 'activity' csv file.
##    This file should be located in the working directory.

## set corret locale to display english labels
Sys.setlocale(category = "LC_TIME", locale = "C")

#####################################
## step 1: load packages           ##
#####################################
if (!require("dplyr")) {
      install.packages("dplyr")
      library(dplyr)
}
if (!require("lubridate")) {
      install.packages("lubridate")
      library(lubridate)
}
if (!require("tidyr")) {
      install.packages("tidyr")
      library(tidyr)
}
if (!require("lattice")) {
      install.packages("lattice")
      library(lattice)
}
if (!require("ggplot2")) {
      install.packages("ggplot2")
      library(ggplot2)
}

#####################################
## step 2: read and clean data     ##
#####################################
# read data with read.csv
# add leading zero's to interval to standardise to HHMM
# convert HHMM to HH:MM
# convert interval to proper time

my_data <-
      read.csv("activity.csv", sep=",", na.strings = "NA") %>%
      tbl_df() %>%
      mutate(date = ymd(date)) %>%
      mutate(interval = sprintf("%04.0f", interval)) %>%
      mutate(hour = hour(parse_date_time(interval, ("hm")))) %>%
      mutate(min = minute(parse_date_time(interval, ("hm")))) %>%
      mutate(time = paste(hour,min, sep =':')) %>%
      mutate(date_time = paste(date, time, sep = '_')) %>%
      mutate(date_time = ymd_hm(date_time)) %>%
      select(-hour, -min, -time) 

#####################################
## step3: Steps per day            ##
#####################################

steps_day_total <- my_data %>%
      filter(complete.cases(.)) %>%
      group_by(date) %>%
      summarise(nr_steps = sum(steps))

with(steps_day_total,
     hist(nr_steps,
          xlab = 'number of steps',
          main = 'Histogram of total nr of steps per day',
          col = 'green'
          )
     )

mean_steps <- mean(steps_day_total$nr_steps)
median_steps <- median(steps_day_total$nr_steps)

rm(steps_day_total)

rm(mean_steps, median_steps)

#####################################
## step 4: Daily activity pattern  ##
#####################################

steps_interval_avg <- my_data %>%
      filter(complete.cases(.)) %>%
      group_by(interval) %>%
      summarise(nr_steps = mean(steps))

with(steps_interval_avg,
     plot(interval,
          nr_steps,
          main = 'Average nr of steps per interval',
          type = 'l'
     )
)

max_avg_step_int <- steps_interval_avg %>%
      with(.,
           interval[nr_steps == max(.$nr_steps)]
           )


rm(max_avg_step_int)

#####################################
## step 5: Imputing missing values ##
#####################################

nr_missing <- sum(!complete.cases(my_data))

my_data_missing <- my_data %>%
      filter(!complete.cases(.)) %>%
      left_join(steps_interval_avg) %>%
      mutate(steps = nr_steps) %>%
      select(steps, date, interval, date_time)

my_data_complete <- my_data %>%
      filter(complete.cases(.)) %>%
      rbind_list(., my_data_missing) %>%
      arrange(date_time)

steps_day_total2 <- my_data %>%
      filter(complete.cases(.)) %>%
      group_by(date) %>%
      summarise(nr_steps = sum(steps))

with(steps_day_total2,
     hist(nr_steps,
          xlab = 'number of steps',
          main = 'Histogram of total nr of steps per day after correcting for missing values',
          col = 'green'
     )
)

mean_steps2 <- mean(steps_day_total2$nr_steps)
median_steps2 <- median(steps_day_total2$nr_steps)

rm(my_data_missing, steps_day_total2)
rm(steps_interval_avg)

rm(nr_missing, mean_steps2, median_steps2)
#####################################
## step 6: activity patterns       ##
#####################################

# create DF to identify weekday/weekend
days <- data.frame(
      c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),
      c(rep('weekday', each=5),rep('weekend', each=2))
      ) %>%
      tbl_df()

colnames(days) <- c('day_name', 'day_type')

# add weekday/weekend factor to my_data_complete
my_data_complete <- my_data_complete %>%
      mutate(day_name = weekdays(date)) %>%
      left_join(days) %>%
      select(-day_name)

rm(days)

# create panel plot
my_data_complete %>%
      group_by(interval, day_type) %>%
      summarise(nr_steps = mean(steps)) %>%
      qplot(interval,
            nr_steps,
            data = .,
            facets = day_type~.,
            )

# base plotting system 'panel' plot
steps_interval_avg2 <- my_data_complete %>%
      group_by(interval, day_type) %>%
      summarise(nr_steps = mean(steps))

par(mfrow = c(2, 1))

steps_interval_avg2 %>%
      filter(day_type == 'weekday') %>%
      with(.,
           plot(interval,
                nr_steps,
                type = 'l'
                 )
      )
steps_interval_avg2 %>%
      filter(day_type == 'weekend') %>%
      with(.,
           plot(interval,
                nr_steps,
                type = 'l'
           )
      )

par(mfrow = c(1,1))

# latice plot (flat lines)
xyplot(nr_steps~interval | day_type, 
       steps_interval_avg2,
       type = "l",
       layout = c(1,2)
       )
# no panel test
xyplot(nr_steps~interval, 
       steps_interval_avg2,
       type = "l"
)

# with(steps_interval_avg2,
#      plot(interval,
#           nr_steps,
#           main = 'Average nr of steps per interval',
#           type = 'l'
#      )
# )

