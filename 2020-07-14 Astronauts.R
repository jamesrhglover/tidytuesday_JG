## TidyTuesday - 2020 07 14

library(tidyverse)
library(ggplot2)

theme_set(theme_light())
theme_set(theme_bw())

library(ggdark)
theme_set(dark_theme_gray())
invert_geom_defaults()
# install.packages("ggthemes")
library(ggthemes)

dark_mode(theme_fivethirtyeight())

tuesdata <- tidytuesdayR::tt_load('2020-07-14')

astronauts <- tuesdata$astronauts

# id
# number
# nationwide_number
# name
# original_name
# sex
# year_of_birth
# nationality
# military_civilian
# selection
# year_of_selection
# mission_number
# total_number_of_missions
# occupation
# year_of_mission
# mission_title
# ascend_shuttle
# in_orbit
# descend_shuttle
# hours_mission
# total_hrs_sum
# field21
# eva_hrs_mission
# total_eva_hrs

# Which country has the most time in space?
space_time <- astronauts %>% 
    group_by(nationality) %>%
    summarise(total_hours = round(sum(hours_mission),0)) %>%
    arrange(desc(total_hours)) %>%
    head(9)

space_time %>%
    mutate(nationality = case_when(nationality == 'U.S.S.R/Russia' ~ 'Russia',
                                   nationality == 'U.S.' ~ 'USA',
                                   nationality == 'U.K.' ~ 'UK',
                                   nationality == 'U.K./U.S.' ~ 'US/UK',
                                   TRUE ~ nationality),
           possition = factor(total_hours, 
                              levels = names(sort(table(total_hours),
                                                  decending = TRUE))),
           ) %>%
    #filter(nationality != 'US/UK') %>%
    ggplot(aes(y = total_hours, x = reorder(nationality, total_hours), fill = nationality)) +
               #label = str_wrap(paste0(nationality," ",total_hours), width = 3))) +
    geom_bar(stat="identity" )+
    coord_flip() +
    #geom_text(hjust = -.5) + 
    theme(legend.position = "none") +
        #axis.title.x=element_blank(),
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          #axis.title.y=element_blank(),
          #axis.text.y=element_blank(),
          #axis.ticks.y=element_blank()
          #) +
    #scale_x_continuous( labels = scales::dollar) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 750000), labels = scales::comma) +
    scale_fill_brewer(palette="Set1") +
    #scale_fill_brewer(palette="Blues") +
    #scale_fill_brewer(direction = -1) + #theme_dark() +
    #expand_limits(x = 0) +
    labs(y = 'Total hours in Space', x = '',
         title = 'Which Country has had the most time in space',
         subtitle = 'Russia holds the record for the highest total time in space, this includes USSR but not Ukraine etc.')
 
# which country has the most austraughts

# Which country has the most diverse group of Astronaughts