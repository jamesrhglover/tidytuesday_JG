# Tidy Tuesday - 21-07-2020

# Completed 24-07-2020


# Download data
library(tidytuesdayR)
library(tidyverse)
theme_set(theme_light())
theme_set(theme_dark())



tt <- tt_load("2020-07-21")


#animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')
# animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv')
# brisbane_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/brisbane_complaints.csv')


tt %>% map(glimpse)

glimpse(tt$animal_outcomes)
# Data around anaimals from RSBCA that either been rehomed, euthanised , other, transfered


# tidy and organise data, instock becomes in care, humanised terms
animal_outcomes <- tt$animal_outcomes %>%
    mutate(outcome = fct_recode(outcome, "Currently In Care" = "In Stock"))


# showing the outcomes over time 
animal_outcomes %>%
    select(Total, year, animal_type , outcome) %>%
    group_by(year, animal_type) %>%
    summarise(Total = sum(Total)) %>%
    ggplot(aes(year, Total, colour = animal_type)) +
    #geom_col() +
    geom_line() +
    labs(x = "Year",
         y = "Number of animals",
         color = "Outcome")

# Wildlife has picked up in the last few years but mainly cats and dogs, let's keep all 3 and look at their outcomes


animal_outcomes %>%
    select(Total, year, animal_type , outcome) %>%
    #filter(animal_type %in% c('Dogs','Cats', 'Wildlife')) %>%
    filter(animal_type == 'Dogs') %>%
    group_by(year, outcome) %>%
    summarise(Total = sum(Total)) %>%
    ggplot(aes(year, Total, colour = outcome)) +
    #geom_col() +
    geom_line() +
    labs(x = "Year",
         y = "Number of animals",
         color = "Outcome")

# There's been a big drop in the amount of animal's Euthanized, however this appears to be a drop in cat's and dogs only and there has been an increase in the euthanized numbers of wild animals
# Hummmm, could they be calling cats and dogs, wild to exclude from the nice fluffy category?

# Let's look at the numbers Euthanised

animal_outcomes %>%
    select(Total, year, animal_type , outcome) %>%
    filter(animal_type %in% c('Dogs','Cats', 'Wildlife')) %>%
    filter(outcome == 'Euthanized') %>%
    group_by(year, animal_type) %>%
    summarise(Total = sum(Total)) %>%
    ggplot(aes(year, Total, colour = animal_type)) +
    #geom_col() +
    geom_line() +
    labs(title = "Number of Animals Euthanized",
         x = "Year",
         #y = "Number of animals ",
         color = "Animals")

# Good that overall numbers are dropping off

# Final presentation ----
# Got to be dog's

# We need to proportion the rescued number and those that had to be put down

animal_outcomes %>%
    filter(animal_type %in% c('Dogs')) %>%
    select(Total, year, outcome) %>%
    mutate(
        outcome = case_when(
            outcome %in% c("Rehomed", "Reclaimed") ~ "Rescued",
            outcome == "Euthanized" ~ "Euthanized",
            TRUE ~ "Other" )
        ) %>%
    group_by(year, outcome) %>%
    summarise(Total = sum(Total)) %>%
    mutate(outcome_percent = round(100 * Total/sum(Total),1)) %>%
    ggplot(aes(year, outcome_percent, colour = outcome)) +
    #geom_col() +
    geom_line() +
    labs(title = "Number of Animals Euthanized",
         x = "Year",
         #y = "Number of animals ",
         color = "Animals")

   
library(gganimate)

animal_outcomes %>%
    filter(animal_type %in% c('Dogs')) %>%
    select(Total, year, outcome) %>%
    mutate(
        outcome = case_when(
            outcome %in% c("Rehomed", "Reclaimed") ~ "Rescued",
            outcome == "Euthanized" ~ "Euthanized",
            TRUE ~ "Other" )
    ) %>%
    group_by(year, outcome) %>%
    summarise(Total = sum(Total)) %>%
    mutate(outcome_percent = round(100 * Total/sum(Total),1)) %>%
    ggplot(aes(year, outcome_percent, colour = outcome)) +
    #geom_col() +
    geom_line() +
    labs(title = "Number of Animals Euthanized",
         x = "Year",
         #y = "Number of animals ",
         color = "Animals") #+
    #transition_reveal(year)


library(ggimage)


dog_image_link <- "https://github.com/jamesrhglover/tidytuesday_JG/blob/master/logo-silhouette-dog-png-favpng-vTErGHPSeJQZ252JaVtaSVdJw.jpg"


animal_outcomes %>%
    mutate(
        image = dog_image_link
    ) %>% 
    filter(animal_type %in% c('Dogs')) %>%
    select(Total, year, outcome, image) %>%
    mutate(
        outcome = case_when(
            outcome %in% c("Rehomed", "Reclaimed") ~ "Rescued",
            outcome == "Euthanized" ~ "Euthanized",
            TRUE ~ "Other" )
    ) %>%
    group_by(year, outcome) %>%
    summarise(Total = sum(Total)) %>%
    mutate(outcome_percent = round(100 * Total/sum(Total),1)) %>%
    ggplot(aes(year, outcome_percent, colour = outcome)) +
    #geom_col() +
    geom_line() #+
    #geom_image(aes(image = image),
               #size = 0.3) #+
    #transition_reveal(time)



labs(title = "Number of Animals Euthanized",
     x = "Year",
     #y = "Number of animals ",
     color = "Animals") +
    
    
    
    
    filter(id == 1) %>%
    ggplot(aes(x = longitude, 
               y = latitude)) +
    geom_path() +
    geom_image(aes(image = image),
               size = 0.3) +
    transition_reveal(time)



# animate

