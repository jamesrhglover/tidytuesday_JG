# Tidy Tuesday - 21-07-2020

# Completed 24-07-2020


# Download data
library(tidytuesdayR)
library(tidyverse)
#theme_set(theme_light())
#theme_set(theme_dark())



tt <- tt_load("2020-07-21")


#animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')
# animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv')
# brisbane_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/brisbane_complaints.csv')


#tt %>% map(glimpse)

#glimpse(tt$animal_outcomes)
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
library(ggthemes)


dog_image_link <- "https://raw.githubusercontent.com/jamesrhglover/tidytuesday_JG/master/FAVPNG_logo-silhouette-dog_FpTMYJhm.png"

manual_colors <- c("#E76F51", "#264653", "#2A9D8F")

animal_outcomes %>%
   # mutate(image = dog_image_link) %>% 
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
    mutate(outcome_percent = Total/sum(Total),
           image = dog_image_link
           ) %>%
    ggplot(aes(year, outcome_percent)) +
    #ggplot(aes(x = year, y = outcome_percent, group = outcome, color = outcome)) +
    #geom_col() +
    geom_path(aes(colour = outcome, group = outcome), size = 1.25) +
    #geom_line(aes(colour = outcome), size = 1.25) +
    labs(
        title = "Australian Dog Adoption Rate",
        subtitle = "Dogs adoptions are on the rise",
        x = NULL,
        y = NULL,
        caption = " #TidyTuesday | Data source: RSPCA"
    ) +
    scale_y_continuous(
        limits = c(0, 0.8),
        breaks = c(0, 0.25, 0.5, 0.75),
        labels = scales::percent_format()
    ) +
    #scale_color_manual(
    #    values = c("#E76F51", "#264653", "#2A9D8F"), #E76F51
    #    name = NULL
    #) +
    #
    #theme(
    #    rect = element_rect(fill = "#F0EFEB"),
    #    text = element_text(family = "Lato"),
    #    plot.title = element_text(family = "Patrick Hand SC", 
    #                              hjust = 0.5, 
    #                              size = 26,
    #                              margin = margin(5, 0, 15, 0)),
    #    plot.subtitle = element_text(size = 10, hjust = 0.5),
    #    axis.text.x = element_text(size = 10),
    #    plot.caption = element_text(family = "Lato")
    #) +
    geom_image(aes(image = image), size = 0.1) +
    scale_color_manual(values = manual_colors) #+
    #transition_reveal(along = year) #+
    #view_follow()


dogs <- animal_outcomes %>%
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
    mutate(outcome_percent = Total/sum(Total)) %>%
    as.data.frame()

dogsplot <- dogs %>%
    mutate(image = dog_image_link) %>%
    ggplot(aes(x = year, y = outcome_percent, group = outcome, color = outcome)) +
    geom_path() +
    geom_image(aes(image = image), size = 0.1) +
    scale_color_manual(values = manual_colors) +
    #transition_reveal(along = year) +
    labs(
        title = "Australian Dog Adoption Rate",
        subtitle = "#TidyTuesday, RSPCA Australia",
        x = NULL,y = NULL
    ) +
    scale_y_continuous(
        limits = c(0, 0.8),
        breaks = c(0, 0.25, 0.5, 0.75),
        labels = scales::percent_format()
    ) 





    
    
bats %>%    
    mutate(image = bat_image_link) %>%
    ggplot(aes(x = longitude, y = latitude, group = id, color = id)) +
    geom_path() +
    geom_image(aes(image = image), size = 0.1) +
    scale_color_manual(values = bat_colors) +
    transition_reveal(along = time)


dog.animation = dogs_plot +
    transition_time(year) +
    labs(subtitle = "Year: {frame_time}") +
    shadow_wake(wake_length = 0.1)

animate(dog.animation, height = 500, width = 800, fps = 10, duration = 10,
        end_pause = 60, res = 100)


geom_image(aes(image = image), size = 0.1) +
    scale_color_manual(values = bat_colors) +
    transition_reveal(along = time)
    


theme_hc()
theme_economist() 
theme_stata()
    
    
    geom_image(aes(image = image),
               size = 0.1) +
    transition_reveal(year) 



+
    
    
    
    



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

    
    graph1 = gapminder %>%
        ggplot(aes(x=gdpPercap, y=lifeExp, color=continent, size=pop)) +
        geom_point(alpha = 0.7, stroke = 0) +
        theme_fivethirtyeight() +
        scale_size(range=c(2,12), guide="none") +
        scale_x_log10() +
        labs(title = "Life Expectancy vs GDP Per Capita by Country",
             x = "Income per person (GDP / capita)",
             y = "Life expectancy (years)",
             color = "Continent",
             caption = "Source: Gapminder") +
        theme(axis.title = element_text(),
              text = element_text(family = "Rubik"),
              legend.text=element_text(size=10)) +
        scale_color_brewer(palette = "Set2")
    
    graph1.animation = graph1 +
        transition_time(year) +
        labs(subtitle = "Year: {frame_time}") +
        shadow_wake(wake_length = 0.1)
    
    


# animate

