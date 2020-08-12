# Tidy Tuesday - 11-08-2020 ----

# Download data
library(tidytuesdayR)
library(tidyverse)

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

#theme_set(theme_light())
#theme_set(theme_dark())

tt <- tt_load("2020-08-11")


head(tt$avatar)
head(tt$scene_description)

# Chapter number and book number by IMDB rating?

str(tt$avatar)

book_details <- tt$avatar
book_scene <- tt$scene_description

# word cloud of character text in the shape of Yang
# most common words 

book_details %>%
    #distinct(writer)
    distinct(director)


table_data <- book_details %>%
                select(chapter_num, chapter, book, book_num, imdb_rating) %>%
                distinct()

# sum incidences for all weeks into one year
table_data <- table_data %>%
    group_by(chapter_num, chapter, book, book_num) %>%
    summarise(`IMDB Score`=sum(imdb_rating)) %>%
    as.data.frame() %>%
    arrange(book_num, chapter_num)

str(table_data)

ggplot(table_data,aes(x=chapter_num, y=book_num, fill=`IMDB Score`))+
geom_tile()
    
textcol <- "grey40"

ggplot(table_data,aes(x=chapter_num, y=book_num, fill=`IMDB Score`)) + 
    labs(x="Episode",y="Book",title="Avatar Ratings")+
    geom_tile() +
    scale_fill_gradientn(colours = c("light pink", "dark blue"))

p <- ggplot(table_data,aes(x=chapter_num, y=book_num, fill=`IMDB Score`)) + 
    labs(x="Episode",y="Book",title="Avatar Ratings")+
    geom_tile() +
    scale_fill_gradientn(colours = c("light pink", "dark blue"))

getwd()

ggsave(p,filename="avatar_rankings.png",height=5.5,width=8.8,units="in",dpi=200)





    theme(legend.position="right",legend.direction="vertical",
          legend.title=element_text(colour=textcol),
          #legend.margin=margin(grid::unit(0,"cm")),
          legend.text=element_text(colour=textcol,size=7,face="bold"),
          legend.key.height=grid::unit(0.8,"cm"),
          legend.key.width=grid::unit(0.2,"cm"),
          axis.text.x=element_text(size=10,colour=textcol),
          axis.text.y=element_text(vjust=0.2,colour=textcol),
          axis.ticks=element_line(size=0.4),
          plot.background=element_blank(),
          panel.border=element_blank(),
          #plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
          plot.title=element_text(colour=textcol,hjust=0,size=14,face="bold"))
   

#### Jack Davidsons - awesome code
    
library(tidyverse)
library(tidytuesdayR)
library(tvthemes)

tuesdata <- tidytuesdayR::tt_load("2020-08-11")

avatar <- tuesdata$avatar
scenes <- tuesdata$scene_description

import_avatar()
    
avatar_data <- avatar %>%
    mutate(
        book_chapt = case_when(
            book == "Water" ~ chapter_num,
            book == "Earth" ~ chapter_num + 21,
            book == "Fire" ~ chapter_num + 42
        ),
        true_chapt = case_when(
            book == "Water" ~ chapter_num - 1,
            book == "Earth" ~ chapter_num + 20,
            book == "Fire" ~ chapter_num + 41,
        ),
        true_chapt = if_else(true_chapt == max(true_chapt), max(book_chapt) + 1, true_chapt),
        imdb_rating = if_else(book_chapt == 20, 9.7, imdb_rating)
    ) %>%
    select(book, director, imdb_rating, book_chapt, true_chapt) %>%
    group_by(book) %>%
    mutate(series_rating = mean(imdb_rating)) %>%
    unique()
    
series_names <- avatar_data %>%
    group_by(book) %>%
    summarise(series_rating = min(series_rating),
              halfway = (min(true_chapt) + max(book_chapt) + 1) / 2)
    
avatar_data %>%
    ggplot(aes(x = book_chapt, y = imdb_rating)) +
    geom_step(aes(y = series_rating, x = true_chapt), size = 1) +
    geom_segment(aes(yend = series_rating, xend = book_chapt), linetype = 2) +
    geom_segment(
        data = avatar_data %>% filter(book == "Water"),
        aes(
            x = min(true_chapt),
            xend = max(book_chapt) + 1,
            y = series_rating,
            yend = series_rating
        ),
        size = 2,
        color = "#006992"
    ) +
    geom_segment(
        data = avatar_data %>% filter(book == "Earth"),
        aes(
            x = min(true_chapt),
            xend = max(book_chapt) + 1,
            y = series_rating,
            yend = series_rating
        ),
        size = 2,
        color = "#00916E"
    ) +
    geom_segment(
        data = avatar_data %>% filter(book == "Fire"),
        aes(
            x = min(true_chapt),
            xend = max(book_chapt) + 1,
            y = series_rating,
            yend = series_rating
        ),
        size = 2,
        color = "#FC440F"
    ) +
    geom_point(aes(color = book)) +
    geom_text(
        data = series_names,
        aes(
            y = series_rating + .025,
            x = halfway,
            label = book,
            color = book
        ),
        family = "Slayer",
        vjust = 0,
        hjust = .5,
        size = 12,
        alpha = 1 / 4
    ) +
    annotate(
        geom = "text",
        x = 10,
        y = 9.8,
        label = "Each point is\nan episode",
        family = "Slayer",
        size = 3, fontface = 2 
    ) +
    annotate(
        "curve",
        xend = 19,
        yend = 9.7,
        x = 15.5,
        y = 9.8,
        curvature = -.2,
        size = 1,
        arrow = arrow(type = "closed", length = unit(.5, "lines")),
        colour = "black"
    ) +
    annotate(
        geom = "text",
        x = 18,
        y = 7.5,
        label = "Even the lowest\nrated episode\nstill got 7.1/10",
        family = "Slayer",
        size = 2
    ) +
    annotate(
        "curve",
        xend = 12,
        yend = 7.1,
        x = 18,
        y = 7.3,
        curvature = -.3,
        arrow = arrow(type = "closed", length = unit(.5, "lines")),
        colour = "black"
    ) +
    annotate(
        geom = "text",
        x = 51.5,
        y = 9.775,
        label = "The final two episodes\nwere the highest rated\n with 9.8/10 each",
        family = "Slayer",
        size = 2
    ) +
    annotate(
        "curve",
        xend = 61.5,
        yend = 9.8,
        x = 57.5,
        y = 9.70,
        curvature = .4,
        arrow = arrow(type = "closed", length = unit(.5, "lines")),
        colour = "black"
    ) +
    annotate(
        "curve",
        xend = 63,
        yend = 9.85,
        x = 57.5,
        y = 9.85,
        curvature = -.4,
        arrow = arrow(type = "closed", length = unit(.5, "lines")),
        colour = "black"
    ) +
    labs(
        x = "",
        y = "IMDb Rating",
        title = "Living in Harmony",
        subtitle = "IMDb Ratings for Avatar: The Last Airbender tended to improve as the series went on!\n\n\n",
        caption = "Data from {appa} (https://github.com/averyrobbins1/appa)\n Visualisation by Jack Davison (Twitter @JDavison_)\nCode found at github.com/jack-davison"
      ) +
      theme_avatar(text.font = "Slayer") +
      theme(
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "none",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(
              color = "#d9cfb5",
              linetype = 3,
              size = 1
          ),
          plot.title = element_text(hjust = .5, size = 25, face = "bold"),
          plot.subtitle = element_text(hjust = .5, size = 10),
          plot.margin = unit(c(1, 1, 1, 1), "cm"),
          plot.caption = element_text(
              hjust = .5,
              size = 8,
              family = "sans",
              color = "#a89567"
          ),
          text = element_text(family = "Slayer")
      ) +
      scale_y_continuous(
          limits = c(7, 10),
          breaks = seq(7, 10, 0.5),
          sec.axis = dup_axis(name = NULL)
      ) +
      scale_color_manual(values = c(
          "Water" = "#006992",
          "Earth" = "#00916E",
          "Fire" = "#FC440F"
      )) +
      scale_fill_manual(values = c(
          "Water" = "#006992",
          "Earth" = "#00916E",
          "Fire" = "#FC440F"
      ))    
    
