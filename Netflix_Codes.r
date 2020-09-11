library(RCurl)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggthemes)

netflix <- read.csv (text = getURL("https://raw.githubusercontent.com/satwikshankar/Projects/master/input.csv"))

# Formating the date_added column to workable format

netflix$date_added <- as.Date(netflix$date_added, format = "%B %d, %Y")

# Basic data distribution

head(netflix)
glimpse(netflix)

options(repr.plot.width = 6, repr.plot.height = 6)

netflix_year <- netflix %>% 
    group_by(date_added,type) %>%
    summarise(shows_added = n()) %>% 
    ungroup() %>% 
    group_by(type) %>%
    mutate(Total_Number_of_Shows = cumsum(shows_added)) 


head(netflix_year)


netflix_year %>% 
ggplot(aes(x = date_added, y = Total_Number_of_Shows, color = type)) + 
geom_line(size = 1) + 
theme_wsj() +
theme(plot.title= element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
scale_x_date(date_breaks = '2 years', date_labels = "%Y") + 
labs(color = "Format", title="Movies vs TV Shows", subtitle = "Year wise Trend",  y = "No. of Shows", x = "Year")


options(repr.plot.width = 7, repr.plot.height = 7)


netflix_country <- netflix %>% 
    mutate(country = strsplit(as.character(country), ",")) %>% 
    unnest(country) %>%
    mutate(country = trimws(country, which = c("left"))) %>%
    group_by(country)%>%
    add_tally()

netflix_country <- netflix_country %>%
    select(country,n,type) %>%
    unique() %>%
    arrange(desc(n))

netflix_country_top5 <- netflix_country[1:10,]

head(netflix_country_top5)

ggplot( netflix_country_top5, aes(x = fct_reorder(country, n, .desc = TRUE), y = n))+
    geom_bar(stat = "identity")+
    facet_wrap(~type)+
    theme_wsj()+
    theme(plot.title= element_text(hjust = 0.5))+
    theme(plot.title= element_text(hjust = 0.5), axis.text.x = element_text(angle = 90), legend.position = 'none') +
    labs(title="Top 5 Countries based  on \n amount of content", y = "Content", x = "Country")


options(repr.plot.width = 7, repr.plot.height = 7)

show_categories <- netflix %>% 
    mutate(genre = strsplit(as.character(listed_in), ",")) %>% 
    unnest(genre) %>%
    mutate(genre = trimws(genre, which = "left")) %>%
    group_by(type, genre) %>%
    summarise(count = n()) %>%
    unique() 


head(show_categories)

show_categories %>% 
    ggplot(aes(x = fct_reorder(genre, count, .desc = TRUE), y = count)) + 
    geom_bar(stat = "identity") + 
    scale_x_discrete() +
    facet_wrap(~type, scales = 'free_x') + 
    theme_wsj() +
    theme(plot.title= element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
    labs(title="Top Genres", subtitle = "Movies Vs Tv Shows", x="Genres", y = "Count") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


options(repr.plot.width = 8, repr.plot.height = 8)


netflix_cast <- netflix %>% 
    select(c("cast","director"))%>% 
    gather(key = role, value = name, cast, director) %>% 
    filter(name != "") %>% 
    mutate(name = strsplit(as.character(name), ",")) %>% 
    unnest(name) %>%
    mutate(name = trimws(name, which = "left")) %>%
    group_by(name,role) %>% 
    summarise(count = n())

head(netflix_cast)

netflix_cast %>%
    group_by(role) %>% 
    top_n(10,count) %>%
    ungroup() %>%
    ggplot(aes(x = reorder(name,count), y = count)) + 
    geom_bar(stat = 'identity') +
    coord_flip()+
    facet_wrap(role~., scales = 'free_y') + 
    theme_wsj()+
    labs(title="Top Casts and Directors") 
