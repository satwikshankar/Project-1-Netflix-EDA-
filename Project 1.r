# 1.Installing necessary Packages
install.packages("ggthemes")
library(RCurl)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggthemes)


# 2.Importing data from github saved location via url
netflix <- read.csv (text = getURL("https://raw.githubusercontent.com/satwikshankar/Projects/master/input.csv"))


# 3.Basic data distribution
head(netflix)
glimpse(netflix)


# 4.Formating the date_added column to workable format
netflix$date_added <- as.Date(netflix$date_added, format = "%B %d, %Y")


# 5.Year-wise trend Analysis
netflix_year <- netflix %>% 
group_by(date_added,type) %>%
summarise(addedToday = n()) %>% 
ungroup() %>% 
group_by(type) %>%
mutate(Total_Number_of_Shows = cumsum(addedToday), 
           label = if_else(date_added == max(date_added,na.rm = TRUE), as.character(type), NA_character_))

head(netflix_year)

#Plotting the data
netflix_year %>% 
ggplot(aes(x = date_added, y = Total_Number_of_Shows, color = type)) + 
geom_line() + 
theme_gray() +
scale_x_date(date_breaks = '2 years', date_labels = "%Y") + 
labs(title="Year wise Trend", y = "No. of Shows", x = "Year") +
theme(legend.position = 'none') + 
geom_text(aes(label = label))


# 6.Country wise Content Availability
netflix_country <- netflix %>% 
mutate(country = strsplit(as.character(country), ",")) %>% 
unnest(country) %>%
mutate(country = trimws(country, which = c("left"))) %>%
group_by(country)%>%
add_tally()

netflix_country <- netflix_country%>%
select(country,n,type) %>%
unique()

netflix_country_top5 <- netflix_country[order(-netflix_country$n),]
netflix_country_top5 <- netflix_country_top5[1:10,]

head(netflix_country_top5)

#Plotting the data
ggplot( netflix_country_top5, aes(x = reorder(country, n), y = n, fill = type))+
geom_bar(stat = "identity")+
facet_wrap(~type)+
theme_gray()+
theme(axis.text.x = element_text(angle = 90), legend.position = 'none') +
labs(title="Top 5 Countries based on amount of content", y = "Content", x = "Country")


# 7.Category wise distribution 
show_categories <- netflix %>% 
    select(c('type','listed_in')) %>% 
    separate_rows(listed_in, sep = ',') %>%
    rename(Show_Category = listed_in)

show_categories$Show_Category <- trimws(show_categories$Show_Category)

head(show_categories)

#Plotting the data
show_categories %>% 
mutate(Show_Category = fct_infreq(Show_Category)) %>% # reordering categorical based on frequency
ggplot(aes(x = Show_Category)) + 
geom_bar() + 
scale_x_discrete() +
facet_wrap(~type, scales = 'free_x') + 
theme_gray() +
labs(title='Top Genres - Movies Vs Tv Shows',x='Genres', y = "Count") + 
theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# 8.Top Actors and Directors based on no. of Movies/TV Shows
netflix_cast <- netflix %>% 
select(c('cast','director')) %>% 
gather(key = 'role', value = 'name', cast, director) %>% 
filter(name != "") %>% 
mutate(name = strsplit(as.character(name), ",")) %>% 
unnest(name)

netflix_cast$name <- trimws(netflix_cast$name, which = "left")

head(netflix_cast)

netflix_cast_n<- netflix_cast %>% 
group_by(name,role) %>% 
summarise(count = n()) %>%
arrange(desc(count))

#Plotting the data
netflix_cast_n %>%
group_by(role) %>% 
top_n(10,count) %>%
ungroup() %>%
ggplot(aes(x = reorder(name,count), y = count, fill = role)) + 
    geom_bar(stat = 'identity') +
    scale_x_discrete() +
    facet_wrap(~role, scales = 'free_x') + 
    theme_gray()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = 'none') +
    labs(x = 'Name of the actor / director')

