netds <- read.csv("netflix_titles.csv", na.strings = c("", "NA"), stringsAsFactors =FALSE)
library(plotly)

values_table1 <- rbind(c('show_id', 'type', 'title', 'director', 'cast', 'country', 'date_added', 'release_year', 'rating' , 'duration', 'listed_in', 'description'), c("Unique ID for every Movie / TV Show", 
     "Identifier - A Movie or TV Show", 
     "Title of the Movie or TV Show", 
     "Director of the Movie /TV Show", 
    "Actors involved in the Movie / TV Show",
    "Country where the movie / show was produced",
    "Added date on Netflix",
    "Actual release year of the Movie / TV Show",
    "Rating type of the Movie or TV Show",
    "Total Duration - in minutes or number of seasons",
    "Genere",
    "The summary description"))

fig_table1 <- plot_ly(
  type = 'table',
  columnorder = c(1,2),
  columnwidth = c(12,12),
  header = list(
    values = c('<b>VARIABLES</b><br>', '<b>DESCRIPTION</b>'),
    line = list(color = '#506784'),
    fill = list(color = '#119DFF'),
    align = c('left','center'),
    font = list(color = 'white', size = 12),
    height = 40
  ),
  cells = list(
    values = values_table1,
    line = list(color = '#506784'),
    fill = list(color = c('#25FEFD', 'white')),
    align = c('left', 'left'),
    font = list(color = c('#506784'), size = 12),
    height = 30
    ))

fig_table1

# print(netds)
netds$show_id <- NULL
netds$rating <- as.factor(netds$rating)


library(lubridate)

netds$date_added <- mdy(netds$date_added)

netds$listed_in <- as.factor(netds$listed_in)

netds$type <- as.factor(netds$type)

data.frame("Variable"=c(colnames(netds)), "Missing Values"=sapply(netds, function(x) sum(is.na(x))), row.names=NULL)

#function to find a mode

mode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

netds$rating[is.na(netds$rating)] <- mode(netds$rating)

library(dplyr)

netds <- distinct(netds, title, country, type, release_year, .keep_all = TRUE)

# install.packages(“caret”)
# library("caret")