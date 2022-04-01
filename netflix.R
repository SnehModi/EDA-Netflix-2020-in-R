# BE-ITB | GRP-12
# Exploratory Data Analysis on Netflix Data

# Reading the data
dataset <- read.csv("F:/Work/EDA-Netflix-2020-in-R/netflix_titles.csv", na.strings = c("", "NA"), stringsAsFactors =FALSE, sep = ',')

# Viewing the data
View(dataset)
head(dataset)

# Knowing the data
dim(dataset)
str(dataset)

# Data frame with specific columns
df <- dataset[, c('type', 'title', 'country', 'rating')]
head(df)
view(df)

# Data Analysis
dataset$show_id <- NULL
dataset$rating <- as.factor(dataset$rating)

library(lubridate)

dataset$date_added <- mdy(dataset$date_added)

dataset$listed_in <- as.factor(dataset$listed_in)

dataset$type <- as.factor(dataset$type)

data.frame("Variable"=c(colnames(dataset)), "Missing Values"=sapply(dataset, function(x) sum(is.na(x))), row.names=NULL)

#function to find a mode

mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

dataset$rating[is.na(dataset$rating)] <- mode(dataset$rating)

library(dplyr)

dataset <- distinct(dataset, title, country, type, release_year, .keep_all = TRUE)



# Exploratory Data Analysis

table(dataset$rating)
length(dataset$rating)

table(dataset$rating)/6232

# Find mean of the release years
mean(dataset$release_year)
mean(dataset$release_year, trim = 0.5)

# Median of the release years
median(dataset$release_year)

# Variance of the release years
var(dataset$release_year)

# Standard Deviation of the release years
sd(dataset$release_year)


# Min
min(dataset$release_year)

# Which Min
which.min(dataset$release_year)

# Max
max(dataset$release_year)

# which max
which.max(dataset$release_year)

# Sum of the release years
sum(dataset$release_year)
sum(dataset$release_year)/6232

# Range of release years
range(dataset$release_year)

# using quantile
quantile(dataset$release_year)

quantile(dataset$release_year, probs = 0.8)

quantile(dataset$release_year, probs = c(0.3, 0.6, 0.9))

# Correlation
cor(dataset$release_year, dataset$title)

# Covariance

# Summary of the release years
summary(dataset$release_year)

# Summary of the entire data set
summary(dataset)




# Algorithm





# Data Visualization
library(tibble)
library(dplyr)
library(plotly)
library(ggplot2)

amount_by_type <- dataset %>% group_by(type) %>% summarise(
  count = n())

figure00  <- ggplot(data = amount_by_type, aes(x= type, y= count, fill= type))+ 
  geom_bar(colour ="black", size= 0.8, fill = "dark green" ,  stat = "identity")+
  guides(fill= "none")+
  xlab("Netflix Content by Type") + ylab("Amount of Netflix Content")+
  ggtitle("Amount of Netflix Content By Type")

ggplotly(figure00, dynamicTicks = T)

# Amount of Netflix Content By Top 10 Country

# 1: split the countries (ex: "United States, India, South Korea, China" form to 'United States' 'India' 'South Korea' 'China') in the country column by using strsplit() function and then assign this operation to "k" for future use.

k <- strsplit(dataset$country, split = ", ")



# 2: Created a new dataframe by using data.frame() function. First column should be type = second one country=. Created type column by using rep() function. The function replicates the values in dataset$type depends on the length of each element of k. we used sapply()) function. Now k is our new data in sapply(). it means that calculate the lenght of each element of the k list so that we create type column. In the country column, we used just unlist() function. It simply converts the list to vector with all the atomic components are being preserved.

dataset_countries<- data.frame(type = rep(dataset$type, sapply(k, length)), country = unlist(k))

# 3: Changed the elements of country column as character by using as.charachter() function.

dataset_countries$country <- as.character(dataset_countries$country)

# 4: we created new grouped data frame by the name of amount_by_country. NA.omit() function deletes the NA values on the country column/variable. Then we groupped countries and types by using group_by() function (in the "dplyr" library). After that used summarise() function to summarise the counted number of observations on the new "count" column by using n() function.

amount_by_country <- na.omit(dataset_countries) %>%
  group_by(country, type) %>%
  summarise(count = n())

# 5: Actually we can use the "amount_by_country" dataframe to observe number of TV Show or Movie in countries. However, this list is too big to be visualized. Thus, we will create a new dataframe as table to see just top 10 countries by the name of "u".

# reshape() fonction will be used to create a reshaped grouped data. amount_by_country is used as data in the function. In this function, we will describe id variable, names of the value, time variable, and direction. Direction is character string, partially matched to either "wide" to reshape to wide format, or "long" to reshape to long format. Then we applied arrange() funtion to the reshaped grouped data. The dplyr function arrange() can be used to reorder (or sort) rows by one or more variables. In this part we sort count.movie column as descending. 

# To check to arguments and detailed descriptions of functions please use to help menu or google.com

# After the arrange funtion, top_n() function is used to list the specified number of rows.

u <- reshape(data=data.frame(amount_by_country),idvar="country",
             v.names = "count",
             timevar = "type",
             direction="wide") %>% arrange(desc(count.Movie)) %>%
  top_n(10)

# 6: names of the second and third columns are changed by using names() function as seen below.

names(u)[2] <- "Number_of_Movies"
names(u)[3] <- "Number_of_TV_Shows"

# 7: In the arrange() function we sorted our count.movie columns as descending but, now, we want to change this sort depends on the total values of "number of Movies" and "number of TV Shows". To sort a data frame in R, use the order() function. By default, sorting is ASCENDING. Therefore, we have to specify as descending. + is used to specify total operation.

u <- u[order(desc(u$Number_of_Movies +u$Number_of_TV_Shows)),] 

# 8: Now we can create our graph by using ggplot2 library. First argument of the ggplot function is our data.frame, then we specified our variables in the aes() function. coloured the graphy depends on the countries. Then typeof the graph is writed as geom_point and dot size specified as 5. After that we named x and y axis. Title of the graph is wroted by using ggtitle() function. 

library(ggplot2)



figure000 <- ggplot(u, aes(Number_of_Movies, Number_of_TV_Shows, colour=country))+ 
  geom_point(size=5)+
  xlab("Number of Movies") + ylab("Number of TV Shows")+
  ggtitle("Amount of Netflix Content By Top 10 Country")

ggplotly(figure000, dynamicTicks = T)





# Amount of Netflix Content By Time

# 0: To see number contents by time we have to create a new data.frame. This process is a little tiring. Maybe there is a short way but I couldn't find it. Lets start!

# 1: Title column take place in our data frame as character therefore I have to convert it to tbl_df format to apply the function below. If this column remains in character format and I want to implement the function, R returns an error: " Error in UseMethod("group_by_") : no applicable method for 'group_by_' applied to an object of class "character""     Therefore, first I assign it title column to f then convert the format as tibble and then assign it again to title column. 

f <- dataset$title
f <-tibble(f)
dataset$title <- f

# 2: new_date variable created by selecting just years. In this way, we can analyze and visualise the data more easy

library(lubridate)

dataset$new_date <- year(dataset$date_added)

# 2: df_by_date crated as a new grouped data frame. Titles are grouped depending the new_date(year) and then na.omit function applied to date column to remove NA values. Finally, number of added contents in a day calculated by using summarise() and n() functions.

df_by_date <- dataset$title %>% 
  group_by(dataset$new_date, dataset$type) %>% 
  na.omit(dataset$new_date) %>% 
  summarise(added_content_num = n())

# 3: now we will visualize our new grouped data frame. 

library(ggplot2)

Type<- df_by_date$`dataset$type`

Date <- df_by_date$`dataset$new_date`

Content_Number <- df_by_date$added_content_num

g1<- ggplot(df_by_date, aes(Date, Content_Number))+
  geom_line(aes(colour = Type), size = 2)+ 
  geom_point() + 
  xlab("Date") + 
  ylab("Number of Content")+
  ggtitle("Amount of Netflix Content By Time")

ggplotly(g1, dynamicTicks = T)

# From above we see that starting from the year 2016 the total amount of content was growing exponentially. We also notice how fast the amount of movies on Netflix overcame the amount of TV Shows. The reason for the decline in 2020 is that the data we have is ending begining of the 2020. At the begining of 2020, the number of ingredients produced is small. Before to say something about 2020 we have to see year-end data.




# Amount of Content by Rating

# Here plotly library used to visualise data. To see the graph in chunk output 
# or console you have to assign it to somewhere such as "fig"
library(plotly)

data <-dataset$title %>% 
  group_by(dataset$rating) %>% 
  summarise(content_num = n())

names(data) [1] <- "rating"
names(data) [2] <- "content"

# From the above, we created our new table to use in graph


figure2 <- plot_ly(data, labels = ~rating, values = ~content, type = 'pie')

# In the first part of visualisation, again, we have to specify our data labels,
# values,  x ad y axis and type of graph.

# In second part, adding title and other arguments of graph.

figure2 <- figure2 %>% layout(title = 'Amount of Content by Rating',
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

figure2




# Amount of content by Rating (Movie vs. TV Show)

# data preparation 
data2 <-dataset$title %>% 
  group_by(dataset$rating, dataset$type)%>% 
  summarise(content_num = n())

names(data2) [1] <- "rating"
names(data2) [2] <- "type"
names(data2) [3] <- "content"

newdata2 <- reshape(data=data.frame(data2),idvar="rating",
                    v.names = "content",
                    timevar = "type",
                    direction="wide") 

names(newdata2)[2] <- "Movie"
names(newdata2)[3] <- "TV Show"


newdata2$`TV Show`[is.na(newdata2$`TV Show`)] <- print(0)

# visualisation

library(plotly)

rating <- newdata2$rating
Movie <- newdata2$Movie
Tv_Show <- newdata2$`TV Show`


figure3 <- plot_ly(newdata2, x = ~rating, y = ~Movie, type = 'bar', name = 'Movie')

figure3 <- figure3 %>% add_trace(y = ~Tv_Show, name = 'TV Show')

figure3 <- figure3 %>% layout(yaxis = list(title = 'Count'),
                              barmode = 'stack', 
                              title="Amount of Content By Rating (Movie vs. TV Show)")

figure3



# Top 20 Genres on NETFLIX

library(crayon)

# before apply to strsplit function, we have to make sure that type of the variable is character.

dataset$listed_in<- as.character(dataset$listed_in)

t20 <- strsplit(dataset$listed_in, split = ", ")


count_listed_in<- data.frame(type = rep(dataset$type, 
                                        sapply(t20, length)), 
                             listed_in = unlist(t20))


count_listed_in$listed_in <- as.character(gsub(",","",count_listed_in$listed_in))


df_count_listed_in <- count_listed_in %>% 
  group_by(listed_in) %>% 
  summarise(count = n()) %>% 
  top_n(20) 

# visualisation

figure4 <- plot_ly(df_count_listed_in, x= ~listed_in, y= ~df_count_listed_in$count, type = "bar" )

figure4 <- figure4 %>% layout(xaxis=list(categoryorder = "array", 
                                         categoryarray = df_count_listed_in$listed_in,
                                         title="Genre"), yaxis = list(title = 'Count'),
                              title="20 Top Genres On Netflix")

figure4




