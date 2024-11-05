library(ggplot2)
library(dplyr)
library(countrycode)

# Read in files into R
credits <- read.csv("credits.csv", header=T)
titles <- read.csv("titles.csv", header=T)

# Change genres variable for each item to a single, most important genre
for (i in 1:length(titles$genres)) {
  split <- strsplit(titles$genres[[i]],split = '\'')
  titles$genres[i] <- split[[1]][2]
}

# Change production countries variable for each item to a single country
for (i in 1:length(titles$production_countries)) {
  split <- strsplit(titles$production_countries[[i]],split = '\'')
  titles$production_countries[i] <- split[[1]][2]
}

## BONUS Oscar Data (Jerry) ### 
oscars <- read.csv("oscars.csv",sep="\t") %>% as_tibble() %>% filter(Year>1944)
soscars <- oscars %>% group_by(FilmId) %>% reframe(noscars=sum(Winner=="True"), nnons=sum(Winner!="True"))
soscars <- soscars %>% filter(FilmId!="") %>% rename(imdb_id=FilmId)
titles <- merge(titles,soscars,by="imdb_id", all.x = TRUE)


# Drops irrelevant columns in the titles dataset
titles <- select(titles, -c(description, imdb_id, tmdb_popularity, tmdb_score))

# Preprocess the column for transformations
titles$production_countries[titles$production_countries == "Lebanon"] <- "LB"

# Specifying certain country codes to be put into North America and Asia
iso_specifications <- c(US ="North America", CA = "North America", MX = "North America", IO = "Asia", SU = "Asia")

# Use countrycode function to classify
continents <- countrycode(sourcevar = titles[, "production_countries"],
                          origin = "iso2c",
                          destination = "continent", 
                          custom_match = iso_specifications)

# Add new column to the existing titles dataframe
titles$production_continent <- continents

### Jerry’s Code Starts ###

# Begin Actors Analysis
credits <-  read.csv("credits.csv") %>% as_tibble()
cac <- credits %>% filter(role=="ACTOR")
cdir <- credits %>% filter(role=="DIRECTOR")
# Check for Duplicates
sum(duplicated(credits))
# Get counts
actcount <- cac %>% group_by(person_id) %>% summarise(apr = n())
dircount <- cdir %>% group_by(person_id) %>% summarise(apr = n())
# Distirbution Testing
ggplot(actcount,aes(apr))+
  geom_bar()
ggplot(dircount,aes(apr))+
  geom_bar()
# Merge
cacm <- merge(cac[,1:2],actcount,by = "person_id")
dirm <- merge(cdir[,1:2],dircount,by="person_id")
# Optain Summary Statisitcs by Movie
mas <- cacm %>% group_by(id) %>% reframe(actormean=mean(apr),actormedian=median(apr),actorvariance=var(apr),actorrange=diff(range(apr)))
mds <- dirm %>% group_by(id) %>% reframe(directormean=mean(apr),directormedian=median(apr),directorsonmovie=n())
pmactors=merge(mas,mds,by="id",all.x=TRUE,all.y=TRUE)


# Carolina's Part: 
finalcols=merge(titles,pmactors,by="id",all.x = TRUE)
# Split into 2 separate dataframes to analyze both movies and TV shows, and their nuanced 
finalcolsMovie <- finalcols %>% filter(type == "MOVIE")
finalcolsMovie <- finalcolsMovie[!is.na(finalcolsMovie$imdb_score), ]
finalcolsShow <- finalcols %>% filter(type == "SHOW")
finalcolsShow <- finalcolsShow[!is.na(finalcolsShow$imdb_score), ]

## Data Preprocessing:

#Movies: 
sum(is.na(movies_trainLabels)) + sum(is.na(movies_testLabels)) #should be 315
ggplot(finalcolsMovie, aes(x = imdb_score)) +
  geom_histogram(binwidth = 0.5, fill = "brown", color = "black") +
  labs(title = "IMDb Scores for Movies", x = "IMDb Score", y = "Frequency")

# in order to figure out how to bin, let's also see the max imbd score value:
max_score <- max(finalcolsMovie$imdb_score, na.rm = TRUE)
max_score # returns 9.1
min_score <- min(finalcolsMovie$imdb_score, na.rm = TRUE)
min_score # returns 1.5

# obtain a 5-number summary for movie data to gain more insights into how to categorize the imbd lables
summary(finalcolsMovie$imdb_score)

# split: (poor: 1.5-5.6, 5.6-6.4: average, 6.4-7.1: good, 7.1-9.1: excellent) based on our 5-number summary
categorySplit <- c(1.5, 5.6, 6.4, 7.1, 9.1)
categories <- c("Poor", "Average", "Good", "Excellent")
finalcolsMovie$imdbCategory <- cut(finalcolsMovie$imdb_score, breaks = categorySplit, labels = categories, include.lowest = TRUE)
head(finalcolsMovie)
# drop the unnecessary columns: imbd_score after transformation, seasons since all movies have N/A seasons!
finalcolsMovie$imdb_score <- NULL
finalcolsMovie$seasons <- NULL
finalcolsMovie
ncol(finalcolsMovie)

# Machine Learning:
set.seed(12345)
indMovie <- sample(2, nrow(finalcolsMovie), replace=TRUE, prob=c(0.67, 0.33))

# training & testing labels for movies
movies_training <- finalcolsMovie[indMovie==1, c(1:20)]
movies_test <- finalcolsMovie[indMovie==2, c(1:19)]
movies_trainLabels <- finalcolsMovie[indMovie==1, 20]
movies_testLabels <- finalcolsMovie[indMovie==2, 20]

# just double checking that each observation had a classification!
sum(is.na(movies_trainLabels))

# Decision Tree
library(rpart)
library(DMwR)
library(rpart.plot)
ctree <- rpart(imdbCategory ~  
              actormedian + directormedian, data=movies_training,method="class",
              control=rpart.control(minsplit=5),parms=list(split="gini"))
library("rpart.plot")
ctree2 <- prune(ctree,cp=0.07)
ctree
prettyTree(ctree)
