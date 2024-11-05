install.packages("countrycode")
install.packages("leaps")

library(ggplot2)
library(dplyr)
library(countrycode)
library(leaps)

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

titles <- select(titles, -c(description, imdb_id, tmdb_popularity, tmdb_score))

titles$production_countries[titles$production_countries == "Lebanon"] <- "LB"

iso_specifications <- c(US ="North America", CA = "North America", MX = "North America", IO = "Asia", SU = "Asia")
continents <- countrycode(sourcevar = titles[, "production_countries"],
                          origin = "iso2c",
                          destination = "continent", 
                          custom_match = iso_specifications)

titles$production_continent <- continents

cac <- credits %>% filter(role=="ACTOR")
cdir <- credits %>% filter(role=="DIRECTOR")
# Check for Duplicates
sum(duplicated(credits))
# Get counts
actcount <- cac %>% group_by(person_id) %>% summarise(apr = n())
dircount <- cdir %>% group_by(person_id) %>% summarise(apr = n())
# Merge
cacm <- merge(cac[,1:2],actcount,by = "person_id")
dirm <- merge(cdir[,1:2],dircount,by="person_id")
# Optain Summary Statisitcs by Movie
mas <- cacm %>% group_by(id) %>% reframe(actormean=mean(apr),actormedian=median(apr),actorvariance=var(apr),actorrange=diff(range(apr)))
mds <- dirm %>% group_by(id) %>% reframe(directormean=mean(apr),directormedian=median(apr),directorsonmovie=n())
pmactors=merge(mas,mds,by="id",all.x=TRUE,all.y=TRUE)


# Performs the Left MErge
finalcols=merge(titles,pmactors,by="id",all.x = TRUE)

# Change genres and continents to be factor variables
finalcols$production_continent <- factor(finalcols$production_continent)
finalcols$genres <- factor(finalcols$genres)


# Split into 2 separate dataframes to analyze both movies and TV shows
finalcolsMovie <- finalcols %>% filter(type == "MOVIE")
finalcolsShow <- finalcols %>% filter(type == "SHOW")

## Data cleaning for Movie dataset ##

# Removing irrelevant columns
finalcolsMovie <- finalcolsMovie[, -c(3, 8, 9, 15, 17)]
# Removing NA IMDB scores
finalcolsMovie <- finalcolsMovie[!(is.na(finalcolsMovie$imdb_score)), ]
# Removing NA actor median rows
finalcolsMovie <- finalcolsMovie[!(is.na(finalcolsMovie$actormedian)), ]
# Removing NA director mean rows
finalcolsMovie <- finalcolsMovie[!(is.na(finalcolsMovie$directormean)), ]
# Removing NA IMDB vote counts
finalcolsMovie <- finalcolsMovie[!(is.na(finalcolsMovie$imdb_votes)), ]
# Removing NA production continents
finalcolsMovie <- finalcolsMovie[!(is.na(finalcolsMovie$production_continent)), ]
# Change NA values relating to Oscar awards to 0
finalcolsMovie$noscars[is.na(finalcolsMovie$noscars)] <- 0
finalcolsMovie$nnons[is.na(finalcolsMovie$nnons)] <- 0

finalMovieWithAgeCert <- finalcolsMovie
finalMovieWithoutAgeCert <- finalcolsMovie

# Removing all age certifications with nothing
finalMovieWithAgeCert <- finalMovieWithAgeCert[!(finalMovieWithAgeCert$age_certification==""), ]
# Removing age certification column all together
finalMovieWithoutAgeCert <- finalMovieWithoutAgeCert[, -c(4)]


## Data cleaning for Show dataset ##

# Removing irrelevant columns
finalcolsShow <- finalcolsShow[, -c(3, 8, 12, 13, 15, 17, 19, 20, 21)]
# Removing NA IMDB scores
finalcolsShow <- finalcolsShow[!(is.na(finalcolsShow$imdb_score)), ]
# Removing NA actor median rows
finalcolsShow <- finalcolsShow[!(is.na(finalcolsShow$actormedian)), ]
# Removing NA IMDB vote counts
finalcolsShow <- finalcolsShow[!(is.na(finalcolsShow$imdb_votes)), ]
# Removing NA production continents
finalcolsShow <- finalcolsShow[!(is.na(finalcolsShow$production_continent)), ]
# Removing NA genre
finalcolsShow <- finalcolsShow[!(is.na(finalcolsShow$genres)), ]

finalShowWithAgeCert <- finalcolsShow
finalShowWithoutAgeCert <- finalcolsShow

# Removing all age certifications with nothing
finalShowWithAgeCert <- finalShowWithAgeCert[!(finalShowWithAgeCert$age_certification==""), ]
# Removing age certification column all together
finalShowWithoutAgeCert <- finalShowWithoutAgeCert[, -c(4)]



#### LINEAR REGRESSION ANALYSIS ####

## Generating training and testing splits for each movie model (2 models total)

set.seed(12345)

indMovieAge <- sample(2, nrow(finalMovieWithAgeCert), replace=TRUE, prob=c(0.67, 0.33))
indMovieNoAge <- sample(2, nrow(finalMovieWithoutAgeCert), replace=TRUE, prob=c(0.67, 0.33))

# For dataset with age certification included
movies_training_age <- finalMovieWithAgeCert[indMovieAge==1, ]
movies_test_age <- finalMovieWithAgeCert[indMovieAge==2, ]
# movies_trainLabels_age <- finalMovieWithAgeCert[indMovieAge==1, 7]
# movies_testLabels_age <- finalMovieWithAgeCert[indMovieAge==2, 7]

# For dataset with age certification dropped
movies_training_no_age <- finalMovieWithoutAgeCert[indMovieNoAge==1, ]
movies_test_no_age <- finalMovieWithoutAgeCert[indMovieNoAge==2, ]
# movies_trainLabels_no_age <- finalMovieWithoutAgeCert[indMovieNoAge==1, 6]
# movies_testLabels_no_age <- finalMovieWithoutAgeCert[indMovieNoAge==2, 6]

## Generating training and testing splits for each show model (2 models total)

set.seed(12345)

indShowAge <- sample(2, nrow(finalShowWithAgeCert), replace=TRUE, prob=c(0.67, 0.33))
indShowNoAge <- sample(2, nrow(finalShowWithoutAgeCert), replace=TRUE, prob=c(0.67, 0.33))

# For dataset with age certification included
shows_training_age <- finalShowWithAgeCert[indShowAge==1, ]
shows_test_age <- finalShowWithAgeCert[indShowAge==2, ]
shows_test_age <- shows_test_age[!(shows_test_age$genres=="fantasy"),]
shows_test_age <- shows_test_age[!(shows_test_age$genres=="sport"),]
# shows_trainLabels_age <- finalShowWithAgeCert[indShowAge==1, 8]
# shows_testLabels_age <- finalShowWithAgeCert[indShowAge==2, 8]

# For dataset with age certification dropped
shows_training_no_age <- finalShowWithoutAgeCert[indShowNoAge==1, ]
shows_test_no_age <- finalShowWithoutAgeCert[indShowNoAge==2, ]
shows_test_no_age <- shows_test_no_age[!(shows_test_no_age$genres=="sport"),]
# shows_trainLabels_no_age <- finalShowWithoutAgeCert[indShowNoAge==1, 7]
# shows_testLabels_no_age <- finalShowWithoutAgeCert[indShowNoAge==2, 7]


### MODEL CREATION ###

## MOVIES WITH AGE CERTIFICATION ##

# Using regsubsets to determine regression line

# movies_with_age_regsubsets_model <- regsubsets(imdb_score ~ release_year + age_certification + runtime
#                                     + genres + imdb_votes + noscars + nnons + production_continent
#                                     + actormedian + actorrange + directormean +
#                                       directormedian + directorsonmovie, data=movies_training_age, nvmax=33)
# 
# summary(movies_with_age_regsubsets_model)
# summary(movies_with_age_regsubsets_model)$adjr2
# coef(movies_with_age_regsubsets_model, id=9)

# Using lm to determine regression line

movies_with_age_model_lm <- lm(imdb_score ~ release_year + age_certification + runtime 
                                    + genres + imdb_votes + noscars + nnons + production_continent
                                    + actormedian + actorrange + directormean + 
                                      directormedian + directorsonmovie, data=movies_training_age)

movies_age_step_lm <- step(movies_with_age_model_lm, direction="both")
summary(movies_age_step_lm)

movies_with_age_model_lm <- lm(imdb_score ~ release_year + runtime + I(factor(genres) == "animation")
                                + I(factor(genres) == "documentation") + I(factor(genres) == "drama")
                                + I(factor(genres) == "fantasy") + imdb_votes + nnons + noscars
                                + I(factor(production_continent) == "Asia") + I(factor(production_continent) == "Europe") 
                                + I(factor(production_continent) == "North America") + actormedian + actorrange, 
                                data=movies_training_age)

summary(movies_with_age_model_lm)

## MOVIES WITHOUT AGE CERTIFICATION ##

# Using regsubsets to determine regression line

# movies_without_age_regsubsets_model <- regsubsets(imdb_score ~ release_year + runtime 
#                                                + genres + imdb_votes + noscars + nnons + production_continent
#                                                + actormedian + actorrange + directormean + 
#                                                  directormedian + directorsonmovie, data=movies_training_no_age,
#                                                nvmax=31)
# 
# summary(movies_without_age_regsubsets_model)
# summary(movies_without_age_regsubsets_model)$adjr2
# coef(movies_without_age_regsubsets_model, id=9)

# Using lm to determine regression line

movies_without_age_model_lm <- lm(imdb_score ~ release_year + runtime 
                               + genres + imdb_votes + noscars + nnons + production_continent
                               + actormedian + actorrange + directormean + 
                                 directormedian + directorsonmovie, data=movies_training_no_age)

movies_no_age_step_lm <- step(movies_without_age_model_lm, direction="both")
summary(movies_no_age_step_lm)

movies_without_age_model_lm <- lm(imdb_score ~ release_year + runtime + I(factor(genres) == "animation")
                               + I(factor(genres) == "comedy") + I(factor(genres) == "crime")
                               + I(factor(genres) == "documentation") + I(factor(genres) == "drama") 
                               + I(factor(genres) == "fantasy") + I(factor(genres) == "music") 
                               + I(factor(genres) == "war") + imdb_votes + nnons + noscars 
                               + I(factor(production_continent) == "Asia") + actormedian 
                               + actorrange + directormedian, 
                               data=movies_training_no_age)

summary(movies_without_age_model_lm)


## SHOWS WITH AGE CERTIFICATION ##

# Using regsubsets to determine regression line

# shows_with_age_regsubsets_model <- regsubsets(imdb_score ~ release_year + age_certification + runtime 
#                                                + genres + seasons + imdb_votes + production_continent
#                                                + actormedian + actorrange, data=shows_training_age, nvmax=31)
# 
# summary(shows_with_age_regsubsets_model)
# summary(shows_with_age_regsubsets_model)$adjr2
# coef(shows_with_age_regsubsets_model, id=9)

# Using lm to determine regression line

shows_with_age_model_lm <- lm(imdb_score ~ release_year + age_certification + runtime 
                              + genres + seasons + imdb_votes + production_continent
                              + actormedian + actorrange, data=shows_training_age)

shows_age_step_lm <- step(shows_with_age_model_lm, direction="both")
summary(shows_age_step_lm)

shows_with_age_model_lm <- lm(imdb_score ~ release_year + I(factor(age_certification) == "TV-G") 
                              + I(factor(age_certification) == "TV-PG") + runtime + I(factor(genres) == "crime") 
                              + I(factor(genres) == "documentation")+ I(factor(genres) == "drama") + seasons 
                              + imdb_votes + I(factor(production_continent) == "Asia") + actormedian, 
                               data=shows_training_age)

summary(shows_with_age_model_lm)

## SHOWS WITHOUT AGE CERTIFICATION ##

# Using regsubsets to determine regression line

# shows_without_age_regsubsets_model <- regsubsets(imdb_score ~ release_year + runtime + genres 
#                                                  + seasons + imdb_votes + production_continent
#                                                  + actormedian + actorrange, data=shows_training_no_age,
#                                                  nvmax=27)
# 
# summary(shows_without_age_regsubsets_model)
# summary(movies_with_age_regsubsets_model)$adjr2
# coef(movies_with_age_regsubsets_model, id=9)

# Using lm to determine regression line

shows_without_age_model_lm <- lm(imdb_score ~ release_year + runtime + genres
                                 + seasons + imdb_votes + production_continent
                                 + actormedian + actorrange, data=shows_training_no_age)

shows_no_age_step_lm <- step(shows_without_age_model_lm, direction="both")
summary(shows_no_age_step_lm)

shows_without_age_model_lm <- lm(imdb_score ~ release_year + runtime + I(factor(genres) == "documentation") 
                                 + I(factor(genres) == "reality") + seasons + imdb_votes 
                                 + production_continent + actormedian, 
                                  data=shows_training_no_age)

summary(shows_without_age_model_lm)


### TESTING VERIFICATION ###

# Function to calculate RMSE of each model
rmse <- function(y,yhat) {
  num <- sum((y - yhat)^2)
  denom <- length(y)
  return(sqrt(num/denom))
}

## MOVIES WITH AGE ##

movies_with_age_step_rmse <- rmse(movies_test_age$imdb_score, predict(movies_age_step_lm, movies_test_age))
movies_with_age_lm_rmse <- rmse(movies_test_age$imdb_score, predict(movies_with_age_model_lm, movies_test_age))

movies_with_age_step_rmse
movies_with_age_lm_rmse

## MOVIES WITHOUT AGE ##

movies_without_age_step_rmse <- rmse(movies_test_no_age$imdb_score, predict(movies_no_age_step_lm, movies_test_no_age))
movies_without_age_lm_rmse <- rmse(movies_test_no_age$imdb_score, predict(movies_without_age_model_lm, movies_test_no_age))

movies_without_age_step_rmse
movies_without_age_lm_rmse

## SHOWS WITH AGE ##

shows_with_age_step_rmse <- rmse(shows_test_age$imdb_score, predict(shows_age_step_lm, shows_test_age))
shows_with_age_lm_rmse <- rmse(shows_test_age$imdb_score, predict(shows_with_age_model_lm, shows_test_age))

shows_with_age_step_rmse
shows_with_age_lm_rmse

## SHOWS WITHOUT AGE ##

shows_without_age_step_rmse <- rmse(shows_test_no_age$imdb_score, predict(shows_no_age_step_lm, shows_test_no_age))
shows_without_age_lm_rmse <- rmse(shows_test_no_age$imdb_score, predict(shows_without_age_model_lm, shows_test_no_age))

shows_without_age_step_rmse
shows_without_age_lm_rmse


### PLOTTING ###

## Step regression models

par(mfrow = c(2, 2))

# Movies with age
plot(predict(movies_age_step_lm, movies_test_age), movies_test_age$imdb_score)
abline(a = 0, b = 1, lwd=2, col = "green")

# Movies without age
plot(predict(movies_no_age_step_lm, movies_test_no_age), movies_test_no_age$imdb_score)
abline(a = 0, b = 1, lwd=2, col = "green")

# Shows with age
plot(predict(shows_age_step_lm, shows_test_age), shows_test_age$imdb_score)
abline(a = 0, b = 1, lwd=2, col = "green")

# Shows without age
plot(predict(shows_no_age_step_lm, shows_test_no_age), shows_test_no_age$imdb_score)
abline(a = 0, b = 1, lwd=2, col = "green")

## lm regression models

par(mfrow = c(2, 2))

# Movies with age
plot(predict(movies_with_age_model_lm, movies_test_age), movies_test_age$imdb_score)
abline(a = 0, b = 1, lwd=2, col = "green")

# Movies without age
plot(predict(movies_without_age_model_lm, movies_test_no_age), movies_test_no_age$imdb_score)
abline(a = 0, b = 1, lwd=2, col = "green")

# Shows with age
plot(predict(shows_with_age_model_lm, shows_test_age), shows_test_age$imdb_score)
abline(a = 0, b = 1, lwd=2, col = "green")

# Shows without age
plot(predict(shows_without_age_model_lm, shows_test_no_age), shows_test_no_age$imdb_score)
abline(a = 0, b = 1, lwd=2, col = "green")



unique(movies_training_no_age$production_continent)

# Helpful functions
unique(titles$production_continent)
table(titles$genres)
sum(is.na(movies_testLabels))
