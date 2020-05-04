################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# including libraries and installing additional libraries
library(caret)
library(tidyverse)
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(lubridate)
#Exploring and Preparing the Dataset

dim(edx)
names(edx)
edx%>%head()

# extracting the year released from the title column
regex_pattern<-"(\\d{4})\\)$"
edx<-edx%>%
  mutate(year_released=year(as.Date(str_match(title,regex_pattern)[,2],format="%Y")))

# converting the timestamp column to readable date
edx <- edx%>%
  mutate(datetime=as_datetime(timestamp))

#exploring relation between ratings and the year movie was released
edx%>%
  group_by(year_released)%>%
  summarise(avg_ratings=mean(rating))%>%
  ggplot(aes(year_released,avg_ratings))+
  geom_point()+geom_smooth(method = "loess")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  ggtitle("Average ratings vs Year Released")
edx%>%
  group_by(year_released)%>%
  summarise(avg_ratings=mean(rating))%>%
  cor()
edx%>%
  group_by(year_released)%>%
  summarise(n_ratings=n())%>%
  ggplot(aes(year_released,n_ratings))+
  geom_point()+
  geom_smooth(method = "loess")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  ggtitle("Number of Ratings vs Year Released ")

#exploring the relationship between ratings and when it was rated
edx%>%
  mutate(week_rated=round_date(datetime,unit = 'week'))%>%
  group_by(week_rated)%>%summarise(avg_rating=mean(rating))%>%
  ggplot(aes(week_rated,avg_rating))+
  geom_point()+
  geom_smooth(method="loess")+
  ggtitle("Average Rating vs Timestamp of Rating")
edx%>%
  mutate(week_rated=round_date(datetime,unit = 'week'))%>%
  group_by(week_rated)%>%summarise(n_rating=n())%>%
  ggplot(aes(week_rated,n_rating))+
  geom_point()+
  geom_smooth(method="loess")+
  ggtitle("Number of Ratings vs Timestamp of Rating")

#Exploring the relationship between ratings and different genres.
unique(unlist(str_split(as.vector(unique(edx$genres)),"\\|")))
length(unique(edx$genres))
edx%>%
  group_by(genres)%>%
  summarise(avg_ratings=mean(rating))%>%
  top_n(10,avg_ratings)
edx%>%
  group_by(genres)%>%
  summarise(avg_ratings=mean(rating),n_ratings=n())%>%
  top_n(10,n())
edx%>%
  group_by(genres)%>%
  summarise(avg_rating=mean(rating),
            se_rating=sd(rating)/sqrt(n()),
            n_ratings=n())%>%top_n(15,n_ratings)%>%
  mutate(genres=reorder(genres,avg_rating))%>%
  ggplot(aes(x=genres,
             y=avg_rating,
             ymin=avg_rating-(qnorm(0.975)*se_rating),
             ymax=avg_rating+(qnorm(0.975)*se_rating)))+
  geom_errorbar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#RMSE
RMSE <-  function(predicted_ratings,actual_ratings){
  sqrt(mean((predicted_ratings-actual_ratings)^2))
}
#results table
results<-tibble()
#split edx to train and test set
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index<-createDataPartition(edx$rating,
                                times=1,p=0.5,list=FALSE)
test_set<-edx[test_index,]
train_set<-edx[-test_index,]
#ensuring the test set has same userId and movieId
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>% 
  semi_join(train_set, by = "userId")

#simple model
mu<-mean(train_set$rating)
result_simple_model<-RMSE(mu,test_set$rating)
results<-data.frame(model='mean only model',
                    result=result_simple_model)
results<-data.frame(model='Mean Only',
                    result=result_simple_model)

#Movie effect
avg_ratings_movie<- train_set%>%
  group_by(movieId)%>%
  summarise(b_i=mean(rating)-mu)
predictions_movie_effect <- mu + test_set%>%
  left_join(avg_ratings_movie,by='movieId')%>% 
  pull(b_i)
result_movie_effect<-RMSE(predictions_movie_effect,
                          test_set$rating)
results<-rbind(results,data.frame(model='Movie Effect',
                                  result=result_movie_effect))

#Movie effect + User effect model
avg_ratings_user  <- train_set%>%
  left_join(avg_ratings_movie,by='movieId')%>%
  group_by(userId)%>%summarise(b_u= mean(rating-mu-b_i))
predictions_movie_user_effect <- test_set%>%
  left_join(avg_ratings_movie,by='movieId')%>%
  left_join(avg_ratings_user,by='userId')%>%
  mutate(pred=mu+b_i+b_u)%>%pull(pred)
result_movie_user_effect<-RMSE(predictions_movie_user_effect,
                               test_set$rating)
results<-rbind(results,data.frame(model='Movie+User Effect',
                                  result=result_movie_user_effect))

#Movie+user+genre effect model
avg_ratings_genre<-train_set %>%
  left_join(avg_ratings_movie,by='movieId')%>%
  left_join(avg_ratings_user,by='userId')%>%
  group_by(genres)%>%
  summarise(b_g=mean(rating-mu-b_i-b_u))
predictions_movie_user_genre_effect<- test_set%>%
  left_join(avg_ratings_movie,by='movieId')%>%
  left_join(avg_ratings_user,by='userId')%>%
  left_join(avg_ratings_genre,by='genres')%>%
  mutate(pred=mu+b_i+b_u+b_g)%>%
  pull(pred)
result_movie_user_genre_effect <-RMSE(predictions_movie_user_genre_effect, 
                                      test_set$rating)
results<-rbind(results,data.frame(model='Movie+User+genre Effect',
                                  result=result_movie_user_genre_effect))

#Movie+user+genre+year_released effect model
avg_ratings_year<-train_set%>%
  left_join(avg_ratings_movie,by='movieId')%>%
  left_join(avg_ratings_user,by='userId')%>%
  left_join(avg_ratings_genre,by='genres')%>%
  group_by(year_released)%>%
  summarise(b_t=mean(rating-mu-b_i-b_u-b_g))
predictions_movie_user_genre_year_effect<-test_set%>%
  left_join(avg_ratings_movie,by='movieId')%>%
  left_join(avg_ratings_user,by='userId')%>%
  left_join(avg_ratings_genre,by='genres')%>%
  left_join(avg_ratings_year,by='year_released')%>%
  mutate(pred=mu+b_i+b_u+b_g+b_t)%>%
  pull(pred)
result_movie_user_genre_year_effect<-RMSE(predictions_movie_user_genre_year_effect,
                                          test_set$rating)
results<-rbind(results,
               data.frame(model='Movie+User+genre+year_released Effect',
                          result=result_movie_user_genre_year_effect))

#Regularisation
lambdas <- seq(0, 10, 0.5)
rmses_regularised <- map_dbl(lambdas, function(lambda){
  mu <- mean(train_set$rating)
  avg_ratings_movie<-train_set%>%
    group_by(movieId)%>%summarise(b_i=sum(rating-mu)/(n()+lambda))
  avg_ratings_user<-train_set%>%
    left_join(avg_ratings_movie,by='movieId')%>%
    group_by(userId)%>%
    summarise(b_u= sum(rating-mu-b_i)/(n()+lambda))
  avg_ratings_genre<-train_set %>%
    left_join(avg_ratings_movie,by='movieId')%>%
    left_join(avg_ratings_user,by='userId')%>%
    group_by(genres)%>%
    summarise(b_g=sum(rating-mu-b_i-b_u)/(n()+lambda))
  avg_ratings_year<-train_set%>%
    left_join(avg_ratings_movie,by='movieId')%>%
    left_join(avg_ratings_user,by='userId')%>%
    left_join(avg_ratings_genre,by='genres')%>%
    group_by(year_released)%>%
    summarise(b_t=sum(rating-mu-b_i-b_u-b_g)/(n()+lambda))
  predictions<-test_set%>%
    left_join(avg_ratings_movie,by='movieId')%>%
    left_join(avg_ratings_user,by='userId')%>%
    left_join(avg_ratings_genre,by='genres')%>%
    left_join(avg_ratings_year,by='year_released')%>%
    mutate(pred=mu+b_i+b_u+b_g+b_t)%>%
    pull(pred)
  RMSE(predictions,test_set$rating)
})
#choosing the best lambda
best_lambda<-lambdas[which.min(rmses_regularised)]

# Training on the edx dataset using the best model
mu<-mean(edx$rating)
avg_ratings_movie<-edx%>%
  group_by(movieId)%>%
  summarise(b_i=sum(rating-mu)/(n()+best_lambda))
avg_ratings_user<-edx%>%
  left_join(avg_ratings_movie,by='movieId')%>%
  group_by(userId)%>%
  summarise(b_u= sum(rating-mu-b_i)/(n()+best_lambda))
avg_ratings_genre<-edx %>%
  left_join(avg_ratings_movie,by='movieId')%>%
  left_join(avg_ratings_user,by='userId')%>%
  group_by(genres)%>%
  summarise(b_g=sum(rating-mu-b_i-b_u)/(n()+best_lambda))
avg_ratings_year<-edx%>%
  left_join(avg_ratings_movie,by='movieId')%>%
  left_join(avg_ratings_user,by='userId')%>%
  left_join(avg_ratings_genre,by='genres')%>%
  group_by(year_released)%>%
  summarise(b_t=sum(rating-mu-b_i-b_u-b_g)/(n()+best_lambda))

#predictions on the validation set
validation<-validation%>%
  mutate(year_released=year(as.Date(str_match(title,regex_pattern)[,2],format="%Y")))
predictions<-validation%>%
  left_join(avg_ratings_movie,by='movieId')%>%
  left_join(avg_ratings_user,by='userId')%>%
  left_join(avg_ratings_genre,by='genres')%>%
  left_join(avg_ratings_year,by='year_released')%>%
  mutate(pred=mu+b_i+b_u+b_g+b_t)%>%
  pull(pred)
final_result<-RMSE(predictions,validation$rating)

