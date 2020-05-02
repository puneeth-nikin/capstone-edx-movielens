################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
#additional
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

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

library(lubridate)
library(caret)
library(tidyverse)
library(data.table)

#RMSE function
RMSE<-function(predicted_ratings,actual_ratings){
  sqrt(mean((predicted_ratings-actual_ratings)^2))
}


# extract year released from  title
pattern_year_released<-"(\\d{4})\\)$"
edx<-edx%>%mutate(year_released= as.numeric(str_match(title,pattern_year_released)[,2]))
#convert timestamp to readable datetime and extract year rated
edx<-edx%>%mutate(date_time_rated=as.POSIXct(timestamp, origin="1970-01-01"),year_rated=year(date_time_rated))
edx<-edx%>%mutate(movieId=as.factor(movieId),userId=as.factor(userId))

#create a column for each genre and check if movie in genre
genres_list<-unique(unlist(strsplit(as.vector(unique(edx$genres)),'\\|')))
x<-map(genres_list,function(x){
  str_detect(edx$genres,x)
})
View(edx)
# calculating average user rating and average movie rating
avg_user_rating_df<-edx%>%group_by(userId)%>%summarise(avg_user_rating=round(mean(rating),digits=3))
edx<-edx%>%left_join(avg_user_rating_df,by="userId")
avg_movie_rating_df<-edx%>%group_by(movieId)%>%summarise(avg_movie_rating=round(mean(rating),digits=3))
edx<-edx%>%left_join(avg_movie_rating_df,by="movieId")

#As the dataset is very large it take a long time to train and fit models. Hence I take a smaller sample for building the model.
#dim(edx)
#set.seed(1,sample.kind = "Rounding")
#edx_sample<- edx[sample(nrow(edx),5000),]

#partition edx dataset
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index_edx <- createDataPartition(y = edx$rating, times = 1, p = 0.5, list = FALSE)
test_set <- edx[test_index_edx,]
train_set <- edx[-test_index_edx,]
#making sure test set has same movies as train set
test_set<- test_set%>%semi_join(train_set,by='movieId')%>%semi_join(train_set,by='userId')

control<-trainControl(method = "cv", number = 10, p = .9)

fit<- train(rating~avg_movie_rating+avg_user_rating+year_released+year_rated,data = train_set,method='lm',trControl=control)

#y_hat<-predict(fit,test_set_sample)

#RMSE(y_hat,test_set_sample$rating)
