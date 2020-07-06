## Setting up working directory for the Project

setwd("C:/Users/patel/Desktop/Recommendation System")

## Install Packages recommenderlab
## Imortant Library packages for the project

library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

##Importing Datasets
movies <- read_csv("IMDB-Dataset/movies.csv")
ratings <- read_csv("IMDB-Dataset/ratings.csv")

##Overview of the Movies Dataset
summary(movies)
head(movies)

##Overview of the Ratings Dataset
summary(ratings)
head(ratings)

##Data Processing

movies_genre <- as.data.frame(movies$genres, stringsAsFactors = FALSE)

movies_genre_2 <- as.data.frame(tstrsplit(movies_genre[,1],'[|]',
                                          type.convert = TRUE), stringsAsFactors = FALSE) #Data Flair
colnames(movies_genre_2) <- c(1:10)

list_genre <- c("Acion", "Advanture", "Animation", "Children","Comedy", "Crime", "Documentary",
                "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_mat_1 <- matrix(0,10330,18)
genre_mat_1[1,] <- list_genre
colnames(genre_mat_1) <- list_genre

for(index in 1:nrow(movies_genre_2)) {
  for (col in 1:ncol(movies_genre_2)){
    gen_col = which(genre_mat_1[1,] == 
      movies_genre_2[index, col]) ## Author Data Flair
    genre_mat_1[index+1, gen_col] <- 1
  }
}

genre_mat_2 <- as.data.frame(genre_mat_1[-1,],
                             stringsAsFactors = FALSE) ##remove first row which was the grnre list

for(col in 1:ncol(genre_mat_2)){
  genre_mat_2[, col] <- as.integer(genre_mat_2[,col]) #convert from Character to Integers 
  
}

str(genre_mat_2)

SearchMatrix <- cbind(movies[,1:2], genre_mat_2[])
head(SearchMatrix) #Data Flair

ratingMatrix <- dcast(ratings, userId~movieId, value.var = "rating", na.rm = FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #Remove USerIds

# Convert Rating Matrix into a recommenderlab spare matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix") 
ratingMatrix

#Recommandation Model

recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
lapply(recommendation_model, "[[", "description") ## Description of the Recommendation Models
recommendation_model$IBCF_realRatingMatrix$parameters ##IBCF-Item Based Collaborative Filtering model

##Determine Similarities between Users
similarity_mat <- similarity(ratingMatrix[1:4], method = "cosine", which = "users")
as.matrix(similarity_mat) #Matrix represents similarities between users
image(as.matrix(similarity_mat), main = "User's Similarity")

##Determine Similarities between Movies

movie_similarity <- similarity(ratingMatrix[,1:4], method = "cosine", which = "items")
as.matrix(movie_similarity) #Matrix represents similarities between Movies
image(as.matrix(movie_similarity), main = "Movies Similarity")

#Extract Unique Rtings of Movies
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values)

#Creating The Count of Movie Ratings
Table_of_Ratings <- table(rating_values) 
Table_of_Ratings

## Most Viewed Movies Visualization
movie_views <- colCounts(ratingMatrix) #count views for each Movie
table_views <- data.frame(movie = names(movie_views),views = movie_views) #Create Data Frame of the View
table_views<- table_views[order(table_views$views, decreasing = TRUE), ] #Sort by Number of Views
table_views$title <- NA
for(index in 1:10325){
  table_views[index,3] <- as.character(subset(movies,movies$movieId == table_views[index,1])$title)
}
table_views[1:6,]

## Making Bar Plot for the table_views 
ggplot(table_views[1:6,], aes(x= title, y = views)) +
geom_bar(stat = "identity", fill = "steelblue") +
geom_text(aes(label = views), vjust = -0.3, size = 3.5) +
theme(axis.title.x = element_text(angle = 45, hjust = 1)) +

ggtitle("Total Views of the Top Films")


## Making Heat Map for the Movie Ratings

image(ratingMatrix[1:20, 1:25], axes = FALSE, main = "Heatmap of the First 20 rows and 25 columns")


##Performing Data Preparation 

##set Minimum number of threshold for users who have rated a film as 50 
## Set Minimum number of threshold for views per film
movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50, colCounts(ratingMatrix) > 50]
movie_ratings

minimum_movies <- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)

image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users], 
      main = "Heat Map of Top Users and Movies")

## Visualize distribution of Average rating per user

average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill = I("steelblue"), col= I("red")) +
  ggtitle("Distribution of the Average Rating per User")

## Data Normalization

normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)

image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                         colCounts(normalized_ratings) > minimum_users],
      main = " Normalized Ratings of the Top Users")

## Performing Data Binarization

binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)

good_rated_films <- binarize(movie_ratings, minRating = 3)
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
                       colCounts(movie_ratings) > binary_minimum_users], 
      main = "Heatmap of the users and movies")

## Split Data into traning and Testing Dataset

sampled_data <- sample(x= c(TRUE, FALSE), size = nrow(movie_ratings),
                    replace = TRUE, prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

##Building the Recommendation System 

recommendation_system <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

recommon_model <- Recommender(data = training_data, method = "IBCF",
                              parameter = list(k= 30))
recommon_model
class(recommon_model)

model_info <- getModel(recommon_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items], 
      main = "Heatmap of the First rows and columns")

sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)

sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill = I("steelblue"), col = I("red"))+
  ggtitle("Distribution of the column count")

## Predicting the movies recommendation
top_recommendations <- 10 ## the number of items to recommend to each user
predicted_recommendations <- predict(object = recommon_model, newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations

user_1 <- predicted_recommendations@items[[1]] #recommendation to the first user
movies_user_1 <- predicted_recommendations@itemLabels[user_1]
movies_user_2 <- movies_user_1

for(index in 1:10){
  movies_user_2[index] <- as.character(subset(movies, 
                                              movies$movieId == movies_user_1[index])$title)
}
movies_user_2

recommendation_matrix <- sapply(predicted_recommendations@items, function(x){
  as.integer(colnames(movie_ratings)[x])
}) ## matrix with the recommendations for each user

recommendation_matrix[,1:4]

