# Movies Recommendation system
# Team 8


rating = read.csv("ratings.csv",sep = ",")
movies = read.csv("movies.csv",sep = ",")

View(rating)
View(movies)
#install.packages("recommenderlab")
#install.packages("cluster")
library(recommenderlab)
library(cluster)

#Merge the Datasets "Ratings" and "Movies"
movieratings = merge(movies,rating, by = "movieId")
View(movieratings)
names(movieratings)

#Delete the unwanted columns
movieratings$generes = NULL
movieratings$timestamp = NULL



movieratings$title
#Covert into a DataFrame
ratings = as.data.frame(movieratings$userId)
ratings$title = movieratings$title
ratings$rating = movieratings$rating
View(ratings)

write.csv(ratings,"Movie_Ratings.csv")

ratings = read.csv("Movie_Ratings.csv")
View(ratings)

# Removing the duplicate rating combination of Movies and Users

str(ratings)
ratings = unique(ratings)

# Ratings of the Movies are considered between 1 and 5
# So, we are removing the users who have given no rating for the Movies

ratings = subset(ratings,ratings$rating > 0)

ratings$X = NULL

length(unique(ratings$movieratings.userId))

# Many of the functions in "recommenderlab" package take realRatingMatrix object
# realRatingMatrix is a 2-D matrix with users in rows, Movies in columns and cells are ratings

RatingMatrix = as(ratings,"realRatingMatrix")

RatingMatrix

# Assumptions: Any given user should have seen and rated atleast 10 movies
# So remove the users who have watched and rated less than 10 movies
RatingMatrix = RatingMatrix[rowCounts(RatingMatrix)>10,colCounts(RatingMatrix)>10]

str(RatingMatrix)

RatingMatrix

# Recommender function in recommendationlab takes realRatingMatrix object

RatingMatrix = as(ratings,"realRatingMatrix")
str(RatingMatrix)

# Sampling of data to create training and testing data

indexes = sample(1:nrow(RatingMatrix), size=0.7*nrow(RatingMatrix))
training = RatingMatrix[indexes,]
testing = RatingMatrix[-indexes,]

# This function gives list of users and their rating for the Movies
as(ratings,"list")
head(as(training, "data.frame"))

# Histogram of users and their ratings
# Here histogram gives us the frequency of the given ratings

hist(getRatings(RatingMatrix), breaks = 100,xlim=c(0, 5))

# From the histogram we can say that most of the movies are rated between 3 and 5

# Let us look at the normalized ratings histogram

hist(getRatings(normalize(RatingMatrix)), breaks=100)

# From the normalized rating we can find that there are more zero rating

# Similarity between the users
# Similarity between the users means users who have similar preference of the Movies

users_similarity = similarity(RatingMatrix, method = "euclidean",which = "users")

as.matrix(users_similarity)

# Heirarchical clustering
ratings$title = as.numeric(ratings$title)
distances = dist(ratings[1:20,], method = "euclidean")
clustermovies = hclust(distances, method = "ward.D")

plot(clustermovies)

# K-means clustering used in User Base Collaborative Clustering

kmeanscluster = kmeans(ratings,centers = 2)
clusplot(ratings,kmeanscluster$cluster, color = TRUE)

# RecommenderRegistry stores different types of recommendation methods
recommenderRegistry$get_entries(dataType = "realRatingMatrix")

# Building the recommender algorithm for popular Movies

recom_popular = Recommender(RatingMatrix[1:200], method = "POPULAR")

names(getModel(recom_popular))

# Get the Top - N list of Movies
# Here top 15 Movies are recommended for user 202
# Different Movies are recommended for different users

predictrating  = predict(recom_popular,RatingMatrix[202],n=15)

as(predictrating,"list")

# Predict the ratings using predict() function

ratingrecommendation = predict(recom_popular, RatingMatrix, type="ratings")
as(ratingrecommendation,"list")

# User based Collaborative filtering method

recom_ubcf = Recommender(RatingMatrix[1:200], method = "UBCF")


# getModel() function gives the details of the recommendation model

modeldetails = getModel(recom_ubcf)

names(modeldetails)

modeldetails$data

# Applying on testing data

n_users = 6

testing_ubcf = predict(object = recom_ubcf,newdata = RatingMatrix[201:400],n = n_users)

recc_matrix = sapply(testing_ubcf@items, function(x)
{
  colnames(RatingMatrix)[x]
})

recc_matrix

#Output File with recommendations

TestFile <-write.csv(recc_matrix,"testfile.csv")

# Recommendation based on Singular Value Decomposition

recom_svd = Recommender(RatingMatrix[1:200], method = "SVD")

modeldetails = getModel(recom_svd)

names(modeldetails)

modeldetails$data

n_users = 6

testing_svd = predict(object = recom_svd,newdata = RatingMatrix[201:400])

recc_matrix = lapply(testing_svd@items, function(x)
{
  colnames(RatingMatrix)[x]
})

recc_matrix
TestFile1 <-write.csv(recc_matrix,"testfile2.csv")



# Evaluation of Predicted Ratings

# Split the data into training/testing in the ratio of 70/30

tdata = evaluationScheme(RatingMatrix, method="split", train=0.7 ,given=2, goodRating=5)

result1 = Recommender(getData(tdata, "train"), "UBCF")
result1

prediction_1 = predict(result1, getData(tdata,"known"), type = "ratings")
prediction_1

# Error values gives Root Mean Square Error, Mean Square Error and Mean Absolute Error of the predicted ratings
errorvalues = calcPredictionAccuracy(prediction_1, getData(tdata, "unknown"))
errorvalues
