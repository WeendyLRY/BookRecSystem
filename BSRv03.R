#load library
library(dplyr)
library("recommenderlab") #builds recommendation engine
library(lsa)
library(SnowballC)
library("data.table") # manipulates the data
library("ggplot2")
library(tidyr) # to get the pivot_wider function
library(grid)
library(jpeg)


# DATA IMPORT 
# load books, users, ratings data
table_books <- read.csv("C:\\Users\\User\\Desktop\\PROJECT\\TESTING\\BSR\\Books.csv")
table_users <- read.csv("C:\\Users\\User\\Desktop\\PROJECT\\TESTING\\BSR\\Users.csv")
table_ratings <- read.csv("C:\\Users\\User\\Desktop\\PROJECT\\TESTING\\BSR\\Ratings.csv")

# rename columns
setnames (table_books, 1:8, c("isbn", "title", "author", "publication_year", "publisher", "small_url", "medium_url", "large_url"))

# rename isbn column 
table_books$isbn = paste0("isbn_",table_books$isbn)

head(table_books)
head(table_users)
head(table_ratings)

# 1 DATA PREPARATION

#convert every tables into data tables so it can be processed easily
tbl_books <- data.table(table_books)
tbl_users <- data.table(table_users)
tbl_ratings <- data.table(table_ratings)

# Attribute (A) is rating
# Case (C) for each user, containing User_ID
# Vote (V) is book iSBn

# rename columns
setnames (tbl_books, 1:5, c("isbn", "title", "author", "publication_year", "publisher"))
setnames (tbl_users, 1:3, c("user_id", "location", "age"))
setnames (tbl_ratings, 1:3, c("user_id", "isbn", "rating"))

# ADDING additional strings to all the characters in ISBN and User-ID
tbl_users$user_id = paste0("user_",tbl_users$user_id)
tbl_ratings$isbn = paste0("isbn_",tbl_ratings$isbn)
tbl_ratings$user_id = paste0("user_",tbl_ratings$user_id)

# remove unused columns
tbl_books_new = subset(tbl_books, select = -c(small_url, medium_url, large_url) )

# RECOMMENDATION BASED ON CORRELATIONS
# using Pearson'R correlation coefficient to measure the linear correlation between two variables, or the ratings of two books
# merge complete_top_6_rated_books to get the ratings


# DATA CLEANSING
# evaluate ratings of each user

tbl_ratings %>%
  group_by(rating) %>%
  summarize(cases = n()) %>%
  ggplot(aes(rating, cases)) + geom_col() +
  theme_minimal() + scale_x_continuous(breaks = 0:10)

tbl_ratings = tbl_ratings[tbl_ratings$rating != 0,] #remove 0's

# to see the frequency of how many times the each users are rating
ratings_sum = tbl_ratings %>%
  group_by(user_id) %>%
  count() 

# to see the frequency of how many times each books are rated
ratings_sum2 = tbl_ratings %>%
  group_by(isbn) %>%
  count() 

# sort the users and books according to their frequency
ratings_sum2 <- ratings_sum2 %>% arrange(desc(n))
ratings_sum <- ratings_sum %>% arrange(desc(n))

summary(ratings_sum$n)
summary(ratings_sum2$n)

# remove the portion of users who have given less than 4 ratings

# remove the portion of users who have given more than 4600 ratings
user_index = ratings_sum$user_id[ratings_sum$n<4600 && ratings_sum$n>3]

# update all the tables after ratings_sum is modified
tbl_users = tbl_users[tbl_users$user_id %in% user_index, ]
tbl_ratings = tbl_ratings[tbl_ratings$user_id %in% user_index, ]
tbl_books_new = tbl_books_new[tbl_books_new$isbn %in% tbl_ratings$isbn,]

# remove data to save space
rm(ratings_sum, user_index)

rm(table_users, table_ratings, table_books)

rm(ratings_sum)

# convert the tbl_rating dataframe into a realratingmatrix object
temp <- as(tbl_ratings,"realRatingMatrix")
class(temp)

# EXPLORING THE AVERAGE RATINGS
# find the average book rating
average_ratings <- colMeans(temp)
qplot(average_ratings) + stat_bin(binwidth = 0.1) + ggtitle ("Distribution of the average book rating")

# find the average book rating amongst relevant dataset only
# kenapa ada beberapa buku yang menerima ratings below 5 stars adalah karna 
# terlalu kurang user merate buku tersebut
# but we cant simply eliminate them
reads_per_book <- colCounts(temp)
average_ratings_relevant <- average_ratings[reads_per_book > 10]
qplot(average_ratings_relevant) + stat_bin(binwidth = 0.1) + ggtitle ("Distribution of the relavent average book rating")

# visualizing the matrix
# building the heatmap with the moost relevant users and items
# which is visualizing, with heat map, the users who have read many books and the books that has been read by many users

#1. determine the min number of books per user
min_n_books <- quantile(rowCounts(temp), 0.999)

#2. determine the min number of users per boooks
min_n_users <- quantile(colCounts(temp), 0.999)

#3. select the users and books matching these criterias 
image(temp[rowCounts(temp) > min_n_books,
                 colCounts(temp) > min_n_users], main = "Heatmap of the top users and books")

# DATA PREPARATION

# select the relevant data 
ratings_books <- temp[rowCounts(temp) > 100,
                             colCounts(temp) > 50]

# exploring the relevant data
# to visualize the top 5 percent of users and books in the new matrix
min_books <- quantile(rowCounts(ratings_books), 0.95)
min_users <- quantile(colCounts(ratings_books), 0.95)

# build the heatmap based on the top 5 percents above
image(ratings_books[rowCounts(ratings_books) > min_books,
                     colCounts(ratings_books) > min_users], main = "Heatmap of the top users and books")

# to get the distribution of the average rating by user
average_ratings_per_user <- rowMeans(ratings_books)

# visualize the distribution
qplot(average_ratings_per_user) + stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average rating per user")

# normalize the data

# to remove the effect of having users who give high or low ratings to all their books which will bias the result

ratings_books_norm <- normalize(ratings_books)

# to evaluate the average rating of users
sum(rowMeans(ratings_books_norm) > 0.00001)

# visualize the normalized matrix
image(ratings_books_norm[rowCounts(ratings_books_norm) > min_books,
                          colCounts(ratings_books_norm) > min_users], main = "Heatmap of the normalized top users and books")

# BINARIZING THE DATA

# binary data based on whether book is rated or not
ratings_books_read <- binarize(ratings_books, minRating = 1)

# get the top 10 %

min_books_binary <- quantile(rowCounts(ratings_books), 0.9)
min_users_binary <- quantile(colCounts(ratings_books), 0.9)

# build a heatmap based on the binarized data
image(ratings_books_watched[rowCounts(ratings_books) > min_books_binary,colCounts(ratings_books) > min_users_binary], main = "Heatmap of the top users and books (Binarized based on rated status)")

# binary data based on whether book is good
ratings_book_good <- binarize(ratings_books, minRating = 5)

# plot a heatmap for the binary data based on whether book is good
image(ratings_book_good[rowCounts(ratings_books) > min_books_binary, colCounts(ratings_books) > min_users_binary], main = "Heatmap of the top users and books")

# defining training and test set

# randomly define the which_train vetor that is TRUE for users in the trainning set and FALSE for the other
# the probability is set as 80 percent
which_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_books),
                      replace = TRUE, prob = c(0.8, 0.2))

# definie the trainning and test sets
recc_data_train <- ratings_books[which_train, ]
recc_data_test <- ratings_books[!which_train, ]

# BUILDING THE RECOMMENDATION MODEL
recc_model <- Recommender(data = recc_data_train, method = "IBCF",
                          parameter = list(k = 30))

# EXPLORING RECOMMENDER MODEL

# extract details about the model (description, parameters)
model_details <- getModel(recc_model)
model_details$description
model_details$k

# check the similarity matrix of model_details$sim
class(model_details$sim)

# explore part of the square matrix of model_details$sim using
# image and building a heat map
n_items_top <- 20

# build heat map for n_items_top
image(model_details$sim[1:n_items_top, 1:n_items_top],
      main = "Heatmap of the first rows and columns")

model_details $ k
row_sums <- rowSums(model_details$sim > 0)
table(row_sums)

# check the distribution of the number of elements by column
col_sums <- colSums(model_details$sim > 0)

# build the distribution chart
qplot(col_sums) + stat_bin(binwidth = 1) + ggtitle("Distribution of the column count")

# to observe which books has the most elements
which_max <- order(col_sums, decreasing = TRUE)[1:6]
rownames(model_details$sim)[which_max]

# generate the titles of the books
isbn <- c("isbn_0671727796", "isbn_0451184963", "isbn_0842329242", "isbn_0743237188", "isbn_0553279378", "isbn_0553296981")

df_temp <- data.frame(isbn)

df_temp2 <- left_join(df_temp, tbl_books)

rm(df_temp, df_temp2)

# APPLYING THE RECOMMENDER MODEL ON THE TEST SET 

# initialize the variable that specifies the number of items to recommend to each user
n_recommended <- 6

# algorithm extract the rated movies for each user
# the algorithm identifies all its similar items for each movie
# then the algorithm ranks each similar item in a sequential way

# then, the algorithm identifies the top n recommendations


recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended)
recc_predicted

# recommendations for the first user:
recc_predicted@items[[1]]

# extracting the recommended books from the recc_predicted@item labels:
recc_user_1 <- recc_predicted@items[[1]]
books_user_1 <- recc_predicted@itemLabels[recc_user_1]
books_user_1

# generate the titles of the books
isbn <- c("isbn_0060929871", "isbn_0060938455", "isbn_014028009X", "isbn_0312983271", "isbn_0345378490", "isbn_0380731851")

df_temp <- data.frame(isbn)

df_temp2 <- left_join(df_temp, tbl_books)

rm(df_temp, df_temp2)

# function(x) --> process(x)

# defining a matrix with the recommendations for each user
recc_matrix <- sapply(recc_predicted@items, {
  colnames(ratings_books)[x]
})

dim(recc_matrix)

# visualize the recommendations for the first four users
recc_matrix[, 1:4]

# since most recommended movies are identified
# define a vector with all the recommendations,
# then build a frequency plot

number_of_items <- factor(table(recc_matrix))
chart_title <- "Distribution of the number of items for IBCF"

# build the distribution chart
qplot(number_of_items) + ggtitle(chart_title)
# most movies have been recommended only a few times. 
# to generate a table to see the most popular movies
number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(names(number_of_items_top),
                        number_of_items_top)
table_top
