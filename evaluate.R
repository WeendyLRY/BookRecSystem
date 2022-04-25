# EVALUATING THE RECOMMENDER MODEL
# splitting data

# before splittint data need to do prep
percentage_training <- 0.8
min(rowCounts(ratings_books))
new_thing <- ratings_books[rowCounts(ratings_books) > 5]
min(rowCounts(new_thing))
items_to_keep <- 2

rating_threshold <- 3

n_eval <- 1

# ready to split data
# using evaluationScheme

# build eval_sets 

eval_sets <- evaluationScheme(data = ratings_books, method = "split", train = percentage_training, given = items_to_keep, goodRating = rating_threshold, k = n_eval) 
eval_sets

getData(eval_sets, "train")
nrow(getData(eval_sets, "train")) / nrow(ratings_books)

getData(eval_sets, "known")
getData(eval_sets, "unknown")
nrow(getData(eval_sets, "known")) / nrow(ratings_books)

unique(rowCounts(getData(eval_sets, "known")))

qplot(rowCounts(getData(eval_sets, "unknown"))) + geom_histogram(binwidth = 10) + ggtitle("unknown items by the users")

# using bootstrap method
eval_sets <- evaluationScheme(data = ratings_books, method = "bootstrap", 
                              train = percentage_training, 
                              given = items_to_keep, goodRating = rating_threshold, 
                              k = n_eval)
perc_test <- nrow(getData(eval_sets, "known"))
length(unique(eval_sets@runsTrain[[1]]))

# to see the percentage of unique users in the trainning set, which should be complementary to the percentage of users in the test set
perc_train <- length(unique(eval_sets@runsTrain[[1]])) / nrow(ratings_books)
perc_train + perc_test

# to count how many items each user is repeated in the trainning set
table_train <- table(eval_sets@runsTrain[[1]])
n_repetitions <- factor(as.vector(table_train))
qplot(n_repetitions) + ggtitle("Number of repetitions in the training set")

# using k-fold to validate models

n_fold <- 4
eval_sets <- evaluationScheme(data = new_thing, 
                              method = "cross-validation", 
                              k = n_fold,       
                              given = items_to_keep, 
                              goodRating = rating_threshold)

size_sets <- sapply(eval_sets@runsTrain, length)
size_sets

# define the function arguments of evaluateModel


# EVALUATE MODELS
model_to_evaluate <- "IBCF"

# because we will use default parameters
model_parameters <- NULL

# to build the model
eval_recommender <- Recommender(data = getData(eval_sets, "train"), 
method = model_to_evaluate, parameter = model_parameters)

# specify how many items to recommend.
items_to_recommend <- 10

# build the matrix with the predicted ratings
eval_prediction <- predict(object = eval_recommender, 
                           newdata = getData(eval_sets, "known"), 
                           n = items_to_recommend, 
                           type = "ratings") 
class(eval_prediction)


# to visualize the distribution of the number of movies per user
qplot(rowCounts(eval_prediction)) + geom_histogram(binwidth = 60) + ggtitle("Distribution of books per user")

# to compute the RMSE, MSE and MAE measures about each user by specifying byUser = TRUE
eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, data = getData(eval_sets, "unknown"), byUser = TRUE)
head(eval_accuracy)

# to visualize the RMSE of each user
qplot(eval_accuracy[, "RMSE"]) + geom_histogram(binwidth = 0.1) + ggtitle("Distribution of the RMSE by user")


# to evaluate the performance index of the whole model
# by computing the average indices
eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, data = getData(eval_sets, "unknown"), byUser = FALSE) 
eval_accuracy

# EVALUATING THE RECOMMENDATIONS

# using prebuilt evaluate functions
results <- evaluate(x = eval_sets, method = model_to_evaluate, n = seq(10, 100, 10))
class(results)

# use getConfusionMatrix to extract a list of confusion matrices. 
# each element in the list corresponds to a different split of the k-fold
# to take a look at the first element
head(getConfusionMatrix(results)[[3]])

# to take into account of all splits at the same time by summing up the indices

columns_to_sum <- c("TP", "FP", "FN", "TN") 
indices_summed <- Reduce("+", getConfusionMatrix(results))[, columns_to_sum] 
head(indices_summed)

# to build a chart with the ROC curve
plot(results, annotate = TRUE, main = "ROC curve")

# add the annotate = TRUE input to visualize the labels

# to build a chart with the Precision-recall
plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")



                              


# COMPARING MODELS

# define the models
list(name = "IBCF", param = list(k = 20))

# to define the points of IBCF cosine and IBCF Pearson
models_to_evaluate <- list(IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
                           IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
                           random = list(name = "RANDOM", param=NULL))

# to test the models by varying the number of items
n_recommendations <- c(1, 5, seq(10, 100, 10))

# to run and evaluate the models
# list_result object is an evaluationResultList object and it can be treated as a list. 
list_results <- evaluate(x = eval_sets, method = recc_model, n = n_recommendations)
class(list_results)

sapply(list_results, class) == "evaluationResults"

avg_matrices <- lapply(list_results, avg)

head(avg_matrices$IBCF_cos[, 5:8])

# CHOOSING THE MOST SUITABLE MODEL
plot(list_results, annotate = 1, legend = "topleft") title("ROC 
curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") 
title("Precision-recall")


# OPTIMIZING A NUMERIC PARAMETER
vector_k <- c(5, 10, 20, 30, 40)

models_to_evaluate <- lapply(vector_k, function(k){
  list(name = "IBCF", param = list(method = "cosine", k = k))
})
names(models_to_evaluate) <- paste0("IBCF_k_", vector_k)

n_recommendations <- c(1, 5, seq(10, 100, 10))
list_results <- evaluate(x = eval_sets, method = models_to_evaluate, n 
                         = n_recommendations)

plot(list_results, annotate = 1, legend = "topleft") title("ROC 
curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")