# Packages 
library("tidyverse")
library(skimr)
library(tm)
library(SnowballC)
library(caret)
library(rpart)
library(rpart.plot)
library(ggcorrplot)
library(wordcloud2)

# load data, remove artwork that doesn't have a date
artwork = read.csv("artwork_data.csv")

# remove artwork with no year or < 1600 because not enough data for training and testing
artwork = filter(artwork, year != "")
artwork = filter(artwork, medium != "")
artwork$year = strtoi(artwork$year)
artwork = filter(artwork, !is.na(year))
artwork = filter(artwork, year >= 1600)

# Exploratory Analysis / histograms
hist(artwork$year)
hist(artwork$year_10_bucket)
hist(artwork$year_25_bucket)
hist(artwork$year_50_bucket)
hist(artwork$year_100_bucket)
summarize(group_by(artwork, year_100_bucket),
          n()/ nrow(artwork))

# create "buckets" for century, half centuries, decades
artwork$year_100_bucket = (artwork$year - (artwork$year %% 100) - 1600) / 100
artwork$year_50_bucket = (artwork$year - (artwork$year %% 50) - 1600) / 50
artwork$year_25_bucket = (artwork$year - (artwork$year %% 25) - 1600) / 25
artwork$year_10_bucket = (artwork$year - (artwork$year %% 10) - 1600) / 10

# create corpus for medium
corpus = Corpus(VectorSource(artwork$medium))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
documentMatrix = removeSparseTerms(DocumentTermMatrix(corpus), 0.99)

# create corpus for medium + title
corpus2 = Corpus(VectorSource(paste(artwork$medium, artwork$title)))
corpus2 = tm_map(corpus2, tolower)
corpus2 = tm_map(corpus2, removePunctuation)
corpus2 = tm_map(corpus2, removeWords, stopwords("english"))
corpus2 = tm_map(corpus2, stemDocument)
documentMatrix2 = removeSparseTerms(DocumentTermMatrix(corpus2), 0.99)

calculate_accuracy <- function(documentMatrix, data, yvar) {
  documentterms = as.data.frame(as.matrix(documentMatrix))
  documentterms[,yvar] = factor(data[,yvar], ordered = TRUE , levels = 0:max(data[,yvar]))
  
  set.seed(871)
  idx = createDataPartition(data$year_10_bucket, p = 0.70, list = FALSE)
  train = documentterms[idx ,]
  test = documentterms[-idx ,]
  
  # Cross-validation for CART
  set.seed(871)
  cv.trees = train(y = train[,yvar],
                   x = train[, !(colnames(train) %in% c(yvar))],
                   method = "rpart", 
                   trControl = trainControl(method = "cv", number = 10),
                   tuneGrid = expand.grid(cp = seq(0.001,0.01,.0005)) )
  finalTree = cv.trees$finalModel
  
  print(yvar)
  # accuracy
  predictions.cart.train <- predict(finalTree, newdata=train, type="class")
  matrix.cart.train = confusionMatrix(predictions.cart.train, train[,yvar]) # confusion matrix
  print("training set:")
  print(matrix.cart.train$overall)
  
  predictions.cart.test <- predict(finalTree, newdata=test, type="class")
  matrix.cart.test = confusionMatrix(predictions.cart.test, test[,yvar]) 
  print("test set:")
  print(matrix.cart.test$overall)
}

calculate_accuracy(documentMatrix, artwork, "year_100_bucket")
calculate_accuracy(documentMatrix, artwork, "year_50_bucket")
calculate_accuracy(documentMatrix, artwork, "year_25_bucket")
calculate_accuracy(documentMatrix, artwork, "year_10_bucket")

calculate_accuracy(documentMatrix2, artwork, "year_100_bucket")
calculate_accuracy(documentMatrix2, artwork, "year_50_bucket")
calculate_accuracy(documentMatrix2, artwork, "year_25_bucket")
calculate_accuracy(documentMatrix2, artwork, "year_10_bucket")

# word clouds
matrix_pos = as.matrix(
  TermDocumentMatrix(corpus2[artwork$year_100_bucket==2])
)
df_pos = data.frame(word = row.names(matrix_pos), freq=rowSums(matrix_pos))
wordcloud2(df_pos[order(df_pos$freq, decreasing=TRUE),])

## final cart model for year_50_bucket
yvar = "year_50_bucket"
documentMatrix = documentMatrix
data = artwork
documentterms = as.data.frame(as.matrix(documentMatrix))
documentterms[,yvar] = factor(data[,yvar], ordered = TRUE , levels = 0:max(data[,yvar]))
set.seed(871)
idx = createDataPartition(data$year_10_bucket, p = 0.70, list = FALSE)
train = documentterms[idx ,]
test = documentterms[-idx ,]
set.seed(871)
cv.trees = train(y = train[,yvar],
                 x = train[, !(colnames(train) %in% c(yvar))],
                 method = "rpart", 
                 trControl = trainControl(method = "cv", number = 10),
                 tuneGrid = expand.grid(cp = seq(0.001,0.01,.0005)) )
finalTree = cv.trees$finalModel
# get tree
prp(finalTree, digits=4, extra=107, type=2)
# get importance
CART_importance_scores = finalTree$variable.importance
n_variables = 20 # how many variables?
barplot( tail( sort(CART_importance_scores), n_variables ),
         beside = TRUE,
         horiz = TRUE,
         las=1,
         main = paste("CART - top", n_variables, "importance scores"),
         cex.names =.7)


