# INPUTS
# Takes in csv file (a string),
# the column containing the text to be processed (a string),
# the column containing the classifier/dependent variable (a string), and
# minimum percentage appearance (a number between 0 and 0.001 for removing sparse terms, else default value of 0 is set)

# OUTPUT
# The processed data frame that can be split.

# Why I used [[]] instead of $:
# https://stackoverflow.com/questions/2641653/pass-a-data-frame-column-name-to-a-function

prepare_data <- function(csv_file, text_column_name, dependent_variable, min_percentage_appearance = 0) {
  
  data <- read.csv(csv_file, stringsAsFactors = FALSE) # Load the data
  if(!require(tm)){ # install or load the package
    install.packages("tm")
    library(tm)
  }
  # if(!require(dplyr)){ # install or load the package
  #   install.packages("dplyr")
  #   library(dplyr)
  # }
  corpus <- Corpus(VectorSource(data[[text_column_name]])) # Build a new corpus object called 'corpus'
  corpus <- tm_map(corpus, content_transformer(tolower)) # Convert text to lower case
  corpus <- tm_map(corpus, removePunctuation) # Remove all punctuation
  corpus <- tm_map(corpus, removeWords, stopwords("english")) # Remove all English stop words from the corpus. Can modify to remove additional words.
  corpus <- tm_map(corpus, stemDocument) # Stem the words in the corpus
  dtm <- DocumentTermMatrix(corpus) # Build the document term matrix (dtm) from the corpus
  if (min_percentage_appearance != 0) {
    dtm <- removeSparseTerms(dtm, 1 - min_percentage_appearance) # Remove highly infrequent terms. Setting a sparsity of 0.95 cause the number of terms to fall from 19248 to 8.
  }
  processed_data <- as.data.frame(as.matrix(dtm)) # Build a data frame from the dtm
  colnames(processed_data) <- make.names(colnames(processed_data)) # Make the variable names of the dtm data frame valid
  processed_data[[dependent_variable]] <- data[[dependent_variable]]
  processed_data[[dependent_variable]] <- as.factor(processed_data[[dependent_variable]]) # Convert the dependent variable to a factor
   
  processed_data # return the processed data frame
  
}

testing <- prepare_data('train.csv', 'tweet', 'sentiment', 0.001)

########## Debugging ##########

train <- read.csv("train.csv", stringsAsFactors = FALSE) # Load the data
if(!require(tm)){ # install or load the package
  install.packages("tm")
  library(tm)
}
corpus2 <- Corpus(VectorSource(train$tweet)) # Build a new corpus2 object called 'corpus2'
corpus2 <- tm_map(corpus2, content_transformer(tolower)) # Convert text to lower case
corpus2 <- tm_map(corpus2, removePunctuation) # Remove all punctuation
corpus2 <- tm_map(corpus2, removeWords, stopwords("english")) # Remove all English stop words from the corpus2. Can modify to remove additional words.
corpus2 <- tm_map(corpus2, stemDocument) # Stem the words in the corpus2
dtm2 <- DocumentTermMatrix(corpus2) # Build the document term matrix (dtm) from the corpus2
sparse_dtm2 <- removeSparseTerms(dtm2, 0.999) # Remove highly infrequent terms. Setting a sparsity of 0.95 cause the number of terms to fall from 19248 to 8.
processed_data <- as.data.frame(as.matrix(sparse_dtm2)) # Build a data frame from the dtm
colnames(processed_data) <- make.names(colnames(processed_data)) # Make the variable names of the dtm data frame valid
processed_data$sentiment <- train$sentiment # Add the dependent variable back to the dtm data frame
processed_data$sentiment <- as.factor(processed_data$sentiment) # Convert the dependent variable to a factor

View(processed_data) # return the processed data frame