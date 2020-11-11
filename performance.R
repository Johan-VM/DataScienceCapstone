################################################################################
# Johan Vásquez Mazo
# Universidad Nacional de Colombia - Sede Medellín

library(quanteda)
library(dplyr)
library(tidyr)
library(data.table)

load("corpora.RData")

#####
# Convert the test set to tokens
Testing <- tokens(Test) %>% tokens_tolower() %>% tokens_remove(profanity) %>%
    tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE)
rm(Train, Dev)

# Create the sets of 3-grams and 4-grams from the test tokens, and remove the last 3-gram of each 3-grams element
Test3grams <- tokens_ngrams(Testing, n = 3L, concatenator = " ") %>% 
    tokens_remove(pattern = ".", valuetype = "regex", startpos = -1L, endpos = -1L)
Test4grams <- tokens_ngrams(Testing, n = 4L, concatenator = " ")
rm(Test, Testing)

# Unlist the sets of 3-grams and 4-grams
unlisted3 <- unlist(Test3grams)
unlisted4 <- unlist(Test4grams)

# Select a number of 3-grams to predict with (1% of total)
set.seed(23)
indices <- sample(1:length(unlisted3), size = length(unlisted3)*0.01)
unlisted3 <- unlisted3[indices]
unlisted4 <- unlisted4[indices]

# Get the last word of each 4-gram
getLast <- function(n3grams, n4grams) {
    last <- c()
    for (i in 1:length(n3grams)) {
        last[i] <- gsub(paste0(n3grams[i], " "), "", n4grams[i])
    }
    last
}

# Predict the next word for every 3-gram
predicted <- sapply(unlisted3, function(x) {predict(x, mainModel)$ngram}) %>% t()
observed <- getLast(unlisted3, unlisted4)

# Construct a data frame to compare the observed next word against the predicted next words
results <- data.frame("real" = observed, first = predicted[ , 1], second = predicted[ , 2], 
    third = predicted[ , 3], fourth = predicted[ , 4])

# Compute accuracy on first prediction
onFirst <- (results$real == results$first)
accuracyOnFirst <- mean(onFirst)*100

# Compute accuracy on four predictions
onFour <- ((results$real == results$first) | (results$real == results$second) | 
    (results$real == results$third) | (results$real == results$fourth))
accuracyOnFour <- mean(onFour)*100
