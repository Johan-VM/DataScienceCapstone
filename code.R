################################################################################
# Johan Vásquez Mazo
# Universidad Nacional de Colombia - Sede Medellín

library(quanteda)
library(dplyr)
library(tidyr)
library(data.table)

#####
# Read .txt files
Blogs <- readLines("./final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
News <- readLines("./final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
Twitter <- readLines("./final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

# Create corpus objects
Blogs <- corpus(Blogs)
News <- corpus(News)
Twitter <- corpus(Twitter)

#####
# Set sample size to 10% of all data observations
set.seed(1999)
Blogs <- corpus_sample(Blogs, size = length(Blogs)*0.1)
News <- corpus_sample(News, size = length(News)*0.1)
Twitter <- corpus_sample(Twitter, size = length(Twitter)*0.1)

names(Blogs) <- 1:length(Blogs)
names(News) <- (1+length(Blogs)):(length(News)+length(Blogs))
names(Twitter) <- (1+length(Blogs)+length(News)):(length(Twitter)+length(Blogs)+length(News))

# Create one big single corpus object
Corpus <- c(Blogs, News, Twitter)
rm(list = setdiff(ls(), "Corpus"))

#####
# Assign 70% of the sampled data to the training set, 20% to the validation set, and 10% to the test set
set.seed(1404)
index <- sample(1:length(Corpus), size = length(Corpus)*0.7)
index2 <- sample(1:(length(Corpus)-length(index)), size = length(Corpus)*0.2)

Train <- Corpus[index]
Dev <- Corpus[-index][index2]
Test <- Corpus[-index][-index2]
rm(list = setdiff(ls(), c("Train", "Dev", "Test")))

#####
# Convert the corpus object to tokens
profanity <- readLines("https://raw.githubusercontent.com/RobertJGabriel/Google-profanity-words/master/list.txt")
Tokens <- tokens(Train) %>% tokens_tolower() %>% tokens_remove(profanity) %>%
    tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE)

# Add tokens for the beginning of sentence and the end of sentence
Tokens <- as.list(Tokens)
for (i in 1:length(Tokens)) {
    Tokens[[i]] <- c("INI", Tokens[[i]], "END")
}
Tokens <- as.tokens(Tokens)

# Construct the 2-grams, 3-grams and 4-grams objects
Bigrams <- Tokens %>% tokens_ngrams(n = 2L, concatenator = " ")
Trigrams <- Tokens %>% tokens_ngrams(n = 3L, concatenator = " ")
Tetragrams <- Tokens %>% tokens_ngrams(n = 4L, concatenator = " ")

#####
# Create a function to produce a named vector with the n-grams frequencies from tokens
ngrams_frequency <- function(Tokens, threshold = 1) {
    ngrams_FV <- Tokens %>% dfm(tolower = FALSE, verbose = FALSE) %>% featfreq() # or docfreq()
    ngrams_FV <- sort(ngrams_FV, decreasing = TRUE)
    return(ngrams_FV[ngrams_FV > threshold])
}

# Create a function to construct a data table with the n-grams frequencies from tokens
ngrams_table <- function(Tokens, noSequence = TRUE, threshold = 1) {
    ngrams_DT <- ngrams_frequency(Tokens, threshold) %>% data.table(ngram = names(.), freq = .)
    if (noSequence) {
        if (length(grep('^INI', ngrams_DT$ngram)) > 0) {
            ngrams_DT <- ngrams_DT[-grep('^INI', ngrams_DT$ngram), ]
        }
        if (length(grep('END$', ngrams_DT$ngram)) > 0) {
            ngrams_DT <- ngrams_DT[-grep('END$', ngrams_DT$ngram), ]
        }
    }
    return(ngrams_DT)
}

#####
# Obtain data tables of the n-grams frequencies for the training set
n1table <- ngrams_table(Tokens, threshold = 50)
n2table <- ngrams_table(Bigrams, threshold = 10)
n3table <- ngrams_table(Trigrams, threshold = 5)
n4table <- ngrams_table(Tetragrams, threshold = 5)

n2tableSeq <- ngrams_table(Bigrams, noSequence = FALSE, threshold = 10)
n3tableSeq <- ngrams_table(Trigrams, noSequence = FALSE, threshold = 5)
n4tableSeq <- ngrams_table(Tetragrams, noSequence = FALSE, threshold = 5)

#####
# Create a function to construct an n-gram model from a list of data frames of n-grams and frequencies
buildModel <- function(ngrams_list) {
    ngrams_model <- list()
    for (df in ngrams_list) {
        stopifnot(is.data.frame(df))
        if (length(unlist(strsplit(df[1,1], " "))) == 1) {
            ngrams_model$unigram <- df
        }
        if (length(unlist(strsplit(df[1,1], " "))) == 2) {
            ngrams_model$bigram <- df
        }
        if (length(unlist(strsplit(df[1,1], " "))) == 3) {
            ngrams_model$trigram <- df
        }
        if (length(unlist(strsplit(df[1,1], " "))) == 4) {
            ngrams_model$tetragram <- df
        }
    }
    ngrams_model
}

# Create two lists of data frames to build the models
ngrams_list <- list(n1table, n2table, n3table, n4table) %>% lapply(as.data.frame)
ngrams_listSeq <- list(n1table, n2tableSeq, n3tableSeq, n4tableSeq) %>% lapply(as.data.frame)

# Build the models
modelNoSeq <- buildModel(ngrams_list)
mainModel <- buildModel(ngrams_listSeq)

##########
### No back-off

# Create a function to tokenize input text and add INI token
tokenizeText <- function(text, addINI = FALSE) {
    profanity <- readLines("https://raw.githubusercontent.com/RobertJGabriel/Google-profanity-words/master/list.txt")
    tokenized <- text %>% tokens() %>% tokens_tolower() %>% tokens_remove(profanity) %>% 
        tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE)
    if (addINI) {
        tokenized <- as.list(tokenized)
        for (i in 1:length(tokenized)) {
            tokenized[[i]] <- c("INI", tokenized[[i]])
        }
        as.tokens(tokenized)
    } else {
        tokenized
    }
}

# Predict the first word
unigramPredict <- function(ngrams_model, n = 4, all = FALSE) {
    initial <- ngrams_model$bigram[grep("^INI ", ngrams_model$bigram$ngram), ]
    if (nrow(initial) == 0) {
        if (all) {
            ngrams_model$unigram
        } else {
            ngrams_model$unigram[1:n, ]
        }
    } else {
        if (all) {
            initial$ngram <- gsub("^INI ", "", initial$ngram)
            initial
        } else {
            initial <- initial[1:n, ]
            initial$ngram <- gsub("^INI ", "", initial$ngram)
            initial
        }
    }
}

# Predict the next word based on the last unigram
bigramPredict <- function(text, ngrams_model, n = 4, all = FALSE, ...) {
    unigrams <- tokenizeText(text, ...)
    tokensList <- as.list(unigrams)
    for (i in 1:length(tokensList)) {
        tokensList[[i]] <- paste0("^", tokensList[[i]], " ")
    }
    match <- unlist(tokensList)
    initial <- ngrams_model$bigram[grep(match[length(match)], ngrams_model$bigram$ngram), ]
    initial$ngram <- gsub(match[length(match)], "", initial$ngram)
    initial <- initial[grep("[^END]", initial$ngram), ]
    if (nrow(initial) >= n) {
        if (all) {
            initial
        } else {
            initial[1:n, ]
        }
    } else {
        initial
    }
}

# Predict the next word based on the last bigram
trigramPredict <- function(text, ngrams_model, n = 4, all = FALSE, ...) {
    bigrams <- tokenizeText(text, ...) %>% tokens_ngrams(n = 2L, concatenator = " ")
    tokensList <- as.list(bigrams)
    for (i in 1:length(tokensList)) {
        tokensList[[i]] <- paste0("^", tokensList[[i]], " ")
    }
    match <- unlist(tokensList)
    initial <- ngrams_model$trigram[grep(match[length(match)], ngrams_model$trigram$ngram), ]
    initial$ngram <- gsub(match[length(match)], "", initial$ngram)
    initial <- initial[grep("[^END]", initial$ngram), ]
    if (nrow(initial) >= n) {
        if (all) {
            initial
        } else {
            initial[1:n, ]
        }
    } else {
        initial
    }
}

# Predict the next word based on the last trigram
tetragramPredict <- function(text, ngrams_model, n = 4, all = FALSE, ...) {
    trigrams <- tokenizeText(text, ...) %>% tokens_ngrams(n = 3L, concatenator = " ")
    tokensList <- as.list(trigrams)
    for (i in 1:length(tokensList)) {
        tokensList[[i]] <- paste0("^", tokensList[[i]], " ")
    }
    match <- unlist(tokensList)
    initial <- ngrams_model$tetragram[grep(match[length(match)], ngrams_model$tetragram$ngram), ]
    initial$ngram <- gsub(match[length(match)], "", initial$ngram)
    initial <- initial[grep("[^END]", initial$ngram), ]
    if (nrow(initial) >= n) {
        if (all) {
            initial
        } else {
            initial[1:n, ]
        }
    } else {
        initial
    }
}

#####
# Create function to order unique results
orderPredict <- function(temp) {
    Unique <- unique(temp$ngram)
    Sum <- numeric()
    for (i in 1:length(Unique)) {
        Sum[i] <- sum(temp$freq[grepl(Unique[i], temp$ngram)])
    }
    data.frame(ngram = Unique, freq = Sum)
}

# Create the main prediction function, without back-off calculations
predict <- function(text, model, n = 4) {
    if (text != "") {
        tokenized <- unlist(tokenizeText(text, addINI = TRUE))
        temp1 <- head(model$unigram, n)
        temp <- temp1
        
        # Predict on trigram
        if (length(tokenized) >= 4) {
            temp4 <- tetragramPredict(text, model, all = TRUE)
            if (length(unique(temp4$ngram)) < n) {
                temp3 <- trigramPredict(text, model, all = TRUE)
                temp4 <- rbind(temp4, temp3)
                if (length(unique(temp4$ngram)) < n) {
                    temp2 <- bigramPredict(text, model, all = TRUE)
                    temp4 <- rbind(temp4, temp2)
                    if (length(unique(temp4$ngram)) < n) {
                        temp1 <- unigramPredict(model, all = TRUE)
                        temp4 <- rbind(temp4, temp1)
                    }
                }
            }
            temp <- temp4
        }
        
        # Predict on bigram
        if (length(tokenized) == 3) {
            temp3 <- trigramPredict(text, model, all = TRUE)
            if (length(unique(temp3$ngram)) < n) {
                temp2 <- bigramPredict(text, model, all = TRUE)
                temp3 <- rbind(temp3, temp2)
                if (length(unique(temp3$ngram)) < n) {
                    temp1 <- unigramPredict(model, all = TRUE)
                    temp3 <- rbind(temp3, temp1)
                }
            }
            temp <- temp3
        }
        
        # Predict on unigram
        if(length(tokenized) == 2){
            temp2 <- bigramPredict(text, model, all = TRUE)
            if (length(unique(temp2$ngram)) < n) {
                temp1 <- unigramPredict(model, all = TRUE)
                temp2 <- rbind(temp2, temp1)
            }
            temp <- temp2
        }
        
    } else {
        temp <- unigramPredict(model, all = TRUE)
    }
    
    temp <- temp[1:(4*n), ] %>% orderPredict()
    temp[1:n, ]
}

##########
### Back-off

# Create a function to convert frequencies to probabilities
calculateProb <- function(result) {
    total <- sum(result$freq)
    result <- mutate(result, prob = freq/total)
    result[ , c("ngram", "prob")]
}

# Calculate the discounted probabilities of observed bigrams given a unigram
bigramProb <- function(uni_gram, model, bigramDiscount = 5) {
    observed <- bigramPredict(uni_gram, model, all = TRUE)
    if (nrow(observed) == 0) {return(observed)}
    unigramCount <- filter(model$unigram, ngram == uni_gram)$freq
    if (identical(unigramCount, numeric(0))) {
        bigramProbability <- data.frame("ngram" = character(), "prob" = numeric())
    } else {
        bigramProbability <- mutate(observed, prob = (freq - bigramDiscount)/unigramCount)
        bigramProbability[ , c("ngram", "prob")]
    }
}

# Calculate the discounted probabilities of observed trigrams given a bigram
trigramProb <- function(bi_gram, model, trigramDiscount = 2) {
    observed <- trigramPredict(bi_gram, model, all = TRUE)
    if (nrow(observed) == 0) {return(observed)}
    bigramCount <- filter(model$bigram, ngram == bi_gram)$freq
    if (identical(bigramCount, numeric(0))) {
        trigramProbability <- data.frame("ngram" = character(), "prob" = numeric())
    } else {
        trigramProbability <- mutate(observed, prob = (freq - trigramDiscount)/bigramCount)
        trigramProbability[ , c("ngram", "prob")]
    }
}

# Calculate the discounted probabilities of observed tetragrams given a trigram
tetragramProb <- function(tri_gram, model, tetragramDiscount = 2) {
    observed <- tetragramPredict(tri_gram, model, all = TRUE)
    if (nrow(observed) == 0) {return(observed)}
    trigramCount <- filter(model$trigram, ngram == tri_gram)$freq
    if (identical(trigramCount, numeric(0))) {
        tetragramProbability <- data.frame("ngram" = character(), "prob" = numeric())
    } else {
        tetragramProbability <- mutate(observed, prob = (freq - tetragramDiscount)/trigramCount)
        tetragramProbability[ , c("ngram", "prob")]
    }
}

#####
# Create function to order unique results
orderPredictBO <- function(result) {
    Unique <- unique(result$ngram)
    Sum <- numeric()
    for (i in 1:length(Unique)) {
        Sum[i] <- sum(result$prob[grepl(Unique[i], result$ngram)])
    }
    data.frame(ngram = Unique, prob = Sum)
}

# Create the main prediction function, with back-off calculations
predictBO <- function(text, ngrams_model, n = 4) {
    text <- tokenizeText(text, addINI = TRUE)
    words <- unlist(text)
    
    # For trigrams
    if (length(words) >= 4) {
        trigrams <- text %>% tokens_ngrams(n = 3L, concatenator = " ") %>% as.list()
        tri_gram <- unlist(trigrams) %>% .[length(.)]
        tetragramOptions <- tetragramProb(tri_gram, ngrams_model)
        if (nrow(tetragramOptions) == 0) {
            mass_trigrams <- 1
        } else {
            mass_trigrams <- 1-sum(tetragramOptions$prob)
        }
        bigrams <- text %>% tokens_ngrams(n = 2L, concatenator = " ") %>% as.list()
        bi_gram <- unlist(bigrams) %>% .[length(.)]
        trigramOptions <- trigramProb(bi_gram, ngrams_model)
        if (nrow(trigramOptions) == 0) {
            mass_bigrams <- 1
        } else {
            trigramOptions$prob <- mass_trigrams*trigramOptions$prob
            mass_bigrams <- mass_trigrams-sum(trigramOptions$prob)
        }
        unigrams <- text %>% as.list()
        uni_gram <- unlist(unigrams) %>% .[length(.)]
        bigramOptions <- bigramProb(uni_gram, ngrams_model)
        if (nrow(bigramOptions) == 0) {
            mass_unigrams <- 1
        } else {
            bigramOptions$prob <- mass_bigrams*bigramOptions$prob
            mass_unigrams <- mass_bigrams-sum(bigramOptions$prob)
        }
        unigramOptions <- unigramPredict(ngrams_model, all = TRUE)
        unigramOptions <- calculateProb(unigramOptions)
        unigramOptions$prob <- mass_unigrams*unigramOptions$prob
        
        result <- rbind(tetragramOptions, trigramOptions, bigramOptions, unigramOptions)
    }
    
    # For bigrams
    if (length(words) == 3) {
        bigrams <- text %>% tokens_ngrams(n = 2L, concatenator = " ") %>% as.list()
        bi_gram <- unlist(bigrams) %>% .[length(.)]
        trigramOptions <- trigramProb(bi_gram, ngrams_model)
        if (nrow(trigramOptions) == 0) {
            mass_bigrams <- 1
        } else {
            mass_bigrams <- 1-sum(trigramOptions$prob)
        }
        unigrams <- text %>% as.list()
        uni_gram <- unlist(unigrams) %>% .[length(.)]
        bigramOptions <- bigramProb(uni_gram, ngrams_model)
        if (nrow(bigramOptions) == 0) {
            mass_unigrams <- 1
        } else {
            bigramOptions$prob <- mass_bigrams*bigramOptions$prob
            mass_unigrams <- mass_bigrams-sum(bigramOptions$prob)
        }
        unigramOptions <- unigramPredict(ngrams_model, all = TRUE)
        unigramOptions <- calculateProb(unigramOptions)
        unigramOptions$prob <- mass_unigrams*unigramOptions$prob
        
        result <- rbind(trigramOptions, bigramOptions, unigramOptions)
    }
    
    # For unigrams
    if (length(words) == 2) {
        unigrams <- text %>% as.list()
        uni_gram <- unlist(unigrams) %>% .[length(.)]
        bigramOptions <- bigramProb(uni_gram, ngrams_model)
        if (nrow(bigramOptions) == 0) {
            mass_unigrams <- 1
        } else {
            mass_unigrams <- 1-sum(bigramOptions$prob)
        }
        unigramOptions <- unigramPredict(ngrams_model, all = TRUE)
        unigramOptions <- calculateProb(unigramOptions)
        unigramOptions$prob <- mass_unigrams*unigramOptions$prob
        
        result <- rbind(bigramOptions, unigramOptions)
    }
    
    if (length(words) == 1) {
        unigramOptions <- unigramPredict(ngrams_model, all = TRUE)
        unigramOptions <- calculateProb(unigramOptions)
        
        result <- unigramOptions
    }
    
    result <- result[1:(4*n), ] %>% orderPredictBO()
    result[1:n, ]
}

################################################################################
### Thinking beyond...

## Some alternative data sets that could be used are books, forum posts or chats.

## The n-gram model is generally an insufficient language model because language has long-distance dependencies.

## The size of the model as a function of n grows exponentially; i.e., the greater n is, the bigger the model gets.

## The model performs poorly when n is small, but greatly increases its accuracy by increasing n. A reasonable choice seems 
# to be n equal to 4, because it is seen that the number of unique n-grams gets very large when n is greater than 4, but the 
# frequencies go down a lot; e.g., the number of 4-grams whose frequency is greater than 1 is almost half of the number of 
# 3-grams whose frequency is grean ter than 1.

## To reduce the size of the model, it must be noted that the number of n-grams whose frequency is 1 is greater than half of 
# the total sum of seen n-grams. Therefore, removing that huge amount of n-grams only seen once (which are probably typos), 
# greatly reduces the size of the model without sacrificing any accuracy.

## By removing the n-grams whose frequency is less than a threshold (e.g., 1), overfitting is avoided, and this leads to 
# better accuracy when applying the model to a test set, which ultimately results in a decrease in the out-of-sample error,  
# or so is expected.

## Another variable that can be included in the model is the position of the n-gram, which can be useful to account for the 
# grammar behind the construction of English sentences.

## A lot of bottlenecks are due to misplaced conditional statements. The model can be optimized by building each function so 
# that it only performs the operations that are needed.

################################################################################
### References

## Natural language processing. https://en.wikipedia.org/wiki/Natural_language_processing.

## CRAN Task View: Natural Language Processing. https://cran.r-project.org/web/views/NaturalLanguageProcessing.html.

## Feinerer, I., Hornik, K., & Meyer, D. (2008). Text Mining Infrastructure in R. Journal of Statistical Software, 25(5), 
# 1-54. http://dx.doi.org/10.18637/jss.v025.i05.

## Benoit, Kenneth, Kohei Watanabe, Haiyan Wang, Paul Nulty, Adam Obeng, Stefan Müller, and Akitaka Matsuo. (2018). 
# “quanteda: An R package for the quantitative analysis of textual data”. Journal of Open Source Software. 3(30), 774. 
# https://doi.org/10.21105/joss.00774.

## Jurafsky, Daniel & Martin, James. (2008). Speech and Language Processing: An Introduction to Natural Language Processing, 
# Computational Linguistics, and Speech Recognition.

## Pibiri, G., & Venturini, R. (2019). Handling Massive N-Gram Datasets Efficiently. ACM Transactions On Information Systems, 
# 37(2), 1-41. doi: 10.1145/3302913.
