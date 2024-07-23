#library imports go here
library(tm)
library(SnowballC)
library(wordcloud)
library(tidytext)
library(dplyr)
library(ggplot2)
library(textdata)
library(sentimentr)
library(plotly)

setwd("/Users/anujprabhu/dat301/Honors Contract/Tweet Sentiment Analysis")
current_directory <- getwd()

stockerbot_df_path <- file.path(current_directory, "/archive/stockerbot-export.csv")
stocks_cleaned_df_path <- file.path(current_directory, "/archive/stocks_cleaned.csv")
stockerbot_df <- read.csv(stockerbot_df_path, header = TRUE, sep = ",")
stocks_cleaned_df <- read.csv(stocks_cleaned_df_path, header = TRUE, sep = ",")

# Set seed for reproducibility
set.seed(123)
# Randomly select 500 row indices
sample_indices <- sample(nrow(stockerbot_df), 500)
# Subset the dataframe using the sampled indices
stockerbot_df <- stockerbot_df[sample_indices, ]

# GENERAL SUMMARY
#View(stockerbot_df)
str(stockerbot_df)
summary(stockerbot_df)
head(stockerbot_df)
nrow(stockerbot_df)
#length(unique(stockerbot_df$company_names))

#View(stocks_cleaned_df)
str(stocks_cleaned_df)
summary(stocks_cleaned_df)
head(stocks_cleaned_df)
nrow(stocks_cleaned_df)
#length(unique(stocks_cleaned_df$ticker))

# Data Cleaning (work with stockerbot_df) and Data Visualization

# Creating Vector Corpus of tweet text
text_corpus <- Corpus(VectorSource(stockerbot_df$text))
num_words_before <- length(text_corpus)
num_words_before

text_corpus <- tm_map(text_corpus, content_transformer(tolower))
# Removing stopwords
my_stopwords <- c(stopwords("en"), "rt", "jul")
text_corpus <- tm_map(text_corpus, removeWords, my_stopwords)
num_words_after <- length(text_corpus)
num_words_after

# Removing urls from tweets
remove_url_http <- function(x) gsub("http[^[:space:]]*", "", x)
text_corpus <- tm_map(text_corpus, content_transformer(remove_url_http))
remove_url_www <- function(x) gsub("www\\S+", "", x)
text_corpus <- tm_map(text_corpus, content_transformer(remove_url_www))

# Removing anything other than english letters and space
remove_cust_punc <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
text_corpus <- tm_map(text_corpus, content_transformer(remove_cust_punc))
text_corpus <- tm_map(text_corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
text_corpus <- tm_map(text_corpus, stripWhitespace)

# Stem corpus documents
text_corpus <- tm_map(text_corpus, stemDocument)

# Create Document Term Matrix
dtm <- DocumentTermMatrix(text_corpus)
inspect(dtm[1:10, 2:7])

wordcloud(text_corpus, min.freq = 5, colors = brewer.pal(8, "Set2"), random.order = F)

# Exploratory Data Analysis and Data Visualization

#tolower
stockerbot_df$text <- tolower(stockerbot_df$text)
#remove alphanumberic words
stockerbot_df$text <- gsub("[^0-9A-Za-z///' ]", "", stockerbot_df$text)
#remove links
stockerbot_df$text <- gsub("http\\w+", "", stockerbot_df$text)
#remove retweet (rt)
stockerbot_df$text <- gsub("rt", "", stockerbot_df$text)
#remove @
stockerbot_df$text <- gsub("@\\w+", "", stockerbot_df$text)

sent_tweets <- sentiment(stockerbot_df$text)
#View(sent_tweets)
stockerbot_df$sentiment <- sent_tweets$sentiment
#View(stockerbot_df)
positive_tweets <- head(unique(stockerbot_df[order(sent_tweets$sentiment, decreasing = TRUE), c(2, 9)]), 25)
positive_tweets

negative_tweets <- head(unique(stockerbot_df[order(sent_tweets$sentiment), c(2, 9)]), 25)
negative_tweets

overall_sentiment <- sum(stockerbot_df$sentiment)
overall_sentiment # positive; but not a good way to do sentiment analysis

# Use aggregate to calculate the sum of sentiment scores for each symbol
summed_sentiment <- aggregate(stockerbot_df$sentiment, by=list(stockerbot_df$symbols), FUN=sum)

# Rename the columns of the aggregated data frame
colnames(summed_sentiment) <- c("Symbol", "SummedSentiment")

# Display the result
# Sort the summed sentiment data frame in decreasing order of sentiment scores
summed_sentiment <- summed_sentiment[order(summed_sentiment$SummedSentiment, decreasing = TRUE), ]

# Create the Pareto chart using ggplot2
score_by_symbol_plot <- ggplot(summed_sentiment, aes(x = reorder(Symbol, -SummedSentiment), y = SummedSentiment)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Pareto Chart of Summed Sentiment Scores by Symbol",
       x = "",  # Empty x-axis label
       y = "Summed Sentiment Score") +
  theme(axis.text.x = element_blank())  # Remove x-axis labels
ggplotly(score_by_symbol_plot)

#findAssocs(dtm, "inc", .3)

#dtm_td = tidy(dtm)

# unique(stocks_cleaned_df[stocks_cleaned_df$ticker == "WPX", ]$name)
# unique(stocks_cleaned_df[stocks_cleaned_df$ticker == "OMG", ]$name)
# unique(stocks_cleaned_df[stocks_cleaned_df$ticker == "DXC", ]$name)
# unique(stocks_cleaned_df[stocks_cleaned_df$ticker == "IBM", ]$name)
# unique(stocks_cleaned_df[stocks_cleaned_df$ticker == "TEL", ]$name)
# unique(stocks_cleaned_df[stocks_cleaned_df$ticker == "AIZ", ]$name)

pos_comps <- unique(stockerbot_df[stockerbot_df$symbols %in% c("WPX", "OMG", "DXC", "IBM", "TEL", "AIZ"), ]$company_names)
neg_comps <- unique(stockerbot_df[stockerbot_df$symbols %in% c("KMB", "BXP", "MNK", "JNPR", "MCD", "CSCO"), ]$company_names)
neut_comps <- unique(stockerbot_df[stockerbot_df$symbols %in% c("ARRY"), ]$company_names)
typeof(pos_comps)
pos_comps
neg_comps
