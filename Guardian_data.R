library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tidyr)
library(tidytext)
library(ggplot2)
library(forcats)
library(stringr)
library(purrr)
library(xml2)
library(rvest)
library(textstem)
library(gridExtra)

# The Guardian API Key
guardian_api_key <- Sys.getenv("Gaurdian_API_Key")

# Function to pull The Guardian articles for a section/date range
get_guardian_articles <- function(section = "us-news", 
                                  from = floor_date(Sys.Date() - months(1), unit = "month"),
                                  to = ceiling_date(Sys.Date() - months(1), unit = "month") - days(1),
                                  pages = 2) {
  all_articles <- list()
  for (i in 1:pages) {
    res <- GET("https://content.guardianapis.com/search", query = list(
      section = section,
      "from-date" = format(from, "%Y-%m-%d"),
      "to-date" = format(to, "%Y-%m-%d"),
      "show-fields" = "headline,trailText,body",
      "page-size" = 150,
      "page" = i,
      "api-key" = guardian_api_key
    ))
    
    content_json <- fromJSON(content(res, "text"), flatten = TRUE)
    articles <- content_json$response$results
    
    if (length(articles) == 0) break  # Stop if empty
    
    df <- tibble(
      title = articles$fields.headline,
      summary = articles$fields.trailText,
      body = articles$fields.body,
      date = as.Date(articles$webPublicationDate),
      url = articles$webUrl,
      section = articles$sectionName
    )
    
    all_articles[[i]] <- df
  }
  
  bind_rows(all_articles)
}

# Pulling the News Articles Data
guardian_news <- get_guardian_articles(section = "us-news", pages = 100)

# Custom cleaning function to clean the raw body of the article
text_clean_function_gaurdian <- function(raw_text) {
  cleaned_text <- read_html(raw_text) %>%
    html_text2() %>% # Extract clean text (preserves paragraph breaks)
    str_squish() %>% # Remove excessive white space
    str_replace_all("'s\\b", "") %>% # Handling Possessive forms (Car's, Trump's etc.,)
    str_replace_all("[[:punct:]]", "") %>% # Removing Punctuation
    str_squish() # Remove excessive white space
  return(cleaned_text) 
}

# Applying the cleaning function on the raw body of the news article
guardian_news <- guardian_news %>% mutate(cleaned_body = map_chr(body, text_clean_function_gaurdian))

# Saving the File
write.csv(as.data.frame(guardian_news) , file = "guardian_news_v2.csv", row.names = FALSE, quote = TRUE)

# Objective 1: Topic Coverage & Prioritization
# Compare which topics are most frequently covered by USA Today vs. The New York Times in national news articles from the past month.

# Method 1: Using Top Words Approach (uni-grams and bi-grams)
gaurdian_news <- guardian_news %>% mutate(publisher = "The Gaurdian") %>% select(date, publisher, title, url, body, cleaned_body)

# Tokenization and Normalization (unigrams and bigrams)
tokenisation_standardisation_unigrams <- function(df){
  df <- df %>% 
    unnest_tokens(word, cleaned_body) %>% # Tokenization of the cleaned text of body
    filter(str_length(word) >= 4) %>% # Removing words with length < 4
    anti_join(stop_words, by = "word") %>% # Handling stopwords
    filter(str_detect(word, "^[a-z]+$")) %>%  # considering only alphabets
    mutate(lemma = lemmatize_words(word)) # Reducing the words to its base form
}

tokenisation_standardisation_bigrams <- function(df) {
  df <- df %>%
    unnest_tokens(bigram, cleaned_body, token = "ngrams", n = 2) %>%
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    mutate(
      word1 = lemmatize_words(word1),
      word2 = lemmatize_words(word2)
    ) %>% # Reducing the words to its base form
    filter(
      str_length(word1) >= 4, 
      str_length(word2) >= 4,
      str_detect(word1, "^[a-z]+$"),
      str_detect(word2, "^[a-z]+$")
    ) %>% # Removing words with length < 4 and only words with alphabets
    anti_join(stop_words, by = c("word1" = "word")) %>% # Handling stopwords
    anti_join(stop_words, by = c("word2" = "word")) %>% # Handling stopwords
    unite("bigram", word1, word2, sep = " ")
}

# Tokenization and Standardisation of the words (unigrams)
gaurdian_data_tokenized_unigrams <- tokenisation_standardisation_unigrams(gaurdian_news)

# Topic Coverage Analysis based on Uni-grams
top_unigrams <- gaurdian_data_tokenized_unigrams %>%
  count(publisher, lemma, sort = TRUE) %>%
  group_by(publisher) %>%
  slice_max(n, n = 15) %>%
  ungroup()

# Visualization of top words from the news articles
p1 <- top_unigrams %>%
  ggplot(aes(x = reorder_within(lemma, n, publisher), y = n, fill = publisher)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~publisher, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Top Unigrams in U.S. News Articles by Publisher (Last 30 Days)",
       x = "UniGram", y = "Frequency")

# Tokenization and Standardisation of the words (unigrams)
gaurdian_data_tokenized_bigrams <- tokenisation_standardisation_bigrams(gaurdian_news)

# Topic Coverage Analysis based on Bi-grams
top_bigrams <- gaurdian_data_tokenized_bigrams %>%
  count(publisher, bigram, sort = TRUE) %>%
  group_by(publisher) %>%
  slice_max(n, n = 15) %>%
  ungroup()

# Visualization of top words from the news articles
p2 <- top_bigrams %>%
  ggplot(aes(x = reorder_within(bigram, n, publisher), y = n, fill = publisher)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~publisher, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Top BiGrams in U.S. News Articles by Publisher (Last 30 Days)",
       x = "BiGram", y = "Frequency")

grid.arrange(p1, p2, ncol = 2)

# Method 2: Implementing TF-IDF (Themes unique to either publisher)
article_words <- all_articles %>%
  unnest_tokens(word, cleaned_body) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "^[a-z]+$"))  # keep only alphabetical words

tfidf_words <- article_words %>%
  count(publisher, word, sort = TRUE) %>%
  bind_tf_idf(word, publisher, n) %>%
  arrange(desc(tf_idf))

tfidf_words %>%
  group_by(publisher) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = publisher)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~publisher, scales = "free") +
  labs(title = "Top TF-IDF Words by Publisher",
       x = "TF-IDF", y = NULL) +
  theme_minimal()

# Objective 2: Sentiment Analysis

# Dowloading sentiment lexicons
sentiment_words_bing <- get_sentiments("bing")
sentiment_words_nrc <- get_sentiments("nrc")

# Measuring sentiment of the articles (based on bing lexicon)
article_sentiment <- gaurdian_news %>%
  unnest_tokens(word, cleaned_body) %>%
  inner_join(sentiment_words, by = "word") %>%
  count(publisher, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)

gaurdian_news <- gaurdian_news %>%  mutate(date = as.Date(date))

# Plot of the Net Sentiment of The Gaurdian Articles
ggplot(article_sentiment, aes(x = publisher, y = net_sentiment, fill = publisher)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Net Sentiment of Articles by Publisher",
       y = "Net Sentiment (Positive - Negative)", x = "Publisher")

# Measuring the sentiment of articles by day
sentiment_by_day <- gaurdian_news %>%
  unnest_tokens(word, cleaned_body) %>%
  inner_join(sentiment_words_bing, by = "word") %>%
  count(date, publisher, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)


# Plot of the Sentiment of The Gaurdian Articles by Day
ggplot(sentiment_by_day, aes(x = date, y = net_sentiment)) +
  geom_line(linewidth = 1.1) +
  labs(title = "Net Sentiment Over Time (The Gaurdian)",
       x = "Date", y = "Net Sentiment (Positive - Negative)") +
  theme_minimal()
























