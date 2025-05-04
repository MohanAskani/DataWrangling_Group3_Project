
# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(purrr)
library(rvest)
library(stringr)
library(tidytext)
library(tidyverse)
library(stopwords)
library(textstem)
library(lubridate)
library(gridExtra)

# Calling New York Times API through curl
system('curl -s -o nyt-2025-4.json "https://api.nytimes.com/svc/archive/v1/2025/4.json?api-key=3MBRKQHHWDYuuAeX9QpkVd95XlrPBjCR"')

# Adjust path as needed
json_data <- fromJSON("nyt-2025-4.json", flatten = TRUE)

# Extract articles
docs <- json_data$response$docs

# Extract only US section News 
us_news <- docs %>% filter(section_name %in% c('U.S.') & type_of_material == 'News')
dim(us_news)

# Extract body function from page URLs
body_extraction <- function(url) {
  page <- read_html(url)
  paragraphs <- page %>% html_elements("p") %>% html_text2()
  clean_text <- paste(paragraphs, collapse = " ")
  return(clean_text)
}

# Call body extract function and store output in full_body
us_news <- us_news %>% mutate(full_body = map_chr(web_url, body_extraction))

# Full body cleaning function starts
clean_full_body <- function(text) {
  if (is.null(text) || is.na(text) || !is.character(text)) return("")
  cleaned <- text %>%
    gsub("(?:'|’)[sS]\\b", "", .) %>%
    # Remove the specific problematic sentence (all variations)
    gsub("We are having trouble retrieving the article content\\..*?(Log in\\.|Subscribe\\.|Advertisement)", "", ., perl = TRUE) %>%
    # Your existing cleaning steps
    gsub("(?i)Advertisement", "", .) %>%
    gsub("(?i)Please enable JavaScript in your browser settings", "", .) %>%
    gsub("(?i)while we verify access", "", .) %>%
    gsub("(?i)please exit and log into your Times account, or subscribe for all of The Times\\.", "", .) %>%
    gsub("(?i)Supported by", "", .) %>%
    gsub("(?i)Thank you for your patience.*?(Advertisement)?", "", ., perl = TRUE) %>%
    gsub("(?i)If you are in Reader mode.*?(Advertisement)?", "", ., perl = TRUE) %>%
    gsub("(?i)Already a subscriber\\? Log in\\.", "", .) %>%
    gsub("(?i)Want all of The Times\\? Subscribe\\.", "", .) %>%
    gsub("(?i)^By [A-Z][a-z]+ [A-Z][a-z]+( and [A-Z][a-z]+ [A-Z][a-z]+)?", "", .) %>%
    gsub("(?i)[A-Z][a-z]+ [A-Z][a-z]+ (reported from|covers|writes).*?\\.", "", .) %>%
    gsub("\\[.*?\\]", "", .) %>%
    gsub("[^A-Za-z .,!?]", " ", .) %>%
    gsub("\n", " ", .) %>%
    # Tokenization and stopword removal
    {.} %>%  # Pipe placeholder
    tibble(text = .) %>%
    unnest_tokens(word, text) %>%
    anti_join(get_stopwords(), by = "word") %>%
    summarise(cleaned = str_c(word, collapse = " ")) %>%
    pull(cleaned) %>%
    # Final cleaning
    gsub("[,.]", "", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws()
  return(cleaned)
}

# Call clean full body function and store output in cleaned_body
us_news <- us_news %>% mutate(cleaned_body = map_chr(full_body, clean_full_body))
#glimpse(us_news)
#cat(us_news$full_body[340])
#cat(us_news$cleaned_body[340])

# Select and rename the required columns
nytimes_processed <- us_news %>%
  select(
    abstract,
    pub_date,
    publisher = source,       
    raw_body = full_body,      
    cleaned_body            
  ) %>%
  mutate(
    pub_date = as.Date(sub("T.*", "", pub_date)),
    # Ensure publisher is consistently labeled
    publisher = "The New York Times"
  )

dim(nytimes_processed)
# Save to CSV if needed
#write_csv(nytimes_processed, "nytimes_processed.csv")
write.csv(as.data.frame(nytimes_processed) , file = "newyorktimes_news_v1.csv", row.names = FALSE, quote = TRUE)

# Find out most discussed topics in NY Times
# Tokenization starts (Uni-grams and Bi-grams)
tokenisation_standardisation_unigrams <- function(df){
  df <- df %>% 
    unnest_tokens(word, cleaned_body) %>% # Tokenization of the cleaned text of body
    filter(str_length(word) >= 4) %>% # Removing words with length < 4
    anti_join(stop_words, by = "word") %>% # Handling stopwords
    filter(str_detect(word, "^[a-z]+$")) %>%  # considering only alphabets
    mutate(lemma = lemmatize_words(word)) # Reducing the words to its base form
}
nytimes_data_tokenized <- tokenisation_standardisation_unigrams(nytimes_processed)

# Analyze frequently discussed topics based on Uni-grams
top_unigrams <- nytimes_data_tokenized %>%
  count(publisher, lemma, sort = TRUE) %>%
  group_by(publisher) %>%
  slice_max(n, n = 15) %>%
  ungroup()
p1 <- top_unigrams %>%
  ggplot(aes(x = reorder_within(lemma, n, publisher), y = n, fill = publisher)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~publisher, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Top Unigrams in The New York Times Articles by Publisher (Last 30 Days)",
       x = "UniGram", y = "Frequency")

tokenisation_standardisation_bigrams <- function(df) {
  df <- df %>%
    unnest_tokens(bigram, cleaned_body, token = "ngrams", n = 2) %>%
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    mutate(
      word1 = lemmatize_words(word1),
      word2 = lemmatize_words(word2)
    ) %>% # Reducing the words to its base form
    filter(
      str_length(word1) >= 3, 
      str_length(word2) >= 3,
      str_detect(word1, "^[a-z]+$"),
      str_detect(word2, "^[a-z]+$")
    ) %>% # Removing words with length < 4 and only words with alphabets
    anti_join(stop_words, by = c("word1" = "word")) %>% # Handling stopwords
    anti_join(stop_words, by = c("word2" = "word")) %>% # Handling stopwords
    unite("bigram", word1, word2, sep = " ")
}
nytimes_data_tokenized_bigrams <- tokenisation_standardisation_bigrams(nytimes_processed)

# Analyze frequently discussed topics based on Bi-grams
top_bigrams <- nytimes_data_tokenized_bigrams %>%
  count(publisher, bigram, sort = TRUE) %>%
  group_by(publisher) %>%
  slice_max(n, n = 15) %>%
  ungroup()
p2 <- top_bigrams %>%
  ggplot(aes(x = reorder_within(bigram, n, publisher), y = n, fill = publisher)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~publisher, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Top BiGrams in The New York Times Articles by Publisher (Last 30 Days)",
       x = "BiGram", y = "Frequency")
grid.arrange(p1, p2, ncol = 2)


# Dowloading sentiment lexicons
sentiment_words_bing <- get_sentiments("bing")
sentiment_words_nrc <- get_sentiments("nrc")

# Measuring sentiment of the articles (based on bing lexicon)
article_sentiment <- nytimes_processed %>%
  unnest_tokens(word, cleaned_body) %>%
  inner_join(sentiment_words_bing, by = "word") %>%
  count(publisher, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)

# Plot of the Net Sentiment of The NY Times Articles
ggplot(article_sentiment, aes(x = publisher, y = net_sentiment, fill = publisher)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Net Sentiment of Articles by Publisher",
       y = "Net Sentiment (Positive - Negative)", x = "Publisher")

# Measuring the sentiment of articles by day
sentiment_by_day <- nytimes_processed %>%
  unnest_tokens(word, cleaned_body) %>%
  inner_join(sentiment_words_bing, by = "word") %>%
  count(pub_date, publisher, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)

# Plot of the Sentiment of The NY Times Articles by Day
ggplot(sentiment_by_day, aes(x = pub_date, y = net_sentiment)) +
  geom_line(linewidth = 1.1) +
  labs(title = "Net Sentiment Over Time (NY Times)",
       x = "Date", y = "Net Sentiment (Positive - Negative)") +
  theme_minimal()
