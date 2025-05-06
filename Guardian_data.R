
# Loading required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tidyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(forcats)
library(stringr)
library(purrr)
library(xml2)
library(rvest)
library(textstem)
library(gridExtra)
library(gganimate)
library(gifski)
library(transformr)
library(usdata)
library(maps)
library(spacyr)
library(readr)
install.packages("spacyr")
spacy_install(lang_models = "en_core_web_sm")
spacy_download_langmodel()
spacy_initialize(model = "en_core_web_sm")

# The Guardian API Key
gaurdian_api_key <- Sys.getenv("Gaurdian_API_Key")

# Function to pull The Guardian articles for a section/date range
get_gaurdian_articles <- function(section = "us-news", 
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
      "api-key" = gaurdian_api_key
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
gaurdian_news <- get_gaurdian_articles(section = "us-news", pages = 100)

# Saving the File
# write.csv(as.data.frame(gaurdian_news) , file = "gaurdian_news_v2.csv", row.names = FALSE, quote = TRUE)
# gaurdian_news <- read.csv("gaurdian_news_v2.csv")

# Custom cleaning function to clean the raw body of the article
text_clean_function_gaurdian <- function(raw_text) {
  cleaned_text <- read_html(raw_text) %>%
    html_text2() %>% # Extract clean text (preserves paragraph breaks)
    str_squish() %>% # Remove excessive white space
    str_to_lower() %>% 
    str_replace_all("\\b(\\w+)'\\w*\\b", "\\1") %>% # Handling Possessive forms (Car's, Trump's etc.,)
    str_replace_all("[[:punct:]]", "") %>% # Removing Punctuation
    str_squish() # Remove excessive white space
  return(cleaned_text) 
}

# Applying the cleaning function on the raw body of the news article
gaurdian_news <- gaurdian_news %>% mutate(cleaned_body = map_chr(body, text_clean_function_gaurdian))
gaurdian_news <- gaurdian_news %>%  mutate(date = as.Date(date))

# Objective 1: Topic Coverage & Prioritization
# Compare which topics are most frequently covered by USA Today vs. The New York Times in national news articles from the past month.

# Method 1: Using Top Words Approach (uni-grams and bi-grams)
gaurdian_news <- gaurdian_news %>% mutate(publisher = "The Gaurdian") %>% select(date, publisher, title, url, body, cleaned_body)

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
  labs(title = "Top UniGrams (The Gaurdian)",
       x = "UniGram", y = "Frequency") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

p1

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
  labs(title = "Top BiGrams (The Gaurdian)",
       x = "BiGram", y = "Frequency") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

p2

grid.arrange(p1, p2, ncol = 2)

# Method 2: Implementing TF-IDF (Themes unique to either publisher)
# Loading New York Times News Articles (April 2025)
newyorktimes_news_v1 <- read_csv("newyorktimes_news_v1.csv")

# Merging both datasets
newyorktimes_news_v1 <- newyorktimes_news_v1 %>% rename(date = pub_date)
all_articles <- bind_rows(newyorktimes_news_v1 %>% select(date, publisher, cleaned_body),
                          gaurdian_news %>% select(date, publisher, cleaned_body))
# parse full article bodies
parsed_articles <- spacy_parse(all_articles$cleaned_body, pos = TRUE, lemma = FALSE)

parsed_articles <- parsed_articles %>% 
  mutate(doc_id = str_extract_all(doc_id, "[:digit:]+")) %>% 
  unnest(cols = c(doc_id)) %>% 
  mutate(doc_id = as.numeric(doc_id))

# join back publisher info
parsed_articles <- parsed_articles %>% mutate(publisher = all_articles$publisher[doc_id])

meaningful_words <- parsed_articles %>%
  filter(pos %in% c("NOUN", "VERB", "ADJ", "ADV")) %>%
  filter(str_length(token) >= 4, str_detect(token, "^[a-z]+$")) %>%
  anti_join(stop_words, by = c("token" = "word")) %>%
  count(publisher, token, sort = TRUE)

word_tfidf <- meaningful_words %>%
  bind_tf_idf(term = token, document = publisher, n = n) %>%
  arrange(desc(tf_idf))

top_terms <- word_tfidf %>%
  group_by(publisher) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup()

p3 <- ggplot(top_terms, aes(x = reorder_within(token, tf_idf, publisher),
                      y = tf_idf, fill = publisher)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ publisher, scales = "free") +
      coord_flip() +
      scale_x_reordered() +
      labs(title = "Top Thematic Words by Publisher (TF-IDF)",
           x = "Word", y = "TF-IDF Score") +
      theme(
        plot.title = element_text(hjust = 0.5)
      )

p3

# Objective 2: Sentiment Analysis
# Downloading sentiment lexicons
sentiment_words_bing <- get_sentiments("bing") # for positive-negative sentiment
sentiment_words_nrc <- get_sentiments("nrc") # for emotion tracking

# Measuring sentiment of the articles (based on bing lexicon)
article_sentiment <- gaurdian_news %>%
  unnest_tokens(word, cleaned_body) %>%
  inner_join(sentiment_words_bing, by = "word") %>%
  count(publisher, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)

# Plot of the Net Sentiment of The Gaurdian Articles
p4 <- ggplot(article_sentiment, aes(x = publisher, y = net_sentiment, fill = publisher)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Net Sentiment of Articles by Publisher",
       y = "Net Sentiment (Positive - Negative)", x = "Publisher")

p4 

# Measuring the sentiment of articles by day
sentiment_by_day <- gaurdian_news %>%
  unnest_tokens(word, cleaned_body) %>%
  inner_join(sentiment_words_bing, by = "word") %>%
  count(date, publisher, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)

# Plot of the Sentiment of The Gaurdian Articles by Day (also volume of articles)
article_counts <- gaurdian_news %>% count(date, publisher, name = "article_count") # Measuring counts of articles
sentiment_by_day <- sentiment_by_day %>% left_join(article_counts, by = c("date"))

scale_factor <- max(abs(sentiment_by_day$net_sentiment), na.rm = TRUE) / max(sentiment_by_day$article_count, na.rm = TRUE)
sentiment_by_day <- sentiment_by_day %>% mutate(scaled_count = article_count * scale_factor)


p5 <- ggplot(sentiment_by_day, aes(x = date)) +
  geom_col(aes(y = scaled_count), fill = "steelblue", alpha = 0.3) +  # article volume
  geom_line(aes(y = net_sentiment), color = "darkred", linewidth = 1.1) +  # sentiment
  scale_y_continuous(
    name = "Net Sentiment (Positive - Negative)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Article Volume", breaks = pretty(sentiment_by_day$article_count))
  ) +
  labs(
    title = "Net Sentiment and Article Volume Over Time (The Guardian)",
    x = "Date"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y.left = element_text(color = "darkred"),
    axis.title.y.right = element_text(color = "steelblue")
  )

p5

# Tracking emotions per day
emotion_by_day <- gaurdian_news %>%
  unnest_tokens(word, cleaned_body) %>%
  inner_join(sentiment_words_nrc, by = "word", relationship = "many-to-many") %>%
  count(date, publisher, sentiment)  # `sentiment` includes joy, anger, fear, etc.

# Choose emotions to track
selected_emotions <- c("anger", "trust", "fear", "joy", "sadness", "anticipation")

# Plot of the Emotions of The Gaurdian Articles by day
p6 <- emotion_by_day %>%
  filter(sentiment %in% selected_emotions) %>%
  ggplot(aes(x = date, y = n, color = publisher)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ sentiment, scales = "free_y", ncol = 2) +
  labs(
    title = "Emotion Trends Over Time by Publisher",
    subtitle = paste("Tracked emotions:", paste(selected_emotions, collapse = ", ")),
    x = "Date",
    y = "Emotion Word Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

p6

# Objective 3: Geographic Analysis

# Run NLP pipeline on all article text
parsed_locations <- spacy_extract_entity(
  gaurdian_news$cleaned_body,
  entity_type = "GPE"
)

# Filtering for entities named as "GPE" (Geo-Political Entity ==> Countries, States, details)
parsed_locations_gpe <- parsed_locations %>% filter(ent_type == "GPE")

parsed_locations_gpe <- parsed_locations_gpe %>%   mutate(text = tolower(text))
parsed_locations_gpe <- parsed_locations_gpe %>% 
  mutate(doc_id = str_extract_all(doc_id, "[:digit:]+")) %>% 
  unnest(cols = c(doc_id)) %>% 
  mutate(doc_id = as.numeric(doc_id)) %>% 
  left_join(
    guardian_news %>%
      mutate(doc_id = row_number()) %>%
      select(doc_id, date, url),
    by = "doc_id"
  )

data("us.cities", package = "maps")
us_cities <- us.cities

us_cities_clean <- us_cities %>%
  mutate(city_clean = tolower(name)) %>% 
  mutate(
    parts = str_split(city_clean, "\\s+"),
    state = map_chr(parts, ~ tail(.x, 1)),              # last word as state
    city = map_chr(parts, ~ paste(head(.x, -1), collapse = " "))  # everything except last word
  ) %>%
  select(-parts)

city_matches <- parsed_locations_gpe %>%
  inner_join(us_cities_clean, by = c("text" = "city"), relationship = "many-to-many") %>% 
  mutate(date = as.Date(date))

city_summary <- city_matches %>%
  count(city_clean, state, lat, long, name = "article_count")

state_summary <- city_matches %>%
  count(state, name = "article_count")

us_states <- map_data("state")


p7 <- ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group),
               fill = "orange2", color = "gray80") +
  geom_point(data = city_summary,
             aes(x = long, y = lat, size = article_count),
             alpha = 0.7) +
  coord_fixed(1.3) +
  labs(title = "City-Level Article Mentions", x = "", y = "") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
p7



state_summary <- state_summary %>%
  mutate(region = tolower(abbr2state(state)))  # converts "TX" → "texas"

p8 <- ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "white") +
  geom_map(data = state_summary, map = us_states,
           aes(map_id = region, fill = article_count),
           color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "State-Level Article Mentions", fill = "Article Count") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

p8

state_weekly <- city_matches %>%
  mutate(week = lubridate::floor_date(date, unit = "week")) %>%
  count(state, week, name = "article_count") %>%
  mutate(region = tolower(abbr2state(state)))  # convert "CA" → "california"

map_data_weekly <- us_states %>%
  left_join(state_weekly, by = "region", relationship = "many-to-many")

p_anim <- ggplot(map_data_weekly, aes(x = long, y = lat, group = group)) +
  # Base US map polygons
  geom_polygon(aes(fill = article_count), color = "white", linewidth = 0.3) +
  
  # Full US shape with fixed aspect ratio
  coord_fixed(1.3) +
  
  # Color scale
  scale_fill_viridis_c(option = "inferno", direction = -1, na.value = "grey90") +
  
  # Improved theme
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 10)),
    legend.position = "right"
  ) +
  
  # Animate over weeks
  transition_time(week) +
  labs(
    title = "Weekly Article Mentions by State",
    subtitle = "Week of: {frame_time}",
    fill = "Article Count"
  ) +
  ease_aes("linear")

# Save as GIF
animate(p_anim, 
        width = 900, 
        height = 500, 
        duration = 10,
        fps = 5,
        renderer = gifski_renderer("animated_map.gif"))


