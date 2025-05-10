# loading required libraries
library(rvest)
library(readr)
library(tidyverse)
library(httr)
library(dplyr)
library(tidytext)
library(textstem)
library(ggplot2)
library(stringr)
library(tidyr)
library(spacyr)
spacy_initialize(model = "en_core_web_sm")

library(purrr)
library(maps)
library(usdata)
library(gganimate)
library(viridis)
library(gifski)

(combined_news$cleaned_body[1])

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
guardian_news <- get_gaurdian_articles(section = "us-news", pages = 100)


# Saving and reading the File
# write.csv(as.data.frame(gaurdian_news) , file = "gaurdian_news_v2.csv", row.names = FALSE, quote = TRUE)
guardian_news <- read_csv("data/guardian_news_v2.csv")

# Custom cleaning function
text_clean_function_guardian <- function(raw_text) {
  cleaned_text <- read_html(raw_text) %>%
    html_text2() %>% # Extract clean text (preserves paragraph breaks)
    str_to_lower() %>% # converts word to lower case
    str_replace_all("\\b(\\w+)(['’]s)\\b", "\\1") %>% # Handling Possessive forms (Car's, Trump's etc.,)
    str_squish() %>% # Remove excessive white space
    
    # Remove numbers (standalone or within words like "covid19")
    str_replace_all("\\b\\w*\\d+\\w*\\b", "") %>%
    
    str_replace_all("[[:punct:]]", " ") %>% # Removing Punctuation
    str_squish() # Remove excessive white space
  
  return(cleaned_text) 
}

guardian_news <- guardian_news %>% mutate(cleaned_body = map_chr(body, text_clean_function_guardian))


# Calling New York Times API through curl
system('curl -s -o nyt-2025-4.json "https://api.nytimes.com/svc/archive/v1/2025/4.json?api-key=3MBRKQHHWDYuuAeX9QpkVd95XlrPBjCR"')

# Adjust path as needed
json_data <- fromJSON("nyt-2025-4.json", flatten = TRUE)

# Extract articles
docs <- json_data$response$docs

# Extract only US section News 
us_news <- docs %>% filter(section_name %in% c('U.S.') & type_of_material == 'News')

# Extract body function from page URLs
body_extraction <- function(url) {
  page <- read_html(url)
  paragraphs <- page %>% html_elements("p") %>% html_text2()
  clean_text <- paste(paragraphs, collapse = " ")
  return(clean_text)
}

# Call body extract function and store output in full_body (stored only those with atleast 300 characters)
us_news <- us_news %>% mutate(raw_body = map_chr(web_url, body_extraction)) %>% filter(nchar(raw_body) >= 300)

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
    gsub("\n", " ", .)
  return(cleaned)
}

# Call clean full body function and store output in cleaned_body
us_news <- us_news %>% mutate(cleaned_body = map_chr(raw_body, clean_full_body))

# Saving and reading the File
# write.csv(as.data.frame(us_news) , file = "newyorktimes_news_v1.csv", row.names = FALSE, quote = TRUE)
nyt_news <- read_csv("data/newyorktimes_news_v1.csv")

guardian_news <- guardian_news %>% mutate(publisher = "The Guardian") %>% rename(pub_date = date)

# merging both the datasets
combined_news <- bind_rows(
  guardian_news %>% select(publisher, pub_date, cleaned_body),
  nyt_news %>% select(publisher, pub_date, cleaned_body)
)

# Tokenization and Normalization (unigrams)
tokenisation_standardisation_unigrams <- function(df){
  df <- df %>% 
    unnest_tokens(word, cleaned_body) %>% # Tokenization of the cleaned text of body
    filter(str_length(word) >= 4) %>% # Removing words with length < 4
    anti_join(stop_words, by = "word") %>% # Handling stopwords
    filter(str_detect(word, "^[a-z]+$")) %>%  # considering only alphabets
    mutate(lemma = lemmatize_words(word)) # Reducing the words to its base form
}

# Tokenization and Standardisation of the words (unigrams)
guardian_data_tokenized_unigrams <- tokenisation_standardisation_unigrams(combined_news)

# Topic Coverage Analysis based on Uni-grams
top_unigrams <- guardian_data_tokenized_unigrams %>%
  count(publisher, lemma, sort = TRUE) %>%
  group_by(publisher) %>%
  slice_max(n, n = 15) %>%
  ungroup()

top_unigrams %>%
  ggplot(aes(x = reorder_within(lemma, n, publisher), y = n, fill = publisher)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~publisher, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Top UniGrams by Publisher",
       x = "UniGram", y = "Frequency") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Tokenization and Normalization (bigrams)
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

# Tokenization and Standardisation of the words (bigrams)
guardian_data_tokenized_bigrams <- tokenisation_standardisation_bigrams(combined_news)

# Topic Coverage Analysis based on Bi-grams
top_bigrams <- guardian_data_tokenized_bigrams %>%
  count(publisher, bigram, sort = TRUE) %>%
  group_by(publisher) %>%
  slice_max(n, n = 15) %>%
  ungroup()

# Visualization of top words from the news articles
top_bigrams %>%
  ggplot(aes(x = reorder_within(bigram, n, publisher), y = n, fill = publisher)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~publisher, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Top BiGrams by Publisher",
       x = "BiGram", y = "Frequency") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# parse full article bodies
parsed_articles <- spacy_parse(combined_news$cleaned_body, pos = TRUE, lemma = FALSE)

parsed_articles <- parsed_articles %>% 
  mutate(doc_id = str_extract_all(doc_id, "[:digit:]+")) %>% 
  unnest(cols = c(doc_id)) %>% 
  mutate(doc_id = as.numeric(doc_id))

# join back publisher info
parsed_articles <- parsed_articles %>% mutate(publisher = combined_news$publisher[doc_id])

# Determining the meaningful words
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

# Plotting TF-IDF top 15 words unique to each publisher
ggplot(top_terms, aes(x = reorder_within(token, tf_idf, publisher),
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

# Downloading sentiment lexicons
sentiment_words_bing <- get_sentiments("bing") # for positive-negative sentiment
sentiment_words_nrc <- get_sentiments("nrc") # for emotion tracking

# Measuring sentiment of the articles (based on bing lexicon)
article_sentiment <- combined_news %>%
  unnest_tokens(word, cleaned_body) %>%
  inner_join(sentiment_words_bing, by = "word") %>%
  count(publisher, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)

ggplot(article_sentiment, aes(x = publisher, y = net_sentiment, fill = publisher)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Net Sentiment of Articles by Publisher",
       y = "Net Sentiment (Positive - Negative)", x = "Publisher") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Measuring the sentiment of articles by day
sentiment_by_day <- combined_news %>%
  unnest_tokens(word, cleaned_body) %>%
  inner_join(sentiment_words_bing, by = "word") %>%
  count(pub_date, publisher, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)

# Plot of the Sentiment of The Gaurdian Articles by Day (also volume of articles)
article_counts <- combined_news %>% count(pub_date, publisher, name = "article_count") # Measuring counts of articles
sentiment_by_day <- sentiment_by_day %>% left_join(article_counts, by = c("pub_date", "publisher"))

scale_factor <- max(abs(sentiment_by_day$net_sentiment), na.rm = TRUE) / max(sentiment_by_day$article_count, na.rm = TRUE)
sentiment_by_day <- sentiment_by_day %>% mutate(scaled_count = article_count * scale_factor)

# Plot with facets for each publisher
ggplot(sentiment_by_day, aes(x = pub_date)) +
  geom_col(aes(y = scaled_count), fill = "steelblue", alpha = 0.3) +  # article volume
  geom_line(aes(y = net_sentiment), color = "darkred", linewidth = 1.1) +  # net sentiment
  scale_y_continuous(
    name = "Net Sentiment (Positive - Negative)",
    sec.axis = sec_axis(~ . * scale_factor, name = "Article Volume")
  ) +
  facet_wrap(~ publisher, scales = "free_x") +
  labs(
    title = "Net Sentiment and Article Volume Over Time by Publisher",
    x = "Date"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y.left = element_text(color = "darkred"),
    axis.title.y.right = element_text(color = "steelblue")
  )


# Tracking emotions per day
emotion_by_day <- combined_news %>%
  unnest_tokens(word, cleaned_body) %>%
  inner_join(sentiment_words_nrc, by = "word", relationship = "many-to-many") %>%
  count(pub_date, publisher, sentiment)  # `sentiment` includes joy, anger, fear, etc.

# Choose emotions to track
selected_emotions <- c("anger", "trust", "fear", "joy", "sadness", "anticipation")

# Plot of the Emotions of The Gaurdian Articles by day
emotion_by_day %>%
  filter(sentiment %in% selected_emotions) %>%
  ggplot(aes(x = pub_date, y = n, color = publisher)) +
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

# Run NLP pipeline on all article text
parsed_locations <- spacy_extract_entity(
  combined_news$cleaned_body,
  entity_type = "GPE"
)

# Filtering for entities named as "GPE" (Geo-Political Entity ==> Countries, States, details)
parsed_locations_gpe <- parsed_locations %>% filter(ent_type == "GPE")

parsed_locations_gpe <- parsed_locations_gpe %>%   mutate(text = tolower(text))

parsed_locations_gpe <- parsed_locations_gpe %>%
  mutate(doc_id = str_extract(doc_id, "\\d+")) %>%
  mutate(doc_id = as.numeric(doc_id)) %>%
  left_join(
    combined_news %>%
      mutate(doc_id = row_number()) %>%
      select(doc_id, pub_date, publisher),
    by = "doc_id"
  )

data("us.cities", package = "maps")

us_cities_clean <- us.cities %>%
  mutate(city_clean = tolower(name)) %>%
  mutate(
    parts = str_split(city_clean, "\\s+"),
    state = map_chr(parts, ~ tail(.x, 1)),  # last word as state abbreviation
    city = map_chr(parts, ~ paste(head(.x, -1), collapse = " "))
  ) %>%
  select(city, state, lat, long)

city_matches <- parsed_locations_gpe %>%
  inner_join(us_cities_clean, by = c("text" = "city"), relationship = "many-to-many")


# Aggregate article counts by city and state
city_summary <- city_matches %>%
  count(publisher, text, state, lat, long, name = "article_count")

# Aggregate counts at state level (if needed later)
state_summary <- city_matches %>%
  count(publisher, state, name = "article_count")

# Load US state boundaries
us_states <- map_data("state")

ggplot() +
  geom_polygon(
    data = us_states,
    aes(x = long, y = lat, group = group),
    fill = "gray95", color = "gray80"
  ) +
  geom_point(
    data = city_summary,
    aes(x = long, y = lat, size = article_count, color = publisher),
    alpha = 0.7
  ) +
  coord_fixed(1.3) +
  labs(
    title = "City-Level Article Mentions by Publisher",
    x = "", y = "",
    size = "Article Count",
    color = "Publisher"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

state_summary <- state_summary %>%
  mutate(region = tolower(abbr2state(state)))  # converts "TX" → "texas"

# Prepare state-wise weekly counts by publisher
state_weekly <- city_matches %>%
  mutate(week = floor_date(pub_date, unit = "week")) %>%
  count(publisher, state, week, name = "article_count") %>%
  mutate(region = tolower(abbr2state(state)))  # convert state abbreviations to lowercase state names

# Join with US map data
us_states <- map_data("state")
map_data_weekly <- us_states %>%
  left_join(state_weekly, by = "region", relationship = "many-to-many")

map_data_weekly <- map_data_weekly %>%
  filter(!is.na(week))

# Animated choropleth plot
p_anim <- ggplot(map_data_weekly, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = article_count), color = "white", linewidth = 0.3) +
  coord_fixed(1.3) +
  scale_fill_viridis_c(option = "inferno", direction = -1, na.value = "grey90") +
  facet_wrap(~ publisher) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 10)),
    legend.position = "right"
  ) +
  transition_time(week) +
  labs(
    title = "Weekly Article Mentions by State",
    subtitle = "Week of: {frame_time}",
    fill = "Article Count"
  ) +
  ease_aes("linear")

# Export as animated GIF
animate(p_anim,
        width = 1000,
        height = 500,
        duration = 10,
        fps = 5,
        renderer = gifski_renderer("animated_map.gif"))





