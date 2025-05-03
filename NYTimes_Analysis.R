#Clear R Environment
rm(list = ls())

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


nyt_key <- "3MBRKQHHWDYuuAeX9QpkVd95XlrPBjCR"

system('curl -s -o nyt-2025-4.json "https://api.nytimes.com/svc/archive/v1/2025/4.json?api-key=3MBRKQHHWDYuuAeX9QpkVd95XlrPBjCR"')


# Adjust path as needed
json_data <- fromJSON("nyt-2025-4.json", flatten = TRUE)

# Extract articles
docs <- json_data$response$docs

glimpse(docs)

dim(docs)

us_news <- docs %>% filter(section_name %in% c('U.S.', 'Sports') & type_of_material == 'News')
dim(us_news)

body_extraction <- function(url) {
  page <- read_html(url)
  paragraphs <- page %>% html_elements("p") %>% html_text2()
  clean_text <- paste(paragraphs, collapse = " ")
  return(clean_text)
}

us_news <- us_news %>% mutate(full_body = map_chr(web_url, body_extraction))

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

us_news <- us_news %>% mutate(cleaned_body = map_chr(full_body, clean_full_body))
glimpse(us_news)
str(us_news)
cat(us_news$full_body[340])
cat(us_news$cleaned_body[340])
summary(nytimes_processed$pub_date)  # Should show proper Date format



# Select and rename the desired columns
nytimes_processed <- us_news %>%
  select(
    abstract,
    pub_date,
    publisher = source,        # Use source column as publisher (assuming it's "The New York Times")
    raw_body = full_body,      # Rename full_body to raw_body
    cleaned_body               # Keep cleaned_body as is
  ) %>%
  mutate(
    pub_date = as.Date(sub("T.*", "", pub_date)),
    # Ensure publisher is consistently labeled
    publisher = "The New York Times"
  )

dim(nytimes_processed)
# Save to CSV if needed
write_csv(nytimes_processed, "nytimes_processed.csv")

tokenisation_standardisation_unigrams <- function(df){
  df <- df %>% 
    unnest_tokens(word, cleaned_body) %>% # Tokenization of the cleaned text of body
    filter(str_length(word) >= 4) %>% # Removing words with length < 4
    anti_join(stop_words, by = "word") %>% # Handling stopwords
    filter(str_detect(word, "^[a-z]+$")) %>%  # considering only alphabets
    mutate(lemma = lemmatize_words(word)) # Reducing the words to its base form
}

nytimes_data_tokenized <- tokenisation_standardisation_unigrams(nytimes_processed)

