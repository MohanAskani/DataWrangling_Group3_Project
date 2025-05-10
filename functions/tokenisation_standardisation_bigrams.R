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
  
  # Topic Coverage Analysis based on Bi-grams
  top_bigrams <- df %>%
    count(publisher, bigram, sort = TRUE) %>%
    group_by(publisher) %>%
    slice_max(n, n = 15) %>%
    ungroup()
  
  # Visualization of top words from the news articles
  top_bigrams_plot <- top_bigrams %>%
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
  return(top_bigrams_plot)
}

