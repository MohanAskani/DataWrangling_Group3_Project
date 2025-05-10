# Tokenization and Normalization (unigrams)
tokenisation_standardisation_unigrams <- function(df){
  df <- df %>% 
    unnest_tokens(word, cleaned_body) %>% # Tokenization of the cleaned text of body
    filter(str_length(word) >= 4) %>% # Removing words with length < 4
    anti_join(stop_words, by = "word") %>% # Handling stopwords
    filter(str_detect(word, "^[a-z]+$")) %>%  # considering only alphabets
    mutate(lemma = lemmatize_words(word)) # Reducing the words to its base form
  
  # Topic Coverage Analysis based on Uni-grams
  top_unigrams <- df %>%
    count(publisher, lemma, sort = TRUE) %>%
    group_by(publisher) %>%
    slice_max(n, n = 15) %>%
    ungroup()
  
  top_unigrams_plot <- top_unigrams %>%
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
  
  return(top_unigrams_plot)
}