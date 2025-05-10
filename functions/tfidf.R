
# generating tf_idf plot for top 15 bigrams
compute_tfidf_plot <- function(combined_news, top_n = 15) {
  require(spacyr)
  require(dplyr)
  require(stringr)
  require(tidytext)
  require(ggplot2)
  require(tidyverse)
  
  # Run spaCy parsing
  parsed_articles <- spacy_parse(combined_news$cleaned_body, pos = TRUE, lemma = FALSE)
  
  # Extract numeric doc_id and map back to publisher
  parsed_articles <- parsed_articles %>%
    mutate(doc_id = str_extract(doc_id, "\\d+"),
           doc_id = as.numeric(doc_id)) %>%
    mutate(publisher = combined_news$publisher[doc_id])
  
  # Filter for meaningful tokens
  meaningful_words <- parsed_articles %>%
    filter(pos %in% c("NOUN", "VERB", "ADJ", "ADV")) %>%
    filter(str_length(token) >= 4, str_detect(token, "^[a-z]+$")) %>%
    anti_join(stop_words, by = c("token" = "word")) %>%
    count(publisher, token, sort = TRUE)
  
  # Compute TF-IDF
  word_tfidf <- meaningful_words %>%
    bind_tf_idf(term = token, document = publisher, n = n) %>%
    arrange(desc(tf_idf))
  
  top_terms <- word_tfidf %>%
    group_by(publisher) %>%
    slice_max(tf_idf, n = top_n) %>%
    ungroup()
  
  # Generate plot
  tfidf_plot <- ggplot(top_terms, aes(x = reorder_within(token, tf_idf, publisher),
                                      y = tf_idf, fill = publisher)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ publisher, scales = "free") +
    coord_flip() +
    scale_x_reordered() +
    labs(title = "Top Thematic Words by Publisher (TF-IDF)",
         x = "Word", y = "TF-IDF Score") +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(tfidf_plot)
}
