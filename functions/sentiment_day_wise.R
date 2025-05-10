compute_sentiment_daywise_plot <- function(combined_news) {
  require(tidytext)
  require(dplyr)
  require(ggplot2)
  
  # Load Bing sentiment lexicon
  sentiment_words_bing <- get_sentiments("bing")
  
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
  
  # Create bar plot
  sentiment_daywise_plot <- ggplot(sentiment_by_day, aes(x = pub_date)) +
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
  
  return(sentiment_daywise_plot)
}
