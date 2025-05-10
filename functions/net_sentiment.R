compute_net_sentiment_plot <- function(combined_news) {
  require(tidytext)
  require(dplyr)
  require(ggplot2)
  
  # Load Bing sentiment lexicon
  sentiment_words_bing <- get_sentiments("bing")
  
  # Compute sentiment counts and net sentiment
  article_sentiment <- combined_news %>%
    unnest_tokens(word, cleaned_body) %>%
    inner_join(sentiment_words_bing, by = "word") %>%
    count(publisher, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(net_sentiment = positive - negative)
  
  # Create bar plot
  sentiment_plot <- ggplot(article_sentiment, aes(x = publisher, y = net_sentiment, fill = publisher)) +
    geom_col(show.legend = FALSE) +
    labs(title = "Net Sentiment of Articles by Publisher",
         y = "Net Sentiment (Positive - Negative)", x = "Publisher") +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(sentiment_plot)
}
