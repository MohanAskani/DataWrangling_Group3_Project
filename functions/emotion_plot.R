compute_emotion_plot <- function(combined_news) {
  require(tidytext)
  require(dplyr)
  require(ggplot2)
  
  # Tracking emotions per day
  emotion_by_day <- combined_news %>%
    unnest_tokens(word, cleaned_body) %>%
    inner_join(sentiment_words_nrc, by = "word", relationship = "many-to-many") %>%
    count(pub_date, publisher, sentiment)  # `sentiment` includes joy, anger, fear, etc.
  
  # Choose emotions to track
  selected_emotions <- c("anger", "trust", "fear", "joy", "sadness", "anticipation")
  
  # Plot of the Emotions of The Gaurdian Articles by day
  emotion_plot <- emotion_by_day %>%
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
  
  return(emotion_plot)
}
