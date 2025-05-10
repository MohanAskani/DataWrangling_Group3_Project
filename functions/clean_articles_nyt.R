
clean_articles_nyt <- function(df) {
  df %>%
    mutate(
      cleaned_body = map_chr(raw_body, function(text) {
        if (is.null(text) || is.na(text) || !is.character(text)) return("")
        cleaned <- text %>%
          gsub("(?:'|’)[sS]\\b", "", .) %>%
          gsub("We are having trouble retrieving the article content\\..*?(Log in\\.|Subscribe\\.|Advertisement)", "", ., perl = TRUE) %>%
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
      })
    )
}
