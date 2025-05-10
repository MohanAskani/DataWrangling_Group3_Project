# Custom cleaning function (For Guardian Articles)
clean_articles_guardian <- function(df) {
  df %>%
    mutate(
      cleaned_body = map_chr(body, function(raw_text) {
        if (is.null(raw_text) || is.na(raw_text) || !is.character(raw_text)) return("")
        
        cleaned <- raw_text %>%
          read_html() %>%
          html_text2() %>%
          str_to_lower() %>%
          str_replace_all("\\b(\\w+)(['’]s)\\b", "\\1") %>%
          str_replace_all("\\b\\w*\\d+\\w*\\b", "") %>%
          str_replace_all("[[:punct:]]", " ") %>%
          str_squish()
        
        return(cleaned)
      })
    )
}


