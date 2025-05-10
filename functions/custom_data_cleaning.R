
# Custom cleaning function (For Guardian Articles)
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

# Full body cleaning function starts (For NY Times Articles)
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
