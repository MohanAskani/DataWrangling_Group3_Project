
compute_city_plot <- function(combined_news) {
  require(tidytext)
  require(dplyr)
  require(ggplot2)
  require(spacyr)
  
  # Run NLP pipeline on all article text
  parsed_locations <- spacy_extract_entity(
    combined_news$cleaned_body,
    entity_type = "GPE"
  )
  
  # Filtering for entities named as "GPE" (Geo-Political Entity ==> Countries, States, details)
  parsed_locations_gpe <- parsed_locations %>% filter(ent_type == "GPE")
  
  parsed_locations_gpe <- parsed_locations_gpe %>%   mutate(text = tolower(text))
  
  parsed_locations_gpe <- parsed_locations_gpe %>%
    mutate(doc_id = str_extract(doc_id, "\\d+")) %>%
    mutate(doc_id = as.numeric(doc_id)) %>%
    left_join(
      combined_news %>%
        mutate(doc_id = row_number()) %>%
        select(doc_id, pub_date, publisher),
      by = "doc_id"
    )
  
  data("us.cities", package = "maps")
  
  us_cities_clean <- us.cities %>%
    mutate(city_clean = tolower(name)) %>%
    mutate(
      parts = str_split(city_clean, "\\s+"),
      state = map_chr(parts, ~ tail(.x, 1)),  # last word as state abbreviation
      city = map_chr(parts, ~ paste(head(.x, -1), collapse = " "))
    ) %>%
    select(city, state, lat, long)
  
  city_matches <- parsed_locations_gpe %>%
    inner_join(us_cities_clean, by = c("text" = "city"), relationship = "many-to-many")
  
  
  # Aggregate article counts by city and state
  city_summary <- city_matches %>%
    count(publisher, text, state, lat, long, name = "article_count")
  
  # Aggregate counts at state level (if needed later)
  state_summary <- city_matches %>%
    count(publisher, state, name = "article_count")
  
  # Load US state boundaries
  us_states <- map_data("state")
  
  city_plot <- ggplot() +
                geom_polygon(
                  data = us_states,
                  aes(x = long, y = lat, group = group),
                  fill = "gray95", color = "gray80"
                ) +
                geom_point(
                  data = city_summary,
                  aes(x = long, y = lat, size = article_count, color = publisher),
                  alpha = 0.7
                ) +
                coord_fixed(1.3) +
                labs(
                  title = "City-Level Article Mentions by Publisher",
                  x = "", y = "",
                  size = "Article Count",
                  color = "Publisher"
                ) +
                theme_minimal(base_size = 13) +
                theme(
                  plot.title = element_text(hjust = 0.5, face = "bold"),
                  legend.position = "right"
                )
  
  return(city_plot)
  
}