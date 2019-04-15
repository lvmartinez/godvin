library(tidyverse)
source("vivino.R")

base_url = "https://api.etilbudsavis.dk/v2/offers/search?r_lat={r_lat}&r_lng={r_lng}&r_radius={r_radius}&query={query}&offset={offset}&limit={limit}"

url = glue::glue(base_url, query = "vin",
           r_lat =  55.6656223,
           r_lng =  12.6009489,
           r_radius = 4000,
           offset = 0,
           limit = 100)

data  =  jsonlite::fromJSON(url)

vindata = tibble(
  heading = data$heading,
  description = data$description,
  price = data$pricing$price,
  size = data$quantity$size$from,
  symbol = data$quantity$unit$symbol,
  pieces = data$quantity$pieces$from,
  store = data$branding$name,
  image = data$images$zoom
) 

pb = progress::progress_bar$new(total = length(vindata$heading))

vivino = map_dfr(vindata$heading, function(q){
  pb$tick()
  get_vivinfo(q)
})

vindata = vindata %>%
  mutate(size_cl = ifelse(symbol == "cl", size, size * 100)) %>%
  mutate(prize_cl = round(price / (pieces * size_cl),2)) %>%
  mutate(image = glue::glue('<a href="{image}" target="_blank"><img border="0" src="{image}" height="100" width="100"></a>')) %>%
  mutate(heading = str_split(heading, ",|eller")) %>%
  unnest() %>%
  select(heading, store, prize_cl, image) %>%
  mutate(heading = str_squish(heading))

vivino = vivino %>%
  mutate(wine_rating = as.numeric(wine_rating),
         wine_price =  as.numeric(wine_price),
         wine_no_ratings = as.numeric(wine_no_ratings))

df = bind_cols(vindata, vivino) %>%
  mutate(rating_cl = round(wine_rating / prize_cl, 2)) %>%
  mutate(wine_image = glue::glue('<a href="{wine_url}" target="_blank"><img border="0" src="http://{wine_image}" height="100" width="100"></a>')) %>%
  select(heading, store, wine_rating, wine_no_ratings, prize_cl, rating_cl, image, wine_image)

  
df %>%
  arrange(desc(wine_rating)) %>%
  DT::datatable(escape = FALSE, rownames = F)

