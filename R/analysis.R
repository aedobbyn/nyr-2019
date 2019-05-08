
source(here::here("R", "didnt_start_it.R"))

addresses <- read_csv(here("data", "derived", "addresses.csv"))
lat_long <- read_csv(here("data", "derived", "lat_long.csv"))

# Fires by borough
fires_by_borough <- 
  addresses %>% 
  drop_na(borough) %>% 
  group_by(borough) %>% 
  count(sort = TRUE) 

borough_population_raw <-
  "https://www.citypopulation.de/php/usa-newyorkcity.php" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("table") %>% 
  rvest::html_table() %>% 
  purrr::pluck(1) %>% 
  as_tibble(.name_repair = "minimal") %>% 
  select(1, (ncol(.) - 1))

names(borough_population_raw)[2] <- "population"

borough_pop <- 
  borough_population_raw %>% 
  rename(
    name = Name
  ) %>% 
  rowwise() %>% 
  mutate(
    name = name %>% 
      str_remove(" \\(.+\\)") %>% 
      str_trim() %>% 
      clean_borough(),
    population = population %>% 
      str_remove_all(",") %>% 
      as.numeric()
  ) %>% 
  slice(-nrow(.))


fires_by_borough %>% 
  left_join(borough_pop, by = c("borough" = "name")) %>% 
  mutate(
    fires_per_person = n/population
  ) %>% 
  arrange(desc(fires_per_person))


dat <- 
  addresses %>% 
  drop_na(address) %>% 
  mutate(
    day = lubridate::wday(created_at, label = TRUE),
    hour = lubridate::hour(created_at)
  ) 

dat %>% 
  group_by(day) %>% 
  count(sort = TRUE)

dat %>% 
  group_by(hour) %>% 
  count(sort = TRUE) %>% 
  ggplot() +
  aes(hour, n) +
  geom_bar(stat = "identity")








