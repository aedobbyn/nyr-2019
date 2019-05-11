
# source(here::here("R", "didnt_start_it.R"))

addresses <- read_csv(here("data", "derived", "addresses.csv"))
lat_long <- read_csv(here("data", "derived", "lat_long.csv"))

# Fires by borough
fires_by_borough <-
  addresses %>%
  drop_na(borough) %>%
  group_by(borough) %>%
  count(sort = TRUE)

# Borough populations
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


# N fires by borough
fires_by_borough_per_cap <- 
  fires_by_borough %>%
  left_join(borough_pop, by = c("borough" = "name")) %>%
  mutate(
    fires_per_person = n / population
  ) %>%
  arrange(desc(fires_per_person))


clean_hour <- function(x) {
  if (x == 0) {
    "Midnight"
  } else if (x == 12) {
    "Noon"
  } else if (x < 13) {
    str_c(x, " am")
  } else {
    str_c(x - 12, " pm")
  }
}

# Fires by day and hour
dat <-
  addresses %>%
  drop_na(address) %>%
  mutate(
    date = lubridate::date(created_at),
    month = lubridate::month(created_at),
    month_label = lubridate::month(created_at, label = TRUE),
    wday = lubridate::wday(created_at, label = TRUE),
    hour = lubridate::hour(created_at),
    hour_label = map_chr(hour, clean_hour)
  ) %>% 
  # Just take a year
  filter(
    date <= min(date) + 356
  )

# Fires by month
by_month <- 
  dat %>%
  group_by(month, month_label) %>%
  count(sort = TRUE)

by_month %>%
  ggplot() +
  aes(month, n) +
  geom_bar(stat = "identity", alpha = 0.5) +
  labs(x = "Month of Year", y = "N Fires") +
  ggtitle("Fires by Month") +
  scale_x_continuous(
    breaks = 1:12,
    labels = lubridate::month(1:12, label = TRUE)
  ) +
  hrbrthemes::theme_ipsum_ps()


# Fires by day
by_day <- 
  dat %>%
  group_by(wday) %>%
  count(sort = TRUE)

by_day %>%
  ggplot() +
  aes(wday, n) +
  geom_bar(stat = "identity", alpha = 0.5) +
  labs(x = "Day of Week", y = "N Fires") +
  ggtitle("Fires by Day") +
  hrbrthemes::theme_ipsum_ps()



# Fires by hour
by_hour <-
  dat %>%
  group_by(hour, hour_label) %>%
  count(sort = TRUE)

hour_label_dict <-
  by_hour %>%
  arrange(hour) %>%
  mutate(
    major =
      case_when(
        hour %% 3 == 0 ~ TRUE,
        TRUE ~ FALSE
      )
  )

by_hour %>%
  ggplot() +
  aes(hour, n) +
  geom_bar(stat = "identity", alpha = 0.5) +
  scale_x_continuous(
    breaks = hour_label_dict %>%
      filter(major) %>%
      pull(hour),
    labels = hour_label_dict %>%
      filter(major) %>%
      pull(hour_label)
  ) +
  geom_smooth(se = FALSE, linetype = "dashed", size = 0.5, method = "loess") +
  geom_vline(xintercept = 6.5, linetype = "dotted") +
  annotate("text", x = 7.5, y = 120, 
           label = "Average \n Sunrise", family = "Arial", size = 2) +
  geom_vline(xintercept = 18.667, linetype = "dotted") +
  annotate("text", x = 19.667, y = 160, 
           label = "Average \n Sunset", family = "Arial", size = 2) +
  labs(x = "Time of Day", y = "N Fires") +
  ggtitle("Fires by Hour") +
  theme_minimal() +
  hrbrthemes::theme_ipsum_ps()

ggsave("fires_by_hour.png", path = here("plots"), device = "png")
ggsave("fires_by_hour.png", path = here("docs", "img"), device = "png")




write_csv(by_day,
          here("data", "derived", "fires_by_day.csv"))
write_csv(fires_by_borough, 
          here("data", "derived", "fires_by_borough.csv"))
write_csv(fires_by_borough_per_cap, 
          here("data", "derived", "fires_by_borough_per_cap.csv"))





# Get average sunrise and sunset times for vlines in Fires by Hour plot

clean_time <- function(x) {
  x %>% 
    str_extract_all(".+[am|pm]") %>% 
    as_vector() 
}

get_mins <- function(df, col = sunset) {
  q_col <- enquo(col)
  
  df %>% 
    separate(
    !!q_col, c("hr", "mn", "am_pm"),
    sep = "[ :]",
    remove = FALSE
  ) %>% 
    mutate(
      hr = as.numeric(hr),
      hr = 
        case_when(
          am_pm == "pm" ~ hr + 12,
          TRUE ~ hr
        ),
      mn = as.numeric(mn),
      total = (hr*60) + mn
    ) %>% 
    select(-am_pm) %>% 
    drop_na()
}
  
get_sun_rise_set <- function(month = 1,
                             base_url = 
                              "https://www.timeanddate.com/sun/usa/new-york?month=",
                             year = 2018,
                             return = "sunrise") {
  
  
  url <- glue("https://www.timeanddate.com/sun/usa/new-york?month={month}&year={year}") 
  
  df <- url %>% 
    xml2::read_html() %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill = TRUE, header = FALSE) %>% 
    purrr::pluck(1) %>% 
    as_tibble(.name_repair = "unique") %>% 
    select(1:3) 
  
  names(df) <- c("day", "sunrise", "sunset")
  
  df <- df %>% 
    slice(4:nrow(.)) %>% 
    mutate(
      sunrise = 
        sunrise %>% 
        map_chr(clean_time),
      sunset = 
        sunset %>% 
        map_chr(clean_time),
      month = month
    ) %>% 
    select(month, day, everything()) 
  
  sunsets <- 
    df %>% 
    select(-sunrise)

  sunsets <- 
    sunsets %>% 
    get_mins(col = sunset)
  
  sunrises <- 
    df %>% 
    select(-sunset) %>% 
    get_mins(col = sunrise)
  
  if (return == "sunrise") {
    sunrises
  } else if (return == "sunset") {
    sunsets
  }
}


all_sunrises <- 
  map_df(1:12, .f = get_sun_rise_set, return = "sunrise")


all_sunsets <- 
  map_df(1:12, .f = get_sun_rise_set, return = "sunset")


summarise_sun_rise_set <- function(tbl) {
  tbl %>% 
    summarise(
      mean_mins = mean(total, na.rm = TRUE)
    ) %>% 
    mutate(
      hr = (mean_mins/60) %>% floor(), 
      mn = mean_mins - (hr*60)
    )
}

all_sunrises %>% summarise_sun_rise_set()
all_sunsets %>% summarise_sun_rise_set()
  

  

