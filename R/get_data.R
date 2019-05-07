
source(here::here("R", "didnt_start_it.R"))

lots_o_fires <-
  get_seed_tweets(
    n_tweets = 3000,
    output_path = here("data", "raw", "lots_o_fires.csv"),
    write_out = TRUE
  )

addresses <-
  pull_addresses(lots_o_fires)

lat_long <-
  get_lat_long(addresses)

fire_sums <-
  lat_long %>%
  count_fires()

write_csv(lots_o_fires, here("data", "raw", "lots_o_fires.csv"))
write_csv(addresses, here("data", "derived", "addresses.csv"))
write_csv(lat_long, here("data", "derived", "lat_long.csv"))
write_csv(fire_sums, here("data", "derived", "fire_sums.csv"))

graph_fire_times(lat_long)
plot_fire_sums(fire_sums)
