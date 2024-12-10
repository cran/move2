## ----include = FALSE--------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(sf_max_print = 5L)
if (Sys.info()["user"] != "bart") {
  if (Sys.getenv("MBPWD") != "") {
    options(keyring_backend = "env")
    move2::movebank_store_credentials("move2_user", Sys.getenv("MBPWD"))
  } else {
    knitr::opts_chunk$set(eval = FALSE)
  }
}

## ----setup, message=FALSE---------------------------------------------------------
library(move2)
library(dplyr)
library(units)
library(sf)

## ---------------------------------------------------------------------------------
galapagos_albatrosses <- movebank_download_study(2911040,
  attributes = c(
    "ground_speed",
    "heading",
    "height_above_ellipsoid",
    "eobs_temperature",
    "individual_local_identifier"
  )
) %>%
  select_track_data(study_site, weight, animal_life_stage)

## ---------------------------------------------------------------------------------
galapagos_albatrosses %>%
  filter(!st_is_empty(.))

## ---------------------------------------------------------------------------------
galapagos_albatrosses %>%
  filter(!st_is_empty(.)) %>%
  mt_filter_per_interval(unit = "6 hours")

## ---------------------------------------------------------------------------------
galapagos_albatrosses %>%
  filter(!st_is_empty(.)) %>%
  mt_filter_per_interval(criterion = "random", unit = "days")

## ---------------------------------------------------------------------------------
galapagos_albatrosses %>%
  group_by(mt_time(), mt_track_id()) %>%
  filter(n() != 1) %>%
  arrange(mt_time())

## ---------------------------------------------------------------------------------
galapagos_albatrosses %>%
  filter(!st_is_empty(.)) %>%
  group_by(mt_time(), mt_track_id()) %>%
  filter(n() != 1) %>%
  arrange(mt_time())

## ---------------------------------------------------------------------------------
simulated_data <- mt_sim_brownian_motion(1:2)[rep(1:4, 2), ]
simulated_data$temperature <- c(1:3, NA, 1:2, 7:8)
simulated_data
simulated_data %>% mt_filter_unique()

## ---------------------------------------------------------------------------------
galapagos_albatrosses %>% mt_filter_unique("sample")

## ----download---------------------------------------------------------------------
galapagos_albatrosses %>%
  group_by(mt_track_id()) %>%
  filter(n() > 500)

## ---------------------------------------------------------------------------------
galapagos_albatrosses %>%
  group_by(mt_track_id()) %>%
  filter(as_units(diff(range(mt_time()))) > set_units(1, "week"))

## ----fig.width=7, fig.height=4.2--------------------------------------------------
foraging_area <- st_as_sfc(st_bbox(c(
  xmin = -82, xmax = -77,
  ymax = -0.5, ymin = -13
), crs = 4326))
library(ggplot2, quietly = TRUE)
ggplot() +
  geom_sf(data = rnaturalearth::ne_coastline(returnclass = "sf", 50)) +
  theme_linedraw() +
  geom_sf(data = foraging_area, fill = "red", alpha = 0.3, color = "red") +
  geom_sf(
    data = galapagos_albatrosses %>% filter(!st_is_empty(.)),
    aes(color = `individual_local_identifier`)
  ) +
  coord_sf(
    crs = sf::st_crs("+proj=aeqd +lon_0=-83 +lat_0=-6 +units=km"),
    xlim = c(-1000, 600), ylim = c(-800, 700)
  )
# Filter to tracks making it at least once to the foraging area
galapagos_albatrosses %>%
  group_by(mt_track_id()) %>%
  filter(any(st_intersects(geometry, foraging_area, sparse = FALSE)))

## ---------------------------------------------------------------------------------
galapagos_albatrosses %>%
  filter_track_data(study_site == "Punta Suarez")

## ---------------------------------------------------------------------------------
galapagos_albatrosses %>%
  filter(!st_is_empty(.)) %>%
  mutate(
    next_new_track = mt_time_lags(.) > set_units(4, "h") |
      is.na(mt_time_lags(.)),
    track_index = cumsum(lag(next_new_track, default = FALSE))
  ) %>%
  mt_set_track_id("track_index")

## ---------------------------------------------------------------------------------
library(lubridate, quietly = TRUE)
galapagos_albatrosses %>%
  mt_set_track_id(paste(mt_track_id(.),
    sep = "_", month.name[month(mt_time(.))]
  ))

