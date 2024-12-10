params <-
list(eval_output = TRUE)

## ----set_width, echo=FALSE, eval=params$eval_output-------------------------------
options(width = 84)
options(sf_max_print = 3)
options(tibble.max_extra_cols = 5)

knitr::opts_chunk$set(
  collapse = TRUE, fig.width = 7, fig.height = 4,
  comment = "#>", fig.align = "center"
)


knitr::opts_chunk$set(
  gganimate = list(
    nframes = 30,
    fps = 5
  )
)

if (Sys.info()["user"] != "bart") {
  if (Sys.getenv("MBPWD") != "") {
    options(keyring_backend = "env")
    move2::movebank_store_credentials("move2_user", Sys.getenv("MBPWD"))
  } else {
    warning("Evaluation of movebank download not possible")
    knitr::opts_chunk$set(eval = FALSE)
  }
}

## ----load_move--------------------------------------------------------------------
library(move2)

## ----example_store_cred, eval=FALSE-----------------------------------------------
#  movebank_store_credentials("username")

## ----download_example-------------------------------------------------------------
data <- movebank_download_study("Galapagos Albatrosses", sensor_type_id = "gps")
data

## ----preload_raster, echo=FALSE, include=FALSE------------------------------------
# preload rnatural earth to prevent messaging
library(ggplot2)
library(raster)

## ----map_albatrosses, fig.width=5.3-----------------------------------------------
library(ggplot2)
ggplot() +
  ggspatial::annotation_map_tile(zoom = 5) +
  ggspatial::annotation_scale() +
  theme_linedraw() +
  geom_sf(data = data, color = "darkgrey", size = 1) +
  geom_sf(data = mt_track_lines(data), aes(color = individual_local_identifier)) +
  coord_sf(
    crs = sf::st_crs("+proj=aeqd +lon_0=-83 +lat_0=-6 +units=km"),
    xlim = c(-1000, 600),
    ylim = c(-800, 700)
  ) +
  guides(color = "none")

## ----preload_spatial, echo=FALSE, include=FALSE-----------------------------------
# preload rnatural earth to prevent messaging
library(gganimate)
library(ggspatial)

## ----island_animation, eval=params$eval_output &     knitr::opts_chunk$get("eval"), fig.width=5.3, results='hide'----
require(gganimate)
require(ggspatial)
animation_site <- ggplot() +
  annotation_map_tile(zoom = 5, progress = "none") +
  geom_sf(
    data = mt_track_lines(data),
    mapping = aes(group = individual_local_identifier),
    color = "black"
  ) +
  transition_states(study_site, state_length = 2) +
  enter_fade() +
  exit_fade() +
  ease_aes("cubic-in-out") +
  labs(title = "{closest_state}") +
  annotation_scale()
animation_site

## ----island_animation_render ,echo=FALSE, results='asis'--------------------------
# animate() doesn't seem to put the images in the right place for pkgdown, so this is a manual workaround
anim_save("island_animation.gif", animation = animation_site)
cat("![](island_animation.gif)\n")

## ----change_view, eval=params$eval_output &     knitr::opts_chunk$get("eval"), fig.width=5.3, results='hide'----
animation_site +
  coord_sf(
    crs = sf::st_crs("+proj=aeqd +lon_0=-83 +lat_0=-6 +units=km")
  ) +
  view_zoom_manual(
    pause_length = 2,
    xmin = c(-850, -900, -950),
    ymin = c(-350, -800, -350),
    ymax = c(610, 700, 610),
    xmax = c(500, 600, 500)
  )

## ----change_view_render ,echo=FALSE, results='asis'-------------------------------
# animate() doesn't seem to put the images in the right place for pkgdown, so this is a manual workaround
anim_save("change_view.gif", renderer = gifski_renderer())
cat("![](change_view.gif)\n")

## ----movement_animation, eval=params$eval_output &     knitr::opts_chunk$get("eval"), results='hide'----
data_interpolated <- mt_interpolate(
  data[!sf::st_is_empty(data), ],
  time = seq(
    as.POSIXct("2008-7-27"),
    as.POSIXct("2008-8-1"), "60 mins"
  ),
  max_time_lag = units::as_units(3, "hours"),
  omit = TRUE
)
animation <- ggplot() +
  annotation_map_tile(zoom = 5, progress = "none") +
  annotation_scale() +
  theme_linedraw() +
  geom_sf(
    data = data_interpolated, size = 3,
    aes(color = individual_local_identifier)
  ) +
  transition_manual(timestamp) +
  labs(
    title = "Galapagos Albatrosses",
    subtitle = "Time: {current_frame}",
    color = "Individual"
  )
animate(animation,
  nframes = length(unique(data_interpolated$timestamp))
)

## ----movement_animation_render ,echo=FALSE, results='asis'------------------------
# animate() doesn't seem to put the images in the right place for pkgdown, so this is a manual workaround
anim_save("movement_animation.gif", renderer = gifski_renderer())
cat("![](movement_animation.gif)\n")

## ----movement_animation_tail------------------------------------------------------
date_range <- as.POSIXct(c("2008-7-29", "2008-8-1"))
ts <- mt_time(data)
times <- sort(unique((c(date_range, ts[ts < max(date_range) & ts > min(date_range)]))))
data_interpolated <-
  mt_interpolate(
    data[!sf::st_is_empty(data), ],
    times,
    omit = TRUE
  )
label_df <- data.frame(
  timestamp = date_range,
  display_time = lubridate::with_tz(date_range, "America/Lima")
)
animation <- ggplot() +
  annotation_map_tile("cartodark", zoom = 5, progress = "none") +
  annotation_scale(bar_cols = c("gray80", "gray40"), text_col = "gray80") +
  geom_sf(data = mt_track_lines(data), color = "grey40") +
  theme_linedraw() +
  geom_sf(
    data = data_interpolated, size = 3,
    aes(color = individual_local_identifier)
  ) +
  scale_color_brewer(palette = "Set1") +
  guides(color = "none") +
  xlab("") +
  ylab("") +
  geom_text(
    data = label_df,
    aes(label = display_time, x = -10100000, y = -1370000),
    color = "grey80", size = 3, hjust = 0
  ) +
  transition_time(timestamp) +
  shadow_wake(0.2, exclude_layer = 6)

## ----movement_animation_tail_show, results='hide', eval=params$eval_output &     knitr::opts_chunk$get("eval"), fig.width=5.3----
animate(animation,
  nframes = 3 * 24 * 2 + 1, detail = 5
)

## ----movement_animation_tail_show_render ,echo=FALSE, results='asis'--------------
# animate() doesn't seem to put the images in the right place for pkgdown, so this is a manual workaround
anim_save("movement_animation_tail_show.gif", renderer = gifski_renderer())
cat("![](movement_animation_tail_show.gif)\n")

## ----start_advance, message=FALSE-------------------------------------------------
require(units)
require(dplyr)
require(sf)
data <- movebank_download_study("Galapagos Albatrosses",
  sensor_type_id = c("gps", "acceleration")
)
data <- data %>%
  filter_track_data(deployment_comments != "not used in analysis")

## ----interpolate_empty------------------------------------------------------------
data <- data[order(mt_track_id(data), mt_time(data)), ]
data <- mt_interpolate(data)

## ----preload, echo=FALSE, include=FALSE-------------------------------------------
# preload rnatural earth to prevent messaging
library(rnaturalearth)

## ----intersection-----------------------------------------------------------------
library(rnaturalearth)
breeding_area <- st_buffer(mt_track_data(data)$deploy_on_location, as_units(25, "km")) %>%
  st_union()
foraging_area <- ne_countries(110,
  returnclass = "sf",
  continent = "South America"
) %>%
  st_union() %>%
  st_buffer(as_units(100, "km"))
regions <- st_sf(data.frame(
  region = c("Breeding", "Foraging"),
  polygon = c(breeding_area, foraging_area)
))
data <- st_join(data, regions)

## ----recode_region_change---------------------------------------------------------
data <- data %>%
  group_by(mt_track_id(.)) %>%
  mutate(
    region_change = paste(
      vctrs::vec_fill_missing(region),
      vctrs::vec_fill_missing(region, "up")
    ),
    region = case_match(region_change,
      "Foraging Breeding" ~ "Inbound",
      "Breeding Foraging" ~ "Outbound",
      "Breeding Breeding" ~ "Breeding",
      "Foraging Foraging" ~ "Foraging",
      .default = region
    )
  )

## ----change_track-----------------------------------------------------------------
data <- data %>%
  mutate(
    sequence_number = with(rle(region), rep(seq_along(lengths), lengths)),
    track = paste(individual_local_identifier, region, sequence_number)
  ) %>%
  ungroup() %>%
  mutate_track_data(individual = droplevels(individual_local_identifier)) %>%
  mt_set_track_id("track") %>%
  filter(!is.na(region))

## ----expenditure_acc--------------------------------------------------------------
acc_to_dba <- function(x) {
  acc_mat <- matrix(as.numeric(unlist(strsplit(x, " "))), nrow = 2)
  mean(colSums(abs(acc_mat - rowMeans(acc_mat))))
}
data$dba <- unlist(lapply(data$eobs_accelerations_raw, acc_to_dba))

## ----track_summary, fig.width=7---------------------------------------------------
track_summary <- data %>%
  mt_track_lines(
    region = unique(region), n = dplyr::n(), start = min(timestamp),
    end = max(timestamp),
    across(
      all_of(c("ground_speed", "dba")),
      list(
        mean = function(x) mean(x, na.rm = TRUE),
        sd = function(x) sd(x, na.rm = TRUE)
      )
    )
  ) %>%
  mutate(duration = as_units(end - start))

## ----tab;e------------------------------------------------------------------------
table(track_summary$individual, track_summary$region)

## ----track_map--------------------------------------------------------------------
ggplot(track_summary) +
  geom_sf(data = ne_coastline(returnclass = "sf", 50)) +
  geom_sf(aes(color = region)) +
  theme_linedraw() +
  coord_sf(
    crs = st_crs("+proj=aeqd +lon_0=-83 +lat_0=-6 +units=km"),
    xlim = c(-1000, 600),
    ylim = c(-800, 700)
  ) +
  labs(color = "Region") +
  scale_color_brewer(type = "qual")

## ----plots_duration,  fig.width=6-------------------------------------------------
ggplot(track_summary, aes(x = region, y = duration)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = individual, group = individual),
    position = position_jitterdodge()
  ) +
  xlab("") +
  scale_y_units("Duration", unit = "days", trans = "log10") +
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5)) +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5)) +
  scale_color_brewer("Individual", type = "qual", palette = "Set1")

## ----plots_speed,  fig.width=6----------------------------------------------------
ggplot(
  track_summary,
  aes(x = region, y = ground_speed_mean)
) +
  theme_linedraw() +
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = individual, group = individual),
    position = position_jitterdodge()
  ) +
  xlab("") +
  ylab("Mean ground speed") +
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5)) +
  scale_color_brewer("Individual", type = "qual", palette = "Set1")

## ----plot_expend,  fig.width=6----------------------------------------------------
ggplot(
  track_summary,
  aes(x = region, y = dba_mean)
) +
  theme_linedraw() +
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = individual, group = individual),
    position = position_jitterdodge()
  ) +
  ylab("Mean DBA") +
  xlab("") +
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5)) +
  scale_color_brewer("Individual", type = "qual", palette = "Set1")

