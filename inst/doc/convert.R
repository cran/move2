## ----include = FALSE--------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", echo = FALSE
)
suppressPackageStartupMessages(library(dplyr))

## ----tosort-----------------------------------------------------------------------
tribble(
  ~move, ~move2, ~`Note/comment`,
  "`angle()`", "`mt_azimuth()`", "",
  "`trackId()`", "`mt_track_id()`", "",
  "`timestamps()`", "`mt_time()`", "",
  "`timestamps()<-`", "`mt_set_time()`", "",
  "`timeLag()`", "`mt_time_lags()`", "",
  "`distance()`, `seglength()`", "`mt_distance()`", "",
  "`speed()`", "`mt_speed()`", "",
  "`moveStack()`", "`mt_stack()`", "",
  "`n.indiv()`", "`mt_n_tracks()`", "",
  "`n.locs()`", "`nrow()`/`table(mt_track_id())`", "",
  "`idData()`", "`mt_track_data()`", "",
  "`idData()<-`", "`mt_set_track_data()`", "",
  "`turnAngleGc()`", "`mt_turnangle()`", "",
  "`unUsedRecords()`", "`x[sf::st_is_empty(x),]`", "",
  "`namesIndiv()`", "`unique(mt_track_id())`", "",
  "`getDuplicatedTimestamps()`", "`mt_is_time_ordered(..., non_zero = TRUE)`",
  "This is not a perfect replacement, but atleast gives a warning where duplicated times occur.",
  "`citations()`, `citations()<-`", "`mt_track_data()$citation`, `mutate_track_data(x, citation=...)`",
  "Citations are now a track properties, this helps when combining studies.",
  "`licenseTerms()`, `licenseTerms()<-`",
  paste0(
    "`mt_track_data()$license_type`, `mutate_track_data(x, license_type=...)`, `mt_track_data()$license_terms`,",
    " `mutate_track_data(x, license_terms=...)`"
  ),
  paste(
    "Licenses are not tracked per object separate anymore, rather they are considered a track attribute.",
    "This should facilitate more easly traceing what study has what license."
  ),
  "`split()`", "`split(x, mt_track_id(x))`", "",
  "`plot()`", "`plot()`, `plot(mt_track_lines())`", "",
  "`equalProj()`", "`sf::st_crs()==sf::st_crs()`", "",
  "`burst()`, `burstId()`, `burstId()<-`, `plotBursts()`", "",
  paste(
    "Currently bursting is not explicit functionality of the package, `group_by` might replace some functionality",
    "that is however grouping per point and not per segment"
  ),
  "`show()`", "`print()`", "",
  "`move()`", "`mt_as_move2()`", "",
  "`interpolateTime()`", "`mt_interpolate()`", "",
  "`thinTrackTime()`", "`mt_filter_per_interval()`", "These two functions are not exactly doing the same, the new one subsets the data to a specified time window, but also retains segments with larger timelags"
) %>%
  knitr::kable()

## ----movebank---------------------------------------------------------------------
tribble(
  ~move, ~move2, ~`Note/comment`,
  "`getMovebankStudies()`", "`movebank_download_study_info()`", "",
  "`getMovebankStudy()`", "`movebank_download_study_info(id = ...)`", "",
  "`getMovebankData()`", "`movebank_download_study()`", "",
  "`searchMovebankStudies()`", "", "",
  "`movebankLogin()`",
  "`movebank_store_credentials()`, `movebank_remove_credentials()`, `movebank_handle()`", "",
  "`getMovebank()`", "`movebank_retrieve()`", "",
  "`getMovebankReferenceTable()`", "`movebank_download_deployment()`",
  "",
  "`getMovebankSensors()`",
  '`movebank_retrieve(entity_type="sensor", tag_study_id=...)`, `movebank_retrieve(entity_type="tag_type")`',
  "",
  "getMovebankID()", "`movebank_get_study_id()`", "",
  "`getMovebankAnimals()`",
  '`movebank_download_deployment()`, `movebank_retrieve(entity_type = "individual", study_id = ...)`',
  "",
  "`getMovebankNonLocationData()`",
  "`movebank_download_study(study_id=..., sensor_type_id='...')`",
  "see `movebank_retrieve('tag_type')` for valid `sensor_type_id`",
  "`getMovebankLocationData()`",
  "`movebank_download_study()`, `movebank_retrieve(entity_type='event', study_id=...)`",
  "",
  "`getMovebankSensorsAttributes()`",
  '`movebank_retrieve(entity_type = "study_attribute", study_id=..., sensor_type_id=...)`',
  "see `movebank_retrieve('tag_type')` for valid `sensor_type_id`"
) %>%
  knitr::kable()

## ----UD---------------------------------------------------------------------------
tribble(
  ~move, ~move2, ~`Note/comment`,
  "`UDStack()`", "", "",
  "`getVolumeUD()`", "", "",
  "`contour()`", "", "",
  "`hrBootstrap()`", "", "",
  "`brownian.bridge.dyn()`", "", "",
  "`brownian.motion.variance.dyn()`", "", "",
  "`dynBGB()`", "", "",
  "`dynBGBvariance()`", "", "",
  "`raster2contour()`", "", "",
  "`getMotionVariance()`", "", "",
  "`outerProbability()`", "", ""
) %>%
  knitr::kable()

## ----echo=TRUE--------------------------------------------------------------------
# corridor
# emd
# getDataRepositoryData
# lines
# move2ade
# points
# sensor
# summary
# thinDistanceAlongTrack
# thinTrackTime
# unUsedRecords<-

