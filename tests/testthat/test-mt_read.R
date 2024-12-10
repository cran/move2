test_that("read move works", {
  expect_identical(mt_read(mt_example()) |> dim(), c(47347L, 21L))
  expect_error(mt_read(I("asdf\n3")), "Not all columns that are expected are present in the file")
})
test_that("multiple deployments", {
  txt <- paste0(
    "event-id,visible,timestamp,location-long,location-lat,sensor-type,individual-taxon-canonical-name",
    ',tag-local-identifier,individual-local-identifier,study-name
  26735128471,true,2020-01-01 00:00:00.000,1.0,1.0,"gps",,"w","b","test study bart"
  26735128472,true,2020-02-01 00:00:00.000,2.0,2.0,"gps",,"w","b","test study bart"
  26735128475,true,2020-05-01 00:00:00.000,5.0,5.0,"gps",,"w","b","test study bart"
  26735128476,true,2020-06-01 00:00:00.000,6.0,6.0,"gps",,"w","b","test study bart"
  26735128473,true,2020-03-01 00:00:00.000,3.0,3.0,"gps",,"r","b","test study bart"
  26735128474,true,2020-04-01 00:00:00.000,4.0,4.0,"gps",,"r","b","test study bart"
  26735128477,true,2020-07-01 00:00:00.000,7.0,7.0,"gps",,"r","c","test study bart"
  26735128478,true,2020-08-01 00:00:00.000,8.0,8.0,"gps",,"r","c","test study bart"
  26735128479,true,2020-09-01 00:00:00.000,9.0,9.0,"gps",,"y","c","test study bart"
  26735128480,true,2020-10-01 00:00:00.000,10.0,10.0,"gps",,"y","c","test study bart"'
  )
  expect_message(expect_s3_class(mt_read(I(txt)), "move2"), "There are multiple tags used for one individual")
  expect_no_message(expect_s3_class(mt_read(I(txt), col_select = !(`tag-local-identifier`)), "move2"))
  suppressMessages(expect_identical(mt_read(I(txt)) |> mt_track_data() |> nrow(), 4L))
  suppressMessages(expect_true("individual-tag-local-identifier" %in% (mt_read(I(txt)) |> names())))

  expect_identical(mt_read(I(txt), col_select = !(`tag-local-identifier`)) |> mt_track_data() |> nrow(), 2L)
  expect_false("individual-tag-local-identifier" %in% (mt_read(I(txt), col_select = !(`tag-local-identifier`)) |>
    names()))
})
test_that("env data read works", {
  expect_s3_class(
    suppressWarnings(data <- mt_read(mt_example("Galapagos_Albatrosses-1332012225316982996.zip"))),
    "move2"
  )
  data <- suppressWarnings(mt_read(mt_example("Galapagos_Albatrosses-1332012225316982996.zip")))
  expect_named(data, c(
    "event-id", "timestamp", "eobs:battery-voltage", "eobs:fix-battery-voltage",
    "eobs:horizontal-accuracy-estimate", "eobs:key-bin-checksum", "eobs:speed-accuracy-estimate",
    "eobs:start-timestamp", "eobs:status", "eobs:temperature", "eobs:type-of-fix", "eobs:used-time-to-get-fix",
    "ground-speed", "heading", "height-above-ellipsoid", "visible", "sensor-type", "individual-local-identifier",
    "height-above-msl", "MODIS Ocean Aqua OceanColor 4km 8d Chlorophyll A (OCI)", "OSU Ocean NPP 0.083deg 8d NPP",
    "ECMWF ERA5 SL Wind (10 m above Ground U Component)", "ECMWF ERA5 SL Wind (10 m above Ground V Component)",
    "geometry"
  ))
  expect_identical(
    units(data$`ECMWF ERA5 SL Wind (10 m above Ground U Component)`),
    units(units::as_units("m/s"))
  )
})


test_that("error non unique id", {
  txt <- paste0(
    "event-id,visible,timestamp,location-long,location-lat,sensor-type,individual-taxon-canonical-name",
    ',tag-local-identifier,individual-local-identifier,study-name
  26735128471,true,2020-01-01 00:00:00.000,1.0,1.0,"gps",,"w","b_n","test study bart"
  26735128472,true,2020-02-01 00:00:00.000,2.0,2.0,"gps",,"w","b_n","test study bart"
  26735128475,true,2020-05-01 00:00:00.000,5.0,5.0,"gps",,"n_w","b","test study bart"
  26735128476,true,2020-06-01 00:00:00.000,6.0,6.0,"gps",,"n_w","b","test study bart"
  26735128473,true,2020-03-01 00:00:00.000,3.0,3.0,"gps",,"r","b","test study bart"
  26735128474,true,2020-04-01 00:00:00.000,4.0,4.0,"gps",,"r","b","test study bart"
  26735128477,true,2020-07-01 00:00:00.000,7.0,7.0,"gps",,"r","c","test study bart"
  26735128478,true,2020-08-01 00:00:00.000,8.0,8.0,"gps",,"r","c","test study bart"
  26735128479,true,2020-09-01 00:00:00.000,9.0,9.0,"gps",,"y","c","test study bart"
  26735128480,true,2020-10-01 00:00:00.000,10.0,10.0,"gps",,"y","c","test study bart"'
  )
  suppressMessages(expect_error(mt_read(I(txt)), "Combining tag and individual identifiers does not result in unique names"))
})
