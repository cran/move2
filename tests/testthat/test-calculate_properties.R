test_that("all have units", {
  expect_s3_class(x <- mt_sim_brownian_motion(), "move2")
  expect_identical(st_crs(x), st_crs(NA))
  expect_false(inherits(mt_distance(x), "units"))
  expect_type(mt_distance(x), "double")
  expect_false(inherits(mt_time_lags(x), "units"))
  expect_type(mt_time_lags(x), "integer")
  expect_false(inherits(mt_speed(x), "units"))
  expect_type(mt_speed(x), "double")
  expect_identical(
    mt_sim_brownian_motion() |>
      mutate(time = as.POSIXct(time, origin = "1970-1-1")) |> mt_speed() |> units()
      |> as.character(),
    "1/s"
  )
  expect_identical(
    mt_sim_brownian_motion() |> sf::st_set_crs(3857) |>
      mutate(time = set_units(time, "h")) |> mt_speed() |> units()
      |> as.character(),
    "m/h"
  )
})
test_that("all movebank downloaded data has units", {
  testthat::skip_if_not_installed("lwgeom")
  skip_if_offline()
  skip_on_cran()
  skip_if_no_mbpwd()
  withr::with_envvar(
    list("movebank:move2_user" = Sys.getenv("MBPWD")),
    withr::with_options(list(keyring_backend = "env"), {
      expect_s3_class(x <- movebank_download_study(2911040,
        attributes = NULL,
        timestamp_end = as.POSIXct("2008-06-6 06:00:00")
      ), "move2")
      expect_s3_class(x <- filter(x, !st_is_empty(x)), "move2")
      expect_s3_class(mt_distance(x), "units")
      # prevent message on loading s2
      suppressMessages(expect_s3_class(mt_azimuth(x), "units"))
      expect_s3_class(mt_turnangle(x), "units")
      expect_s3_class(mt_speed(x), "units")
      expect_s3_class(mt_time_lags(x), "units")
    })
  )
})
test_that("all have correct length", {
  expect_s3_class(x <- mt_sim_brownian_motion(t = list(1, 1:2, 1:3, 1:4, 1:10), tracks = 5L), "move2")
  expect_length(mt_distance(x), 20)
  expect_length(mt_time_lags(x), 20)
  expect_length(mt_time(x), 20)
  expect_length(mt_speed(x), 20)
  expect_length(x |> sf::st_set_crs(4326) |> mt_azimuth(), 20)
  expect_length(x |> sf::st_set_crs(4326) |> mt_turnangle(), 20)
})
test_that("all have na in correct locations", {
  expect_s3_class(x <- mt_sim_brownian_motion(t = list(1, 1:2, 1:3, 1:4, 1:10), tracks = 5L), "move2")
  expect_equal(which(is.na(mt_distance(x))), c(1L, 3L, 6L, 10L, 20L), ignore_attr = TRUE)
  expect_identical(which(is.na(mt_time_lags(x))), c(1L, 3L, 6L, 10L, 20L))
  expect_false(anyNA(mt_time(x)))
  expect_equal(which(is.na(mt_speed(x))), c(1, 3, 6, 10, 20), ignore_attr = TRUE)
  expect_identical(x |> sf::st_set_crs(4326) |> mt_azimuth() |> is.na() |> which(), c(1L, 3L, 6L, 10L, 20L))
  expect_identical(x |> sf::st_set_crs(4326) |> mt_turnangle() |> is.na() |> which(), c(1L, 2L, 3L, 4L, 6L, 7L, 10L, 11L, 20L))
})
test_that("has correct value", {
  n <- 10
  d <- data.frame(
    x = c(0, 0, 1, 0, 1, 0, -1, -2, -3, -3), y = c(1:4, 4, 5, 6, 6, 5, 4),
    timestamp = 1:n, track = gl(1, n)
  )
  m <- mt_as_move2(st_as_sf(d, coords = 1:2, crs = 3857), track_id_column = "track", time_column = "timestamp") %>%
    st_transform(4326)
  expect_identical(
    units::set_units(mt_azimuth(m), "degrees") |> round(),
    set_units(c(0, 45, -45, 90, -45, -45, -90, -135, 180, NA), "degrees")
  )
  expect_identical(
    units::set_units(mt_turnangle(m), "degrees") |> round(),
    set_units(c(NA, 45, -90, 135, -135, 0, -45, -45, -45, NA), "degrees")
  )
  m <- mt_as_move2(st_as_sf(d, coords = 1:2), track_id_column = "track", time_column = "timestamp")
  expect_identical(
    units::set_units(mt_azimuth(m), "degrees"),
    set_units(c(0, 45, -45, 90, -45, -45, -90, -135, 180, NA), "degrees")
  )
  expect_equal(
    units::set_units(mt_turnangle(m), "degrees"),
    set_units(c(NA, 45, -90, 135, -135, 0, -45, -45, -45, NA), "degrees"),
    tolerance = 1e-10
  )
})
test_that("turn angles in -180 180 range", {
  d <- data.frame(x = c(0, 0, 1, 0, 1), y = -c(1:5), timestamp = 1:5, track = gl(1, 5), timestamp2 = c(0, 1, 3, 6, 10))
  expect_identical(
    mt_as_move2(st_as_sf(d, coords = 1:2), track_id_column = "track", time_column = "timestamp") |>
      mt_azimuth() |> set_units("degrees"),
    set_units(c(180, 135, -135, 135, NA), "degrees")
  )
  expect_identical(
    mt_as_move2(st_as_sf(d, coords = 1:2), track_id_column = "track", time_column = "timestamp") |>
      mt_turnangle() |> set_units("degrees"),
    set_units(c(NA, -45, 90, -90, NA), "degrees")
  )
  expect_identical(
    mt_as_move2(st_as_sf(d, coords = 2:1), track_id_column = "track", time_column = "timestamp") |>
      mt_turnangle() |> set_units("degrees"),
    set_units(c(NA, 45, -90, 90, NA), "degrees")
  )
  d[["y"]] <- -d[["y"]]
  expect_identical(
    mt_as_move2(st_as_sf(d, coords = 1:2), track_id_column = "track", time_column = "timestamp") |>
      mt_turnangle() |> set_units("degrees"),
    set_units(c(NA, 45, -90, 90, NA), "degrees")
  )
  expect_identical(
    mt_as_move2(st_as_sf(d, coords = 2:1), track_id_column = "track", time_column = "timestamp") |>
      mt_turnangle() |> set_units("degrees"),
    set_units(c(NA, -45, 90, -90, NA), "degrees")
  )
  expect_equal(mt_as_move2(st_as_sf(d, coords = 2:1), track_id_column = "track", time_column = "timestamp") |>
    mt_distance(), c(1, sqrt(2), sqrt(2), sqrt(2), NA), ignore_attr = TRUE)
  expect_equal(mt_as_move2(st_as_sf(d, coords = 2:1), track_id_column = "track", time_column = "timestamp") |>
    mt_speed(), c(1, sqrt(2), sqrt(2), sqrt(2), NA), ignore_attr = TRUE)
  expect_equal(mt_as_move2(st_as_sf(d, coords = 2:1), track_id_column = "track", time_column = "timestamp2") |>
    mt_speed(), c(1, sqrt(2) / 2, sqrt(2) / 3, sqrt(2) / 4, NA), ignore_attr = TRUE)
  expect_equal(mt_as_move2(st_as_sf(d, coords = 2:1), track_id_column = "track", time_column = "timestamp2") |>
    mt_distance(), c(1, sqrt(2), sqrt(2), sqrt(2), NA), ignore_attr = TRUE)
  expect_equal(mt_as_move2(st_as_sf(d, coords = 1:2), track_id_column = "track", time_column = "timestamp") |>
    mt_distance(), c(1, sqrt(2), sqrt(2), sqrt(2), NA), ignore_attr = TRUE)
  expect_equal(mt_as_move2(st_as_sf(d, coords = 1:2), track_id_column = "track", time_column = "timestamp") |>
    mt_speed(), c(1, sqrt(2), sqrt(2), sqrt(2), NA), ignore_attr = TRUE)
  expect_equal(mt_as_move2(st_as_sf(d, coords = 1:2), track_id_column = "track", time_column = "timestamp2") |>
    mt_speed(), c(1, sqrt(2) / 2, sqrt(2) / 3, sqrt(2) / 4, NA), ignore_attr = TRUE)
  expect_equal(mt_as_move2(st_as_sf(d, coords = 1:2), track_id_column = "track", time_column = "timestamp2") |>
    mt_distance(), c(1, sqrt(2), sqrt(2), sqrt(2), NA), ignore_attr = TRUE)
})

test_that("Failures correct", {
  expect_s3_class(m <- mt_sim_brownian_motion(tracks = 1, t = 1:3), "move2")
  expect_error(m[c(1, 3, 2), ] |> mt_distance(), "Not all timestamps in `x` are ordered within track.")
  expect_error(m[c(1, 3, 2), ] |> mt_time_lags(), "Not all timestamps in `x` are ordered within track.")
  expect_error(
    { # nolint
      m$geometry[2] <- st_point()
      m
    } |> mt_distance(),
    "Not all locations are non empty points."
  )
})

test_that("mt_azimuth with different types of track ids", {
  a <- mt_sim_brownian_motion()
  expect_identical(mt_azimuth(a), mt_azimuth(mt_set_track_id(a, as.character(mt_track_id(a)))))
  expect_identical(mt_azimuth(a), mt_azimuth(mt_set_track_id(a, factor(mt_track_id(a)))))
})
