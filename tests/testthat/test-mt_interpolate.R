test_that("basic interpolation", {
  d <- data.frame(x = c(1L:5L), y = c(1L:5L), timestamp = 1L:5L, track = gl(1, 5), timestamp2 = c(0, 1, 3, 6, 10)) |>
    st_as_sf(coords = 1L:2L) |>
    mt_as_move2(track_id_column = "track", time_column = "timestamp")
  expect_silent(mt_interpolate(d))
  expect_identical(mt_interpolate(d, 2:4, omit = TRUE) |> mt_time(), 2:4)
  dd <- d
  dd$geometry[c(1, 3, 5)] <- sf::st_point()
  expect_equal(st_coordinates(mt_interpolate(dd)), cbind(c(NA, 2:4, NA), c(NA, 2:4, NA)), ignore_attr = TRUE)
  dd <- d
  dd$geometry[c(2, 3, 4)] <- sf::st_point()
  expect_equal(st_coordinates(mt_interpolate(dd)), cbind(c(1L:5L), c(1L:5L)), ignore_attr = TRUE)
  expect_identical(mt_interpolate(dd), d)
  expect_identical(st_geometry(mt_interpolate(d[-3, ], 3L)), st_geometry(d)[TRUE, ]) # temporary fix for points matrix
  expect_silent(mt_interpolate(d[-3, ], 3L))
  expect_silent(mt_interpolate(d[-3, ], 3L))
  expect_error(mt_interpolate(d[-3, ], "5 mins"), "does not correspond to the class of the .time.")
})

test_that("crs in crs out for interpolate", {
  m <- mt_sim_brownian_motion()
  sf::st_geometry(m)[c(3:5, 14, 20)] <- sf::st_point()
  expect_identical(st_crs(m), st_crs(mt_interpolate(m)))
  m <- sf::st_set_crs(mt_sim_brownian_motion(), 4326)
  expect_identical(st_crs(m), st_crs(mt_interpolate(m)))
  m <- sf::st_set_crs(mt_sim_brownian_motion(), 3857)
  expect_identical(st_crs(m), st_crs(mt_interpolate(m)))
})


test_that("start end locations stay empty", {
  m <- mt_sim_brownian_motion()
  sf::st_geometry(m)[c(3:5, 10, 14, 20)] <- sf::st_point()
  expect_identical(which(st_is_empty(mt_interpolate(m))), c(10L, 20L))
  # omit only takes effect when time argument is used
  expect_identical(mt_interpolate(m, omit = TRUE), mt_interpolate(m, omit = FALSE))
  expect_identical(which(st_is_empty(mt_interpolate(sf::st_set_crs(m, 4326)))), c(10L, 20L))
  m <- mt_sim_brownian_motion()
  sf::st_geometry(m)[c(1, 3:5, 11, 14, 20)] <- sf::st_point()
  expect_identical(which(st_is_empty(mt_interpolate(m))), c(1L, 11L, 20L))
  expect_identical(which(st_is_empty(mt_interpolate(sf::st_set_crs(m, 4326)))), c(1L, 11L, 20L))
})


test_that("works on long lat", {
  # to gen data: dput(data.frame(geosphere::destPoint(c(45,67),270, d = c(0,10,60,90)*1000), t=c(1,2,7,10), id="a"))
  m <- structure(list(lon = c(
    45, 44.7707477678058, 43.6247042339104,
    42.9374757707606
  ), lat = c(
    67, 66.9998348693807, 66.9940560433849,
    66.9866282535198
  ), t = c(1, 2, 7, 10), id = c(
    "a", "a", "a",
    "a"
  )), class = "data.frame", row.names = c(NA, -4L)) |>
    st_as_sf(coords = 1L:2L, crs = 4326) |>
    mt_as_move2(time_column = "t", track_id_column = "id")
  mm <- m
  mm$geometry[2L:3L] <- st_point()
  expect_equal(mt_interpolate(mm), m, tolerance = 7e-08) # allow small tolerances as geosphere might differ from s2
  expect_true(all(sf::st_distance(mt_interpolate(mm), m, by_element = TRUE) < set_units(1, "m")))
  expect_equal(mt_interpolate(st_transform(mm, 3857)), st_transform(m, 3857),
    tolerance = 7e-08
  ) # allow small tolerances as geosphere might differ from s2
  expect_true(all(sf::st_distance(mt_interpolate(st_transform(mm, 3857)),
    st_transform(m, 3857),
    by_element = TRUE
  ) < set_units(1, "m")))
  expect_equal(st_transform(mt_interpolate(st_transform(mm, 3857)), 4326), m,
    tolerance = 7e-08
  ) # allow small tolerances as geosphere might differ from s2
  expect_true(all(sf::st_distance(st_transform(mt_interpolate(st_transform(mm, 3857)), 4326), m,
    by_element = TRUE
  ) < set_units(1, "m")))
  mmm <- m
  mmm$x <- 1
  expect_identical(
    mt_interpolate(m, mt_time(m)[2L:3L], omit = TRUE),
    mt_interpolate(mmm, mt_time(mmm)[2L:3L], omit = TRUE)[, 1L:3L]
  )
  expect_identical(
    mmm$x,
    mt_interpolate(mmm, mt_time(mmm)[2L:3L])$x
  )
  mmm$x <- NULL
  mmm$time <- 1

  expect_identical(
    mt_interpolate(m, mt_time(m)[2L:3L] + 2, omit = TRUE),
    mt_interpolate(mmm, mt_time(mmm)[2L:3L] + 2, omit = TRUE)[, 1L:3L]
  )
  expect_identical(
    c(1, 1, NA, 1, NA, 1),
    mt_interpolate(mmm, mt_time(mmm)[2L:3L] + 2)$time
  )
})

test_that("time_gap", {
  t <- c(1, 3, 5, 6, 7)
  m <- mt_as_move2(data.frame(x = t, y = t, t = t, id = "a", stringsAsFactors = FALSE),
    coords = 1L:2L, time_column = "t", track_id_column = "id"
  )
  m$geometry[c(2L, 4L)] <- sf::st_point()
  expect_identical(which(st_is_empty(mt_interpolate(m, max_time_lag = 5))), integer(0))
  expect_identical(which(st_is_empty(mt_interpolate(m, max_time_lag = 3))), 2L)
  expect_identical(which(st_is_empty(mt_interpolate(m, max_time_lag = 2))), 2L)
  expect_identical(which(st_is_empty(mt_interpolate(m, max_time_lag = 1.5))), c(2L, 4L))
  expect_error(mt_interpolate(m, max_time_lag = as.difftime(141, units = "secs")))
  expect_error(mt_interpolate(m, max_time_lag = set_units(3, "min")))
})

test_that("time_gap with posixct", {
  t <- as.POSIXct("1980-1-1", tz = "UTC") + c(1, 3, 5, 6, 7) * 60
  m <- mt_as_move2(data.frame(x = t, y = t, t = t, id = "a", stringsAsFactors = FALSE),
    coords = 1L:2L, time_column = "t", track_id_column = "id"
  )
  m$geometry[c(2L, 4L)] <- sf::st_point()
  expect_identical(which(st_is_empty(mt_interpolate(m, max_time_lag = as.difftime(5, units = "mins")))), integer(0))
  expect_identical(which(st_is_empty(mt_interpolate(m, max_time_lag = as.difftime(241, units = "secs")))), integer(0))
  expect_identical(which(st_is_empty(mt_interpolate(m, max_time_lag = as.difftime(3, units = "mins")))), 2L)
  expect_identical(which(st_is_empty(mt_interpolate(m, max_time_lag = as.difftime(141, units = "secs")))), 2L)
  expect_identical(which(st_is_empty(mt_interpolate(m, max_time_lag = set_units(3, "min")))), 2L)
  expect_identical(which(st_is_empty(mt_interpolate(m, max_time_lag = set_units(141, "sec")))), 2L)
  expect_error(mt_interpolate(m, max_time_lag = 3))
  expect_identical(
    mt_interpolate(m, "2 mins") |> mt_time(),
    as.POSIXct("1980-1-1", tz = "UTC") + c(1, 2, 3, 4, 5, 6, 7) * 60
  )
  expect_identical(
    mt_interpolate(m, "2 mins", omit = TRUE) |> mt_time(),
    as.POSIXct("1980-1-1", tz = "UTC") + c(2, 4, 6) * 60
  )
  t <- mt_time(m)[2L:3L]
  expect_identical(
    mt_interpolate(m, t, omit = TRUE),
    mt_interpolate(m, lubridate::with_tz(t, "EST"), omit = TRUE)
  )
  expect_identical(
    mt_interpolate(m, t, omit = FALSE),
    mt_interpolate(m, lubridate::with_tz(t, "EST"), omit = FALSE)
  )
})


test_that("time_gap with posixct", {
  t <- as.Date("1980-1-1") + c(1, 3, 5, 6, 7)
  m <- mt_as_move2(data.frame(
    x = t, y = t, t = t, id = "a",
    stringsAsFactors = FALSE
  ), coords = 1L:2L, time_column = "t", track_id_column = "id")
  m$geometry[c(2L, 4L)] <- sf::st_point()
  expect_error(mt_interpolate(m, max_time_lag = 5))
  expect_identical(
    which(st_is_empty(
      mt_interpolate(m, max_time_lag = set_units(5, "days"))
    )),
    integer(0)
  )
  expect_identical(
    which(st_is_empty(
      mt_interpolate(m, max_time_lag = set_units(3, "days"))
    )),
    2L
  )
  expect_identical(
    which(st_is_empty(
      mt_interpolate(m, max_time_lag = as.difftime(48, units = "hours"))
    )),
    2L
  )
  expect_identical(
    which(st_is_empty(
      mt_interpolate(m, max_time_lag = set_units(36, "hour"))
    )),
    c(2L, 4L)
  )
})
test_that("error different time", {
  m <- mt_sim_brownian_motion(t = c(1L:5L, 5.5))
  tt <- Sys.time()
  mt <- mt_sim_brownian_motion(t = (as.Date(tt) + -1L:4L))

  expect_s3_class(mt_interpolate(m, 1.5), "move2")
  expect_error(mt_interpolate(m, tt), "does not correspond to the class of the .time.")
  expect_error(mt_interpolate(mt, tt), "does not correspond to the class of the .time.")
  expect_error(mt_interpolate(mt, "hour"), "does not correspond to the class of the .time.")
})
