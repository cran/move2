test_that("convertions of move", {
  skip_if_not_installed("move")
  data(leroy, package = "move")
  expect_s3_class(m <- mt_as_move2(leroy), "move2")
  expect_identical(move::n.locs(leroy), dim(m)[1])
  expect_identical(move::timestamps(leroy), mt_time(m))
  expect_identical(mt_track_id_column(m), "track")
})
test_that("convertions of move with changed times", {
  skip_if_not_installed("move")
  data(leroy, package = "move")
  leroy$timestamp <- leroy$timestamp + 4
  expect_s3_class(m <- mt_as_move2(leroy), "move2")
  expect_identical(move::n.locs(leroy), dim(m)[1])
  expect_identical(move::timestamps(leroy), mt_time(m))
})

test_that("round trip", {
  skip_if_not_installed("move")
  expect_s3_class(a <- mt_sim_brownian_motion(as.POSIXct("1970-1-1") + 1:5), "move2")
  expect_identical(st_geometry(aa <- mt_as_move2(to_move(a))), st_geometry(a))
  expect_identical(mt_time(a), mt_time(aa))
})
test_that("error on wrong column name", {
  expect_error(mt_as_move2(
    st_as_sf(data.frame(
      x = c(0, 0, 1, 0),
      y = c(1:4), timestamp = 1:4, track = gl(1, 4)
    ), coords = 1:2),
    track_id_column = "track", time_column = "time"
  ))
  expect_error(mt_as_move2(
    st_as_sf(data.frame(
      x = c(0, 0, 1, 0), y = c(1:4),
      timestamp = 1:4, track = gl(1, 4)
    ), coords = 1:2),
    track_id_column = "trac", time_column = "timestamp"
  ))
})

test_that("error on wrong column values", {
  expect_error(mt_as_move2(
    st_as_sf(data.frame(
      x = c(0, 0, 1, 0),
      y = c(1:4), timestamp = 1:4, track = c(NA, 1, 1, 1)
    ), coords = 1:2),
    track_id_column = "track", time_column = "timestamp"
  ), "should not contain NA")
  expect_error(mt_as_move2(
    st_as_sf(data.frame(
      x = c(0, 0, 1, 0),
      y = c(1:4), timestamp = 1:4, track = rep(as.Date(1, origin = "1970-1-1"), 4)
    ), coords = 1:2),
    track_id_column = "track", time_column = "timestamp"
  ), "should be of the type integer, in")
  expect_error(mt_as_move2(
    st_as_sf(data.frame(
      x = c(0, 0, 1, 0),
      y = c(1:4), timestamp = letters[1:4], track = rep((1), 4)
    ), coords = 1:2),
    track_id_column = "track", time_column = "timestamp"
  ), "The time column should be")
})
