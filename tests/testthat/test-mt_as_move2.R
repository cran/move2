test_that("convertions of move", {
  skip_if_not_installed("move")
  data(leroy, package = "move")
  expect_s3_class(m <- mt_as_move2(leroy), "move2")
  expect_false("citation" %in% colnames(mt_track_data(m)))
  expect_false("license" %in% colnames(mt_track_data(m)))
  expect_identical(move::n.locs(leroy), dim(m)[1L])
  expect_identical(move::timestamps(leroy), mt_time(m))
  expect_identical(mt_track_id_column(m), "track")
  expect_identical(move::citations(leroy), character())
  expect_silent(move::citations(leroy) <- "text")
  expect_true("citation" %in% colnames(mt_track_data(mt_as_move2(leroy))))
  expect_identical(mt_track_data(mt_as_move2(leroy))$citation, "text")
  expect_identical(move::licenseTerms(leroy), character())
  expect_silent(move::licenseTerms(leroy) <- "text2")
  expect_true("license" %in% colnames(mt_track_data(mt_as_move2(leroy))))
  expect_identical(mt_track_data(mt_as_move2(leroy))$license, "text2")
})
test_that("convertions of move with changed times", {
  skip_if_not_installed("move")
  data(leroy, package = "move")
  leroy$timestamp <- leroy$timestamp + 4.0
  expect_s3_class(m <- mt_as_move2(leroy), "move2")
  expect_identical(move::n.locs(leroy), dim(m)[1L])
  expect_identical(move::timestamps(leroy), mt_time(m))
})

test_that("round trip", {
  skip_if_not_installed("move")
  expect_s3_class(a <- mt_sim_brownian_motion(as.POSIXct("1970-1-1") + 1L:5L), "move2")
  expect_identical(st_geometry(aa <- mt_as_move2(to_move(a)))[TRUE, ], st_geometry(a)) # realize points to prevent error
  expect_identical(mt_time(a), mt_time(aa))
})
test_that("error on wrong column name", {
  expect_error(mt_as_move2(
    st_as_sf(data.frame(
      x = c(0L, 0L, 1L, 0L),
      y = 1L:4L, timestamp = 1L:4L, track = gl(1L, 4L)
    ), coords = 1L:2L),
    track_id_column = "track", time_column = "time"
  ))
  expect_error(mt_as_move2(
    st_as_sf(data.frame(
      x = c(0L, 0L, 1L, 0L), y = 1L:4L,
      timestamp = 1L:4L, track = gl(1L, 4L)
    ), coords = 1L:2L),
    track_id_column = "trac", time_column = "timestamp"
  ))
})

test_that("error on wrong column values", {
  expect_error(mt_as_move2(
    st_as_sf(data.frame(
      x = c(0L, 0L, 1L, 0L),
      y = 1L:4L, timestamp = 1L:4L, track = c(NA, 1L, 1L, 1L)
    ), coords = 1L:2L),
    track_id_column = "track", time_column = "timestamp"
  ), "should not contain NA")
  expect_error(mt_as_move2(
    st_as_sf(data.frame(
      x = c(0L, 0L, 1L, 0L),
      y = 1L:4L, timestamp = 1L:4L,
      track = rep(as.Date(1L, origin = "1970-1-1"), 4L)
    ), coords = 1L:2L),
    track_id_column = "track", time_column = "timestamp"
  ), "should be of the type integer, in")
  expect_error(mt_as_move2(
    st_as_sf(data.frame(
      x = c(0L, 0L, 1L, 0L),
      y = 1L:4L, timestamp = letters[1L:4L], track = rep(1L, 4L)
    ), coords = 1L:2L),
    track_id_column = "track", time_column = "timestamp"
  ), "The time column should be")
})
