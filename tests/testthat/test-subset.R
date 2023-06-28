test_that("creating and removing extra columns", {
  m <- mt_sim_brownian_motion()
  expect_silent(m[, "count"] <- 1:20)
  expect_identical(m$count, 1:20)
  expect_identical(m[(1:10) * 2, ], m[c(FALSE, TRUE), ])
  expect_identical(m[, "count", drop = TRUE], 1:20)
  expect_s3_class(m[, "count", drop = FALSE], "move2")
  expect_s3_class(m[, "count"], "move2")
  expect_silent(m[, "count"] <- NULL)
  expect_false("count" %in% colnames(m))
  expect_identical(nrow(mt_track_data(m[1:3, ])), 1L)
  expect_identical(nrow(mt_track_data(m[11:13, ])), 1L)
  expect_identical(nrow(mt_track_data(m[9:13, ])), 2L)
  expect_silent(m[, "count"] <- 1:20)
  expect_identical(nrow(mt_track_data(m[1:3, "count"])), 1L)
  expect_identical(nrow(mt_track_data(m[11:13, "count"])), 1L)
  expect_identical(nrow(mt_track_data(m[9:13, "count"])), 2L)
  expect_identical(m[1:3, "count", drop = TRUE], 1:3)
  expect_identical(m[11:13, "count", drop = TRUE], 11:13)
  expect_identical(m[9:13, "count", drop = TRUE], 9:13)
})

test_that("indexing columns one bracket", {
  m <- mt_sim_brownian_motion()
  expect_named(m[c(TRUE, FALSE, FALSE)], colnames(m), ignore.order = TRUE)
  expect_named(m[TRUE], c("track", "time", "geometry"), ignore.order = TRUE)
  expect_named(m[FALSE], c("track", "time", "geometry"), ignore.order = TRUE)
  expect_named(m["track"], c("track", "time", "geometry"), ignore.order = TRUE)
  m$w <- 1
  m$v <- 1
  m$u <- 1
  expect_named(m["v"], c("track", "time", "geometry", "v"), ignore.order = TRUE)
  expect_named(m[FALSE], c("track", "time", "geometry"), ignore.order = TRUE)
  expect_named(m[TRUE], c("u", "v", "w", "track", "time", "geometry"),
    ignore.order = TRUE
  )
  expect_named(m["track"], c("track", "time", "geometry"), ignore.order = TRUE)
})
