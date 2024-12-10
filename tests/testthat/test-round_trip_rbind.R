test_that("Round trip combining and splitting indviduals", {
  expect_s3_class(a <- mt_sim_brownian_motion(tracks = "a"), "move2")
  expect_s3_class(b <- mt_sim_brownian_motion(tracks = "b"), "move2")
  expect_silent(d <- rbind(a, b))
  expect_silent(aa <- d |> filter_track_data(.track_id = "a"))
  expect_identical(a[TRUE, ], aa) # temporary fix for points matrix
  expect_silent(bb <- d |> filter_track_data(.track_id = "b"))
  expect_identical(b[TRUE, ], bb) # temporary fix for points matrix
})

test_that(
  "Round trip combining and splitting indviduals, with multiple tracks",
  {
    expect_s3_class(a <- mt_sim_brownian_motion(tracks = c("a", "d")), "move2")
    expect_s3_class(b <- mt_sim_brownian_motion(tracks = "b"), "move2")
    expect_silent(d <- rbind(a, b))
    expect_identical(mt_track_id(d), c(mt_track_id(a), mt_track_id(b)))
    expect_identical(mt_time(d), c(mt_time(a), mt_time(b)))
    expect_silent(aa <- d |> filter_track_data(.track_id = c("a", "d")))
    expect_identical(a, aa)
    expect_silent(bb <- d |> filter_track_data(.track_id = "b"))
    expect_identical(b[TRUE, ], bb) # temporary fix for points matrix
  }
)

test_that(
  "Round trip combining and splitting indviduals, with multiple tracks reversed order",
  {
    expect_s3_class(a <- mt_sim_brownian_motion(tracks = c("a", "d")), "move2")
    expect_s3_class(b <- mt_sim_brownian_motion(tracks = "b"), "move2")
    expect_silent(d <- rbind(b, a))
    expect_silent(aa <- d |> filter_track_data(.track_id = c("a", "d")))
    expect_identical(a, aa)
    expect_silent(bb <- d |> filter_track_data(.track_id = "b"))
    expect_identical(b[TRUE, ], bb) # temporary fix for points matrix
  }
)
test_that("Rbind different timezones", {
  expect_identical(
    rbind(
      mt_sim_brownian_motion(as.POSIXct("1970-1-1", tz = "UTC") + 1:3),
      mt_sim_brownian_motion(as.POSIXct("1970-1-1", tz = "UTC+5") + 1:3, tracks = "4")
    ) |> mt_time(),
    structure(c(1, 2, 3, 1, 2, 3, 18001, 18002, 18003), class = c(
      "POSIXct",
      "POSIXt"
    ), tzone = "UTC")
  )
  expect_error(rbind(
    mt_sim_brownian_motion(as.POSIXct("1970-1-1", tz = "UTC") + 1:3),
    mt_sim_brownian_motion(1:3, tracks = 1)
  ))
})
test_that("rbinding same name errors", {
  expect_s3_class(a <- mt_sim_brownian_motion(tracks = c("a", "d")), "move2")
  expect_s3_class(b <- mt_sim_brownian_motion(tracks = "d"), "move2")
  expect_error(rbind(b, a))
})
