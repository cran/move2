test_that("need time input", {
  expect_error(mt_filter_per_interval(mt_sim_brownian_motion()))
})
test_that("need time input", {
  expect_equal(sum(mt_per_interval(mt_sim_brownian_motion(t = as.Date(0:6 * 15, "1970-1-1")), unit = "month")),
    expected = 8
  )
  expect_equal(
    mt_per_interval(mt_sim_brownian_motion(t = as.Date(0:6 * 15, "1970-1-1"), tracks = 1),
      criterion = "first", unit = "month"
    ),
    expected = c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
  )
  expect_equal(
    mt_per_interval(mt_sim_brownian_motion(t = as.Date(0:6 * 15, "1970-1-1"), tracks = 1),
      criterion = "first", unit = "2 months"
    ),
    expected = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
  )
  expect_equal(
    mt_per_interval(mt_sim_brownian_motion(t = as.Date(0:6 * 15, "1970-1-1"), tracks = 1),
      criterion = "last", unit = "month"
    ),
    expected = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
  )
  skip_on_os("mac", arch = "x86_64")
  expect_equal(
    mt_per_interval(
      mt_sim_brownian_motion(
        t = as.POSIXct(as.Date((0:6 * 15) + 0, "1970-1-1"),
          tz = "UTC"
        ),
        tracks = 1
      ),
      criterion = "last", unit = "month"
    ),
    expected = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
  )
  expect_equal(
    mt_per_interval(mt_sim_brownian_motion(t = as.Date(0:6 * 15, "1970-1-1"), tracks = 1),
      criterion = "last", unit = "day"
    ),
    expected = rep(TRUE, 7)
  )
})
