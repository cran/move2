test_that("multiplication works", {
  expect_s3_class(x <- mt_sim_brownian_motion(t = list(1L:5L, 3L:7L)), "move2")
  suppressMessages(expect_true(x |> mt_track_lines() |> st_geometry() |> st_is("LINESTRING") |> all()))
  suppressMessages(expect_identical(x |> mt_track_lines() |> nrow(), 2L))
  suppressMessages(expect_identical(x |> mt_track_lines(t = mean(time)) |> pull(t), c(3.0, 5.0)))
})
