m <- mt_sim_brownian_motion(t = 1L:2L, tracks = 2L)
ml <- mt_sim_brownian_motion(t = 1L:2L, tracks = letters)
test_that("mt_is_move2", {
  expect_error(assert_that(mt_is_move2(1L)), "The input does not inherit the <move2> class")
  expect_error(assert_that(mt_is_move2(data.frame(a = 4L))), "The input does not inherit the <move2> class")
  expect_true(assert_that(mt_is_move2(m)))
})
test_that("Proper failure message ordered assertions", {
  skip_if_not_installed("dplyr")
  expect_error(
    assert_that(mt_is_time_ordered(dplyr::arrange(m, time))),
    "The following tracks occur at multiple places in the data set: `1` and `2`."
  )
  expect_error(
    assert_that(mt_is_time_ordered(head(dplyr::arrange(m, time), 3L))),
    "The following track occurs at multiple places in the data set: `1`."
  )
  expect_error(
    assert_that(mt_is_time_ordered(dplyr::arrange(ml, time))),
    "Not all tracks are grouped in `x`."
  )
  expect_error(
    assert_that(mt_has_unique_location_time_records(m[c(1L, 2L, 2L), ])),
    "In total there is 1 duplicated timestamp."
  )
  expect_error(
    assert_that(mt_has_unique_location_time_records(m[c(1L, 2L, 2L), ])),
    "The first duplicated record is from track 1 at time 2 \\(record: 3\\)."
  )
  expect_error(
    assert_that(mt_has_unique_location_time_records(m[c(1L, 1L, 2L, 2L), ])),
    "In total there are 2 duplicate timestamps."
  )
  sf::st_geometry(m)[2L] <- sf::st_point()
  expect_true(assert_that(mt_has_unique_location_time_records(m[c(1L, 2L, 2L), ])))
  expect_error(
    assert_that(mt_has_no_empty_points(m)),
    "Not all locations are non empty points."
  )
  expect_error(
    assert_that(mt_has_no_empty_points(m)),
    "At least the following record is not a non-empty point: "
  )
  expect_error(
    assert_that(mt_has_no_empty_points(m[c(1L, 2L, 2L), ])),
    "At least the following records are not non-empty points: "
  )
  expect_true(mt_sim_brownian_motion(c(1.0, 1.0)) |> mt_is_time_ordered())
  expect_true(mt_sim_brownian_motion(c(1.0, 1.0)) |> mt_is_time_ordered(non_zero = FALSE))
  expect_false(mt_sim_brownian_motion(c(1.0, 1.0)) |> mt_is_time_ordered(non_zero = TRUE))
  expect_true(mt_sim_brownian_motion(c(1.0, 1.0)) |> mt_is_time_ordered_non_empty_points())
  expect_true(mt_sim_brownian_motion(c(1.0, 1.0)) |> mt_is_time_ordered_non_empty_points(non_zero = FALSE))
  expect_false(mt_sim_brownian_motion(c(1.0, 1.0)) |> mt_is_time_ordered_non_empty_points(non_zero = TRUE))
})
test_that("column incompatablity", {
  m <- mt_read(mt_example())
  expect_true(mt_has_unique_location_time_records(m))
  expect_false(mt_has_unique_location_time_records(m[c(1L, 5L, 5L, 6L, 6L), ]))
  expect_error(assert_that(mt_has_unique_location_time_records(m[c(1L, 5L, 5L, 6L, 6L), ])), "In total there are 2 duplicate timestamps")
  expect_error(
    assert_that(mt_has_unique_location_time_records(m[c(1L, 5L, 5L, 6L, 6L), ])),
    "is from track F1 at time 2011-02-11 18:06:13.999 .record: 3"
  )
  mm <- m |>
    mutate("individual_int64" = as.integer64(`individual-local-identifier`)) |>
    mt_set_track_id("individual_int64")
  expect_true(mt_has_unique_location_time_records(mm))
  expect_false(mt_has_unique_location_time_records(mm[c(1L, 5L, 5L, 6L, 6L), ]))
  mm <- m |>
    mutate("date" = as.Date(timestamp)) |>
    mt_set_time("date")
  expect_false(mt_has_unique_location_time_records(mm))
})
