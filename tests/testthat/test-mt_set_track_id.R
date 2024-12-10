test_that("test renaming track id", {
  m <- mt_sim_brownian_motion()
  expect_false(has_name(m, "new_track"))
  expect_true(has_name(m, "track"))
  expect_silent(mt_track_id(m) <- "new_track")
  expect_true(has_name(m, "new_track"))
  expect_false(has_name(m, "track"))
})
test_that("test renaming time", {
  m <- mt_sim_brownian_motion()
  expect_false(has_name(m, "new_time"))
  expect_true(has_name(m, "time"))
  expect_silent(mt_time(m) <- "new_time")
  expect_true(has_name(m, "new_time"))
  expect_false(has_name(m, "time"))
})
test_that("splitting track and assigning new track ids", {
  m <- mt_sim_brownian_motion()
  expect_identical(mt_n_tracks(m), 2L)
  expect_silent(mt_track_id(m) <- gl(4L, 5L))
  expect_identical(nrow(mt_track_data(m)), 4L)
  expect_identical(mt_n_tracks(m), 4L)
})
test_that("merging track and assigning new id", {
  m <- mt_sim_brownian_motion(tracks = 3) |> mutate_track_data(size = 1:3)
  expect_identical(mt_n_tracks(m), 3L)
  expect_silent(mt_track_id(m) <- c(rep(1, 20), rep(2, 10)))
  expect_identical(nrow(mt_track_data(m)), 2L)
  expect_identical(mt_n_tracks(m), 2L)
})
test_that("splitting track and assigning new id retains column and data removed when no match", {
  m <- mt_sim_brownian_motion() |> mutate_track_data(sex = c("f", "m"))
  expect_identical(mt_n_tracks(m), 2L)
  expect_silent(mt_track_id(m) <- gl(4, 5))
  expect_identical(nrow(mt_track_data(m)), 4L)
  expect_identical(mt_n_tracks(m), 4L)
  expect_identical(mt_track_data(m)$sex, c("f", "f", "m", "m"))
  expect_identical(ncol(mt_track_data(mt_set_track_id(m, gl(1, 20)))), 2L)
  expect_identical(
    colnames(mt_track_data(mt_set_track_id(m, gl(1, 20)))),
    c("track", "sex")
  )
  expect_identical(
    mt_track_data(mt_set_track_id(m, gl(1, 20)))$sex,
    list(c("f", "f", "m", "m"))
  )
  expect_identical(
    mt_track_data(mt_set_track_id(m, c(
      rep("a", 5),
      rep("b", 10),
      rep("a", 5)
    )))$sex,
    list(c("f", "m"), c("f", "m"))
  )
  expect_identical(
    mt_track_data(mt_set_track_id(m, c(
      rep("a", 5),
      rep("b", 10),
      rep("c", 5)
    )))$sex,
    list(c("f"), c("f", "m"), "m")
  )
  expect_identical(
    mt_track_data(mt_set_track_id(
      m,
      c(
        rep("a", 10),
        rep("b", 10)
      )
    ))$sex,
    list(c("f", "f"), c("m", "m"))
  )

  expect_identical(mt_set_track_id(m, gl(1, 20)) |> mt_track_data() |> class(), m |> mt_track_data() |> class())
  m <- mt_set_track_data(m, dplyr::as_tibble(mt_track_data(m)))
  expect_identical(mt_set_track_id(m, gl(1, 20)) |> mt_track_data() |> class(), m |> mt_track_data() |> class())
})
test_that("splitting track with new column", {
  m <- mt_sim_brownian_motion()
  expect_identical(mt_n_tracks(m), 2L)
  expect_silent(m[, "new_id"] <- gl(4, 5))
  expect_silent(mt_track_id(m) <- "new_id")
  expect_identical(nrow(mt_track_data(m)), 4L)
  expect_identical(mt_n_tracks(m), 4L)
})
# FIX do more tests with setting track id's and the track data

test_that("Expect class of track data is retained", {
  a <- mt_sim_brownian_motion()
  a$new <- gl(4, 5)
  expect_identical(mt_set_track_id(a, "new") |> mt_n_tracks(), 4L)
  expect_identical(mt_set_track_id(a, "new") |> mt_track_data() |> class(), a |> mt_track_data() |> class())
  a <- mt_set_track_data(a, dplyr::as_tibble(mt_track_data(a)))
  expect_identical(mt_set_track_id(a, "new") |> mt_track_data() |> class(), a |> mt_track_data() |> class())
})
test_that("Assing track id with track attribute", {
  a <- mt_sim_brownian_motion(tracks = 3) |> mutate_track_data(hh = c(1L, 1L, 2L), kk = 1:3)
  expect_identical(mt_set_track_id(a, "hh") |> mt_n_tracks(), 2L)
  expect_identical(mt_set_track_id(a, "hh") |> mt_track_id_column(), "hh")
  expect_identical(mt_set_track_id(a, "hh") |> mt_track_id(), c(rep(1L, 20), rep(2L, 10)))
  expect_identical(mt_track_data(mt_set_track_id(a, "hh"))$kk, list(1:2, 3L))
  expect_false(as.logical(anyDuplicated(colnames(mt_track_data(mt_set_track_id(a, "hh"))))))
})
test_that("Assing track id with track attribute", {
  a <- mt_sim_brownian_motion(tracks = 3) |>
    mutate_track_data(hh = c(1L, 1L, 2L), kk = 1:3) |>
    mutate(kk = rep(1:6, 5))
  expect_error(mt_set_track_id(a, "kk"), class = "move2_error_two_track_id_columns")
})
