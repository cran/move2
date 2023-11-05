x <- mt_sim_brownian_motion() |> dplyr::mutate(
  date = as.Date("2020-1-1") + time * 3,
  tf = factor(time), new_id = gl(4, 5),
  new_id_2 = as.numeric(new_id) %/% 2
)

test_that("setting time column works as expected", {
  expect_identical(x |> mt_time_column(), "time")
  expect_identical(x |> mt_set_time_column("date") |> mt_time_column(), "date")
  expect_error(x |> mt_set_time_column("asdf"), "The argument `value` needs to be the name of a column in `x`")
  expect_error(x |> mt_set_time_column("tf"), "The time column should be .*, a .* or a ")
  expect_error(x |> mt_set_time_column(letters))
  expect_error(x |> mt_set_time_column(2))
})
test_that("setting track id works as expected", {
  expect_identical(x |> mt_set_track_id("new_id") |> mt_n_tracks(), 4L)
  expect_identical(x |> mt_set_track_id("new_id_2") |> mt_n_tracks(), 3L)
  expect_error(x |> mt_set_track_id_column("new_id_2"), "The `track_data` does not have a column with the name ")

  expect_error(x |> mt_set_track_id_column("new"), "The argument `value` needs to be the name of a column in `x`")
  expect_error(
    x |> mt_set_track_id_column("date"),
    "Track id.s. should be of the type integer, integer64, character or factor."
  )
  m <- mt_sim_brownian_motion(t = 1:2, tracks = 3) |>
    mutate(rr = c(1, 1, 1, 1, 2, 2)) |>
    mt_set_track_data(data.frame(rr = c(1, 2, 2), track = 1:3))

  expect_error(mt_set_track_id_column(m, "rr"), "There are duplicated track identifiers in the new `track_id` column ..rr.. of the `track_data`")
})
test_that("length required", {
  expect_silent(mt_set_time(x, seq_len(nrow(x))))
  expect_error(
    mt_set_time(x, seq_len(nrow(x))[-1]),
    "The new `time` column should have the same length as the <move2> object."
  )
  expect_error(
    mt_set_time(x, seq_len(nrow(x) / 2)),
    "The new `time` column should have the same length as the <move2> object."
  )
  expect_silent(mt_set_track_id(x, seq_len(nrow(x))))
  expect_error(
    mt_set_track_id(x, seq_len(nrow(x))[-1]),
    "The new `track_id` column should have the same length as the <move2> object."
  )
  expect_error(
    mt_set_track_id(x, seq_len(nrow(x) / 2)),
    "The new `track_id` column should have the same length as the <move2> object."
  )
})
