sim_data <- mt_sim_brownian_motion()
sim_data$sex <- "female"
test_that("colum moving from events", {
  expect_named(sim_data, c("time", "track", "geometry", "sex"))
  expect_named(
    sim_data |> mt_as_track_attribute(sex),
    c("time", "track", "geometry")
  )
  expect_error(
    sim_data |> mt_as_track_attribute(sex, time),
    "The attributes to move do not have a unique value per individual"
  )
  expect_named(
    sim_data |> mt_as_track_attribute(sex, track),
    c("time", "track", "geometry")
  )
  expect_named(
    sim_data |> mt_as_track_attribute(track),
    c("time", "track", "geometry", "sex")
  )
  expect_named(sim_data |> mt_track_data(), c("track"))
  expect_named(
    sim_data |> mt_as_track_attribute(sex) |>
      mt_track_data(),
    c("sex", "track")
  )
})
test_that("round trip", {
  expect_identical(
    sim_data |> mt_as_track_attribute(sex) |>
      mt_as_event_attribute(sex),
    sim_data[, c(1, 2, 4, 3)]
  )
  expect_identical(
    sim_data |>
      mutate_track_data(name = letters[seq_len(mt_n_tracks(sim_data))]) |>
      mt_as_event_attribute(name) |> mt_as_track_attribute(name),
    sim_data[, c(1, 2, 4, 3)] |>
      mutate_track_data(name = letters[seq_len(mt_n_tracks(sim_data))]) |>
      select_track_data(name, track)
  )
})
test_that("colum moving from track data", {
  expect_named(
    sim_data |> mutate_track_data(age = 3:4) |>
      mt_as_event_attribute(age),
    c("time", "track", "geometry", "age", "sex"),
    ignore.order = TRUE
  )
  expect_named(
    sim_data |> mutate_track_data(age = 3:4) |>
      mt_as_event_attribute(age, track),
    c("time", "track", "geometry", "age", "sex"),
    ignore.order = TRUE
  )
  expect_named(
    sim_data |> mutate_track_data(age = 3:4) |>
      mt_as_event_attribute(track),
    c("time", "track", "geometry", "sex"),
    ignore.order = TRUE
  )
  expect_named(
    sim_data |> mutate_track_data(age = 3:4) |>
      mt_as_event_attribute(age) |> mt_track_data(),
    c("track")
  )
  expect_named(
    sim_data |> mutate_track_data(age = 3:4) |>
      mt_as_event_attribute(age, track) |> mt_track_data(),
    c("track")
  )
  expect_named(
    sim_data |> mutate_track_data(age = 3:4) |>
      mt_as_event_attribute(track) |> mt_track_data(),
    c("track", "age"),
    ignore.order = TRUE
  )
})
