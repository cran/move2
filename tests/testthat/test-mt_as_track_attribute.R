sim_data <- mt_sim_brownian_motion()
sim_data$sex <- "female"
test_that("colum moving from events", {
  expect_named(sim_data, c("time", "track", "geometry", "sex"))
  expect_named(
    sim_data |> mt_as_track_attribute(sex),
    c("time", "track", "geometry")
  )
  expect_named(
    sim_data |> mt_as_track_attribute(sex, .keep = TRUE),
    c("time", "track", "geometry", "sex")
  )
  expect_named(
    sim_data |> mt_as_track_attribute(sex, .keep = TRUE) |> mt_track_data(),
    c("track", "sex")
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
    c("track", "sex")
  )
})
test_that("round trip", {
  expect_identical(
    sim_data |> mt_as_track_attribute(sex) |>
      mt_as_event_attribute(sex),
    sim_data[, c(1L, 2L, 4L, 3L)]
  )
  expect_identical(
    sim_data |>
      mutate_track_data(name = letters[seq_len(mt_n_tracks(sim_data))]) |>
      mt_as_event_attribute(name) |> mt_as_track_attribute(name),
    sim_data[, c(1L, 2L, 4L, 3L)] |>
      mutate_track_data(name = letters[seq_len(mt_n_tracks(sim_data))])
  )
})
test_that("mt_as_track_attribute is order resistant", {
  dat <- mt_sim_brownian_motion(1L:3L, tracks = letters[1L:3L]) |>
    mutate_track_data(d = 3L:5L, b = c(4L, 7L, 8L))
  dat2 <- dat |> mt_as_event_attribute("b")
  expect_identical(
    mt_track_data(dat),
    dat2[rev(seq_len(nrow(dat2))), ] |> mt_as_track_attribute("b") |> mt_track_data()
  )
})
test_that("colum moving from track data", {
  expect_named(
    sim_data |> mutate_track_data(age = 3L:4L) |>
      mt_as_event_attribute(age),
    c("time", "track", "geometry", "age", "sex"),
    ignore.order = TRUE
  )
  expect_named(
    sim_data |> mutate_track_data(age = 3L:4L) |>
      mt_as_event_attribute(age, track),
    c("time", "track", "geometry", "age", "sex"),
    ignore.order = TRUE
  )
  expect_named(
    sim_data |> mutate_track_data(age = 3L:4L) |>
      mt_as_event_attribute(track),
    c("time", "track", "geometry", "sex"),
    ignore.order = TRUE
  )
  expect_named(
    sim_data |> mutate_track_data(age = 3L:4L) |>
      mt_as_event_attribute(age) |> mt_track_data(),
    c("track")
  )
  expect_named(
    sim_data |> mutate_track_data(age = 3L:4L) |>
      mt_as_event_attribute(age, .keep = TRUE) |> mt_track_data(),
    c("track", "age")
  )
  expect_named(
    sim_data |> mutate_track_data(age = 3L:4L) |>
      mt_as_event_attribute(age, .keep = TRUE),
    c("time", "track", "sex", "age", "geometry")
  )
  expect_named(
    sim_data |> mutate_track_data(age = 3L:4L) |>
      mt_as_event_attribute(age, track) |> mt_track_data(),
    c("track")
  )
  expect_named(
    sim_data |> mutate_track_data(age = 3L:4L) |>
      mt_as_event_attribute(track) |> mt_track_data(),
    c("track", "age"),
    ignore.order = TRUE
  )
})
test_that("columns get removed from event data", {
  expect_true("sex" %in% colnames(sim_data))
  expect_false("sex" %in% colnames(mt_as_track_attribute(sim_data, "sex")))
  expect_false("sex" %in% colnames(mt_as_track_attribute(sim_data, starts_with("s"))))
  ss <- mt_sim_brownian_motion()
  ss$age <- 5L
  ss$sex <- "f"
  expect_false(any(c("age", "sex") %in% colnames((mt_as_track_attribute(ss, all_of(c("age", "sex")))))))
})
test_that("columns get removed from track data", {
  ss <- mt_sim_brownian_motion() |> mutate_track_data(age = 5L, parent = "a")
  expect_true("age" %in% colnames(mt_track_data(ss)))
  expect_false("age" %in% colnames(mt_track_data(mt_as_event_attribute(ss, "age"))))
  expect_false("age" %in% colnames(mt_track_data(mt_as_event_attribute(ss, starts_with("a")))))
  expect_false("age" %in% colnames(mt_track_data(mt_as_event_attribute(ss, all_of("age")))))
  expect_false(any(c("age", "parent") %in%
    colnames(mt_track_data(mt_as_event_attribute(
      ss,
      all_of(c("age", "parent"))
    )))))
})
