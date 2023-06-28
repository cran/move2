m <- mt_sim_brownian_motion(tracks = letters[2:3], t = 1:10)
test_that("testing tidy functions", {
  skip_if_not_installed("dplyr")
  expect_s3_class(m, "move2")
  expect_s3_class(m |> filter(time < 3), "move2")
  expect_identical(m |> mt_track_data() |> nrow(), 2L)
  expect_identical(m |> filter(time < 3) |> mt_track_data() |> nrow(), 2L)
  expect_named(m, c("time", "track", "geometry"), ignore.order = TRUE)
  expect_named(m |> mutate(coll = time), c("time", "track", "geometry", "coll"), ignore.order = TRUE)
  expect_identical(m |> mutate(coll = time * 2) |> filter(track == "c") |> pull(coll), 2 * (1:10))
})
test_that("slice", {
  skip_if_not_installed("dplyr")
  expect_identical(m |> slice(1:4) |> mt_track_data() |> nrow(), 1L)
  expect_s3_class(m |> slice(1:4), "move2")
  expect_s3_class(m |> slice(1:4) |> mt_track_data(), "data.frame")
})
test_that("grouped_df class is retained on row slice", {
  skip_if_not_installed("dplyr")
  expect_true(m %>% group_by(track) %>% slice(1) %>% inherits("grouped_df"))
  expect_true(m %>% group_by(track) %>% filter(time < 3) %>% inherits("grouped_df"))
})
test_that("select retains columns", {
  skip_if_not_installed("dplyr")
  expect_true("track" %in% (m |> select(-track) |> names()))
  expect_true("time" %in% (m |> select(-time) |> names()))
  expect_true("time" %in% (m |> select(-track) |> names()))
  expect_true("track" %in% (m |> select(-time) |> names()))
  expect_true("track" %in% (m |> select(track) |> names()))
  expect_true("time" %in% (m |> select(time) |> names()))
  expect_true("time" %in% (m |> select(track) |> names()))
  expect_true("track" %in% (m |> select(time) |> names()))
  expect_true("track" %in% (m |> select(geometry) |> names()))
  expect_true("time" %in% (m |> select(geometry) |> names()))
})

test_that("select work on aditional columns", {
  skip_if_not_installed("dplyr")
  expect_named(m |> mutate(extra = time),
    c("time", "track", "extra", "geometry"),
    ignore.order = TRUE
  )
  expect_named(
    m |> mutate(extra = time) |>
      select(extra), c("extra", "geometry", "time", "track"),
    ignore.order = TRUE
  )
  expect_named(
    m |> mutate(extra = time) |>
      select(-extra), c("time", "track", "geometry"),
    ignore.order = TRUE
  )
  expect_named(
    m |> mutate(extra = time) |>
      select(track), c("track", "geometry", "time"),
    ignore.order = TRUE
  )
})
test_that("id column retained in track data", {
  expect_named(
    m |> select_track_data(-track) |> mt_track_data(), "track"
  )
})

test_that("selection of column without column name", {
  expect_identical(m %>% mutate(aa = time), m %>% mutate(aa = mt_time()))
  expect_identical(m %>% mutate(aa = track), m %>% mutate(aa = mt_track_id()))
  expect_identical(m %>% group_by(aa = track), m %>% group_by(aa = mt_track_id()))
  expect_identical(m %>% group_by(aa = time), m %>% group_by(aa = mt_time()))
  expect_error(
    mt_time(),
    "`mt_time..` can only be used without a `x` argument inside dplyr verbs"
  )
  expect_error(
    mt_track_id(),
    "`mt_track_id..` can only be used without a `x` argument inside dplyr verbs"
  )
})
test_that("dplyr methods trackid and time", {
  expect_identical(
    m %>%
      dplyr::mutate(aa = mt_track_id()),
    dplyr::mutate(m, aa = track)
  )
  expect_identical(
    m %>%
      dplyr::mutate(aa = mt_time()) %>%
      pull(aa),
    mt_time(m)
  )
  expect_identical(
    m %>%
      dplyr::mutate(aa = mt_track_id()) %>%
      pull(aa),
    mt_track_id(m)
  )
})

test_that("group_split", {
  skip_if_not_installed("dplyr")
  m <- mt_sim_brownian_motion()
  expect_equal(
    m %>%
      dplyr::group_by(mt_track_id()) %>%
      dplyr::group_split(.keep = FALSE),
    split(m, mt_track_id(m)),
    ignore_attr = TRUE
  )
})

test_that("grouping corresponds", {
  m <- mt_sim_brownian_motion() |>
    mutate(tt = time < 5) |>
    dplyr::tibble() |>
    mt_as_move2(time_column = "time", track_id_column = "track") %>%
    mutate_track_data(sex = letters[1:2])
  expect_identical(
    m |> group_by_track_data(sex),
    (m |> group_by(sex = sort(rep(letters[1:2], 10))))
  )
  expect_identical(
    m |> group_by(tt) |> group_by_track_data(sex, .add = TRUE),
    (m |> group_by(tt, sex = sort(rep(letters[1:2], 10))))
  )
  expect_identical(
    m |> group_by(tt) |> group_by_track_data(sex, .add = FALSE),
    (m |> group_by(sex = sort(rep(letters[1:2], 10))))
  )
})
