test_that("printing", {
  expect_output(mt_sim_brownian_motion() |> print(), "Track features:")
  expect_output(mt_sim_brownian_motion(tracks = 3) |> print(), "First 10 features:\n   time track", fixed = TRUE)
  expect_output(mt_sim_brownian_motion(tracks = 3) |> print(), "1 *1 *1 *POINT .0 0.")
})
test_that("print line reduction works (data.frame)", {
  expect_identical(
    mt_sim_brownian_motion(tracks = 3) |> print() |> capture_output_lines() |> length(),
    mt_sim_brownian_motion(tracks = 3) |> print(n = 4) |> capture_output_lines() |> length() + 6L
  )
  expect_identical(
    mt_sim_brownian_motion(tracks = 3) |> print() |> capture_output_lines() |> length(),
    mt_sim_brownian_motion(tracks = 3) |> print(n = 2) |> capture_output_lines() |> length() + 9L
  )
  expect_identical(
    mt_sim_brownian_motion(tracks = 12) |> print() |> capture_output_lines() |> length(),
    mt_sim_brownian_motion(tracks = 12) |> print(n = 2) |> capture_output_lines() |> length() + 16L
  )
  expect_output(
    mt_sim_brownian_motion(tracks = 5) |> print(n = 3), "First 3 track features:"
  )
  expect_output(
    mt_sim_brownian_motion(tracks = 8) |> print(n = 8), "Track features:"
  )
  expect_output(
    mt_sim_brownian_motion(tracks = 11) |> print(), "First 10 track features:"
  )
})
test_that("print line reduction works (tibble)", {
  m <- mt_read(mt_example()) |> select_track_data(-`study-name`)
  mm <- mt_set_track_data(m, dplyr::as_tibble(mt_track_data(m)))
  expect_identical(
    m |> print() |> capture_output_lines() |> length(),
    m |> print(n = 8) |> capture_output_lines() |> length() + 2L
  )
  expect_identical(
    m |> print() |> capture_output_lines() |> length(),
    m |> print(n = 9) |> capture_output_lines() |> length() + 1L
  )
  expect_gt(
    m |> print() |> capture_output_lines() |> length(),
    m |> print(n = 4) |> capture_output_lines() |> length() + 6
    # Printing seems dependent on interactive session therefore for flexible test
  )
  for (i in list(m, mm))
  {
    expect_output(
      i |> print(n = 3), "First 3 track features:"
    )
    expect_output(
      i |> print(n = 8), "Track features:"
    )
    expect_output(
      i |> print(n = 8), "M1"
    )
    expect_no_match(
      i |> print(n = 3) |> capture.output(), "M1"
    )
  }
})
test_that("check header", {
  m <- mt_read(mt_example())
  expect_no_match(
    m[1:10, ] |> print(n = 3) |> capture.output(), "on averag"
  )
  expect_match(
    m |> print(n = 3) |> capture.output(), "on aver",
    all = FALSE
  )
  expect_match(
    m[1:10, ] |> print(n = 3) |> capture.output(), format(diff(range(mt_time(m)[1:10])), digits = 3),
    all = FALSE
  )

  expect_match(
    m |> print(n = 3) |> capture.output() |> head(1), paste0("column. \"", mt_track_id_column(m))
  )
  expect_match(
    m |> print(n = 3) |> capture.output() |> head(1), paste0("column. \"", mt_time_column(m))
  )
})
