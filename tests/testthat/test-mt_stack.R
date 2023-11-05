test_that("mt_stack works", {
  expect_s3_class(
    mt_stack(
      mt_sim_brownian_motion(tracks = c("a")),
      mt_sim_brownian_motion(tracks = c("j", "h"))
    ), "move2"
  )
  expect_identical(
    mt_stack(
      mt_sim_brownian_motion(tracks = c("a")),
      mt_sim_brownian_motion(tracks = c("j", "h"))
    ) |>
      mt_track_id() |> unique(), c("a", "j", "h")
  )
  expect_warning(expect_identical(
    mt_stack(
      mt_sim_brownian_motion(tracks = c("a")) |> mt_set_track_id("asdf"),
      mt_sim_brownian_motion(tracks = c("j", "h"))
    ) |>
      mt_track_id() |>
      unique(), c("a", "j", "h")
  ))
  expect_error(
    mt_stack(
      mt_sim_brownian_motion(tracks = c("a")) |>
        mt_set_track_id("asdf"),
      mt_sim_brownian_motion(tracks = c("j", "h")) |>
        dplyr::mutate(asdf = "w")
    )
  )
})
test_that("mt_stack works with lists", {
  expect_identical(
    mt_stack(
      mt_sim_brownian_motion(t = 1:3, tracks = c("a"), sigma = 0),
      mt_sim_brownian_motion(tracks = c("j", "h"), sigma = 0)
    ),
    mt_stack(list(
      mt_sim_brownian_motion(t = 1:3, tracks = c("a"), sigma = 0),
      mt_sim_brownian_motion(tracks = c("j", "h"), sigma = 0)
    ))
  )
})
test_that("mt_stack doesnt change single object", {
  expect_identical(
    mt_stack(
      mt_sim_brownian_motion(tracks = c("j", "h"), sigma = 0)
    ),
    mt_stack(list(
      mt_sim_brownian_motion(tracks = c("j", "h"), sigma = 0)
    ))
  )
  m <- mt_read(mt_example())
  expect_identical(
    mt_stack(m),
    mt_stack(list(m))
  )
  expect_equal(
    mt_sim_brownian_motion(tracks = c("j", "h"), sigma = 0),
    mt_stack(list(
      mt_sim_brownian_motion(tracks = c("j", "h"), sigma = 0)
    )),
    ignore_attr = TRUE
  )
  expect_equal(
    m,
    mt_stack(list(m)),
    ignore_attr = TRUE
  )
  m <- mt_set_track_data(m, tibble::as_tibble(mt_track_data(m)))
  expect_identical(
    mt_stack(m),
    mt_stack(list(m))
  )
  expect_equal(
    m,
    mt_stack(list(m)),
    ignore_attr = TRUE
  )
})
test_that("mt_stack with different types", {
  suppressWarnings(expect_error(
    mt_stack(mt_sim_brownian_motion(tracks = letters), mt_read(mt_example())),
    "Can't combine `..1\\$time` <integer> and `..2\\$time` <datetime<UTC>>"
  ))
  suppressWarnings(expect_error(
    mt_stack(
      mt_sim_brownian_motion(t = as.POSIXct("1970-1-1") + 1:10),
      mt_read(mt_example())
    ),
    "arguments have different crs"
  ))
  a <- sf::st_set_crs(mt_sim_brownian_motion(
    t = as.POSIXct("1970-1-1") + 1:10,
    tracks = factor(letters[1:3])
  ), 4326)
  b <- mt_read(mt_example())
  suppressWarnings(expect_s3_class(
    mt_stack(
      a, b
    ),
    "move2"
  ))
  suppressWarnings(expect_warning(
    mt_stack(a, b),
    paste0(
      "The `track_id_column` differs between the objects to stack, for successfull stacking ",
      "all `track_id_column` attributes have been renamed to `track`"
    )
  ))
  mt_track_id(a) <- ("individual-local-identifier")
  suppressWarnings(expect_warning(
    mt_stack(a, b),
    paste0(
      "The `time_column` differs between the objects to stack, for successfull stacking ",
      "all `time_column` attributes have been renamed to `time`"
    )
  ))
  suppressWarnings(expect_error( # fail on different crs
    mt_stack(
      mt_sim_brownian_motion(
        t = as.POSIXct("1970-1-1") + 1:10,
        tracks = factor(letters[1:3])
      ),
      b
    )
  ))
})
test_that("duplicate individuals check_unique", {
  expect_s3_class(
    mt_stack(
      mt_sim_brownian_motion(tracks = letters[1:3]),
      mt_sim_brownian_motion(tracks = letters[4:6])
    ), "move2"
  )
  expect_error(
    mt_stack(
      mt_sim_brownian_motion(tracks = letters[1:3]),
      mt_sim_brownian_motion(tracks = letters[3:6])
    ),
    "There is a duplicated track identifier in the .* objects to combine \\(e.g. `c`)"
  )
  expect_error(
    mt_stack(
      mt_sim_brownian_motion(tracks = letters[1:3]),
      mt_sim_brownian_motion(tracks = letters[1:6])
    ),
    "There are duplicated track identifiers in the .* objects to combine \\(e.g. `a`, `b`, and `c`)"
  )
})

test_that("duplicate individuals merge", {
  expect_identical(
    mt_stack(mt_sim_brownian_motion(tracks = letters[1:3]),
      mt_sim_brownian_motion(tracks = letters[4:6]),
      .track_combine = "merge"
    ) |> mt_track_id(),
    rep(letters[1:6], each = 10)
  )
  expect_identical(
    mt_stack(
      mt_sim_brownian_motion(tracks = letters[1:3]) |>
        mutate_track_data(a = "a"),
      mt_sim_brownian_motion(tracks = letters[3:6]) |>
        mutate_track_data(a = "b"),
      .track_combine = "merge"
    ) |> mt_track_data(),
    structure(
      list(
        track = c("a", "b", "c", "d", "e", "f"),
        a = c("a", "a", NA, "b", "b", "b")
      ),
      row.names = c(NA, -6L), class = "data.frame"
    )
  )
})


test_that("duplicate individuals rename", {
  expect_identical(
    mt_stack(mt_sim_brownian_motion(tracks = letters[1:3]),
      mt_sim_brownian_motion(tracks = letters[4:6]),
      .track_combine = "rename"
    ) |> mt_track_id(),
    rep(letters[1:6], each = 10)
  )
  expect_silent(expect_identical(
    mt_stack(
      mt_sim_brownian_motion(tracks = letters[1:3]) |>
        mutate_track_data(a = "a"),
      mt_sim_brownian_motion(tracks = letters[3:6]) |>
        mutate_track_data(a = "b"),
      .track_combine = "rename", .track_id_repair = "unique_quiet"
    ) |> mt_track_data(),
    structure(list(
      track = c("a", "b", "c...3", "c...4", "d", "e", "f"),
      a = c(
        "a",
        "a", "a", "b", "b", "b", "b"
      )
    ), row.names = c(NA, -7L), class = "data.frame")
  ))
})
test_that("test different track columns", {
  a <- mt_sim_brownian_motion(tracks = factor(letters[1:3]))
  b <- mt_sim_brownian_motion(tracks = 4:6)
  e <- mt_sim_brownian_motion(tracks = (10:12))
  e <- mt_set_track_id(e, as.integer64(mt_track_id(e)))
  d <- mt_sim_brownian_motion(tracks = LETTERS[7:9])
  expect_identical(mt_stack(a, b) |> mt_track_id(), factor(c(rep(letters[1:3], each = 10), rep(4:6, each = 10)),
    levels = c("a", "b", "c", as.character(4:6))
  ))
  expect_identical(
    mt_stack(b, a) |> mt_track_id(),
    factor(c(rep(4:6, each = 10), rep(letters[1:3], each = 10)))
  )

  expect_identical(mt_stack(a, d) |> mt_track_id(), (c(rep(letters[1:3], each = 10), rep(LETTERS[7:9], each = 10))))
  expect_identical(
    mt_stack(d, a) |> mt_track_id(),
    (c(rep(LETTERS[7:9], each = 10), rep(letters[1:3], each = 10)))
  )
  expect_identical(mt_stack(b, d, a) |> mt_track_id(), (c(rep(4:6, each = 10), rep(LETTERS[7:9], each = 10), rep(letters[1:3], each = 10))))
  expect_identical(mt_stack(d, a, b) |> mt_track_id(), (c(rep(LETTERS[7:9], each = 10), rep(letters[1:3], each = 10), rep(4:6, each = 10))))
  expect_identical(mt_stack(e, b, d, a) |> mt_track_id(), (c(rep(10:12, each = 10), rep(4:6, each = 10), rep(LETTERS[7:9], each = 10), rep(letters[1:3], each = 10))))
  expect_identical(mt_stack(d, a, b, e) |> mt_track_id(), (c(rep(LETTERS[7:9], each = 10), rep(letters[1:3], each = 10), rep(4:6, each = 10), rep(10:12, each = 10))))
})
