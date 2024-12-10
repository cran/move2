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
      mt_track_id() |>
      unique(), c("a", "j", "h")
  )
  expect_warning(expect_identical(
    mt_stack(
      mt_sim_brownian_motion(tracks = c("a")) |>
        mt_set_track_id("asdf"),
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
      mt_sim_brownian_motion(t = 1L:3L, tracks = c("a"), sigma = 0),
      mt_sim_brownian_motion(tracks = c("j", "h"), sigma = 0)
    ),
    mt_stack(list(
      mt_sim_brownian_motion(t = 1L:3L, tracks = c("a"), sigma = 0),
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
  m <- mt_read(mt_example(), n_max = 3000)
  expect_identical(
    mt_stack(m),
    mt_stack(list(m))
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
    m[TRUE, ], # realize to prevent error
    mt_stack(list(m)),
    ignore_attr = TRUE
  )
  m <- mt_set_track_data(m, tibble::as_tibble(mt_track_data(m)))
  expect_identical(
    mt_stack(m),
    mt_stack(list(m))
  )
  expect_equal(
    m[TRUE, ], # realize to prevent error
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
    tracks = factor(letters[1L:3L])
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
        tracks = factor(letters[1L:3L])
      ),
      b
    )
  ))
})
test_that("duplicate individuals check_unique", {
  expect_s3_class(
    mt_stack(
      mt_sim_brownian_motion(tracks = letters[1L:3L]),
      mt_sim_brownian_motion(tracks = letters[4L:6L])
    ), "move2"
  )
  expect_error(
    mt_stack(
      mt_sim_brownian_motion(tracks = letters[1L:3L]),
      mt_sim_brownian_motion(tracks = letters[3L:6L])
    ),
    "There is a duplicated track identifier in the .* objects to combine \\(e.g. `c`)"
  )
  expect_error(
    mt_stack(
      mt_sim_brownian_motion(tracks = letters[1L:3L]),
      mt_sim_brownian_motion(tracks = letters[1:6])
    ),
    "There are duplicated track identifiers in the .* objects to combine \\(e.g. `a`, `b`, and `c`)"
  )
})

test_that("duplicate individuals merge", {
  expect_identical(
    mt_stack(mt_sim_brownian_motion(tracks = letters[1L:3L]),
      mt_sim_brownian_motion(tracks = letters[4L:6L]),
      .track_combine = "merge"
    ) |> mt_track_id(),
    rep(letters[1:6], each = 10)
  )
  expect_warning(
    mt_stack(
      mt_sim_brownian_motion(tracks = letters[1L:3L]) |>
        mutate_track_data(a = "a"),
      mt_sim_brownian_motion(tracks = letters[3L:6L]) |>
        mutate_track_data(a = "b"),
      .track_combine = "merge"
    ), "The column.*, does not have one unique value per track, therefore these values are replaced by .NA.."
  )

  expect_identical(
    suppressWarnings(mt_stack(
      mt_sim_brownian_motion(tracks = letters[1L:3L]) |>
        mutate_track_data(a = "a"),
      mt_sim_brownian_motion(tracks = letters[3L:6L]) |>
        mutate_track_data(a = "b"),
      .track_combine = "merge"
    ) |> mt_track_data()), structure(
      list(
        track = c("a", "b", "c", "d", "e", "f"),
        a = c("a", "a", NA, "b", "b", "b")
      ),
      row.names = c(NA, -6L), class = "data.frame"
    )
  )

  expect_identical(
    mt_stack(
      mt_sim_brownian_motion(tracks = letters[1L:3L]) |>
        mutate_track_data(a = "a"),
      mt_sim_brownian_motion(tracks = letters[3L:6L]) |>
        mutate_track_data(a = "b"),
      .track_combine = "merge_list"
    ) |> mt_track_data(),
    structure(
      list(
        track = c("a", "b", "c", "d", "e", "f"),
        a = list("a", "a", c("a", "b"), "b", "b", "b")
      ),
      row.names = c(NA, -6L), class = "data.frame"
    )
  )
})


test_that("duplicate individuals rename", {
  expect_identical(
    mt_stack(mt_sim_brownian_motion(tracks = letters[1L:3L]),
      mt_sim_brownian_motion(tracks = letters[4L:6L]),
      .track_combine = "rename"
    ) |> mt_track_id(),
    rep(letters[1:6], each = 10)
  )
  expect_silent(expect_identical(
    mt_stack(
      mt_sim_brownian_motion(tracks = letters[1L:3L]) |>
        mutate_track_data(a = "a"),
      mt_sim_brownian_motion(tracks = letters[3L:6L]) |>
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
test_that("merge_list indviduals track data", {
  a <- mt_sim_brownian_motion(1L:3L, tracks = c("a", "b")) |> mutate_track_data(sex = c("f", "m"), age = 4:5)
  b <- a |> mutate(time = time + 4)
  expect_identical(
    mt_stack(a, b, .track_combine = "merge_list") |> mt_track_data(),
    structure(list(track = c("a", "b"), sex = list(c("f", "f"), c("m", "m")), age = list(c(4L, 4L), c(5L, 5L))),
      row.names = c(NA, -2L), class = "data.frame"
    )
  )
  expect_identical(
    mt_stack(a, b |>
      filter_track_data(.track_id = "b"), .track_combine = "merge_list") |>
      mt_track_data(),
    structure(list(track = c("a", "b"), sex = list(c("f"), c("m", "m")), age = list(c(4L), c(5L, 5L))),
      row.names = c(NA, -2L), class = "data.frame"
    )
  )
  expect_identical(
    mt_stack(a, b |>
      filter_track_data(.track_id = "a"),
    .track_combine = "merge_list"
    ) |>
      mt_track_data(),
    structure(
      list(
        track = c("a", "b"),
        sex = list(c("f", "f"), c("m")), age = list(c(4L, 4L), c(5L))
      ),
      row.names = c(NA, -2L), class = "data.frame"
    )
  )
  expect_identical(
    mt_stack(a, b |> filter_track_data(.track_id = "a") |>
      mutate_track_data(extra = "o"), .track_combine = "merge_list") |> mt_track_data(),
    structure(
      list(
        track = c("a", "b"), sex = list(c("f", "f"), c("m")),
        age = list(c(4L, 4L), c(5L)), extra = list(c(NA, "o"), NA_character_)
      ),
      row.names = c(NA, -2L), class = "data.frame"
    )
  )
  expect_identical(
    mt_stack(a |> filter_track_data(.track_id = "b"),
      b |>
        filter_track_data(.track_id = "a"),
      .track_combine = "merge_list"
    ) |>
      mt_track_data(),
    structure(
      list(
        track = c("b", "a"),
        sex = list(c("m"), c("f")),
        age = list(c(5L), c(4L))
      ),
      row.names = c(NA, -2L), class = "data.frame"
    )
  )
})


test_that("merge indviduals track data", {
  a <- mt_sim_brownian_motion(1L:3L, tracks = c("a", "b")) |>
    mutate_track_data(sex = c("f", "m"), age = 4:5)
  b <- a |> mutate(time = time + 4)
  res <- structure(list(track = c("a", "b"), sex = c("f", "m"), age = 4:5),
    row.names = c(NA, -2L), class = "data.frame"
  )
  expect_identical(
    mt_stack(a, b, .track_combine = "merge") |> mt_track_data(),
    res
  )
  expect_identical(
    mt_stack(a,
      b |>
        filter_track_data(.track_id = "b"),
      .track_combine = "merge"
    ) |>
      mt_track_data(),
    res
  )
  expect_identical(
    mt_stack(a,
      b |>
        filter_track_data(.track_id = "a"),
      .track_combine = "merge"
    ) |>
      mt_track_data(),
    res
  )
})

test_that("test different track columns", {
  a <- mt_sim_brownian_motion(tracks = factor(letters[1L:3L]))
  b <- mt_sim_brownian_motion(tracks = 4L:6L)
  e <- mt_sim_brownian_motion(tracks = (10:12))
  e <- mt_set_track_id(e, as.integer64(mt_track_id(e)))
  d <- mt_sim_brownian_motion(tracks = LETTERS[7L:9L])
  expect_identical(
    mt_stack(a, b) |> mt_track_id(),
    factor(
      c(
        rep(letters[1L:3L], each = 10),
        rep(4L:6L, each = 10)
      ),
      levels = c("a", "b", "c", as.character(4L:6L))
    )
  )
  expect_identical(
    mt_stack(b, a) |> mt_track_id(),
    factor(c(rep(4L:6L, each = 10L), rep(letters[1L:3L], each = 10L)))
  )

  expect_identical(
    mt_stack(a, d) |> mt_track_id(),
    (c(
      rep(letters[1L:3L], each = 10L),
      rep(LETTERS[7L:9L], each = 10)
    ))
  )
  expect_identical(
    mt_stack(d, a) |> mt_track_id(),
    (c(rep(LETTERS[7L:9L], each = 10L), rep(letters[1L:3L], each = 10L)))
  )
  expect_identical(mt_stack(b, d, a) |> mt_track_id(), (c(rep(4L:6L, each = 10), rep(LETTERS[7L:9L], each = 10), rep(letters[1L:3L], each = 10))))
  expect_identical(mt_stack(d, a, b) |> mt_track_id(), (c(rep(LETTERS[7L:9L], each = 10), rep(letters[1L:3L], each = 10), rep(4L:6L, each = 10))))
  expect_identical(mt_stack(e, b, d, a) |> mt_track_id(), (c(rep(10:12, each = 10), rep(4L:6L, each = 10), rep(LETTERS[7L:9L], each = 10), rep(letters[1L:3L], each = 10))))
  expect_identical(mt_stack(d, a, b, e) |> mt_track_id(), (c(rep(LETTERS[7L:9L], each = 10), rep(letters[1L:3L], each = 10), rep(4L:6L, each = 10), rep(10:12, each = 10))))
})
test_that("conflicting time columns", {
  expect_error(
    mt_stack(mt_sim_brownian_motion(tracks = "a"), mt_sim_brownian_motion(tracks = "b") |>
      mutate(new_time = time * 3.0) |>
      mt_set_time_column("new_time")),
    "The .time_column. differs between the objects to stack and renaming would overwrite existing data."
  )
})
test_that("merge stacking lists works", {
  m <- mt_sim_brownian_motion(1L:3L, tracks = letters[5L:8L]) |>
    mutate(bb = c(rep("a", 6L), rep("b", 6L))) |>
    mutate_track_data(sex = c("f", "f", "m", "m"), age = c(4, 4, 5, 6), old_track = track)
  aa <- lapply(split(m, mt_track_id(m)), \(x) mt_set_track_id(x, "bb"))
  expect_identical(
    mt_stack(aa[[1L]], aa[[2L]], aa[[3L]], aa[[4L]], .track_combine = "merge_list"),
    mt_stack(aa, .track_combine = "merge_list")
  )
  expect_identical(
    suppressWarnings(mt_stack(aa[[1L]], aa[[2L]], aa[[3L]], aa[[4L]], .track_combine = "merge")),
    suppressWarnings(mt_stack(aa, .track_combine = "merge"))
  )
})
