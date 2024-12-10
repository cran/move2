test_that("subset function behaves as expected", {
  for (i in 0:2) {
    d <- data.frame(a = 1L, b = c(1L, NA), d = c(NA, 1L), e = 1L:4L, f = 1L:2L)
    d <- d[c(seq_len(i), seq_len(nrow(d))), ]
    t <- mt_sim_brownian_motion(tracks = 1L, t = rep(1L, 4L + i))
    d <- bind_cols(t, d)

    for (j in seq_len(nrow(d)))
    {
      expect_identical(slice_subsets(j, d), j)
    }
    expect_identical(slice_subsets(i + 1L:2L, d[, 3L:4L]), i + 1L)
    expect_identical(slice_subsets(i + 3L:4L, d[, 3L:4L]), i + 3L)
    expect_identical(slice_subsets(i + 1L:2L, d[, c(5L, 3L)]), i + 2L)
    expect_identical(slice_subsets(i + 3L:4L, d[, c(5L, 3L)]), i + 4L)
    expect_identical(slice_subsets(i + 1L:2L, d[, 3L:5L]), i + 1L:2L)
    expect_identical(slice_subsets(i + 3L:4L, d[, 3L:5L]), i + 3L:4L)
    expect_identical(slice_subsets(i + 1L:2L, d[, c(6L, 3L)]), i + 1L:2L)
    expect_identical(slice_subsets(i + 3L:4L, d[, c(6L, 3L)]), i + 3L:4L)
    expect_identical(slice_subsets(i + 1L:2L, d[, c(6L, 7L)]), i + 1L:2L)
    expect_identical(slice_subsets(i + 3L:4L, d[, c(6L, 7L)]), i + 3L:4L)
    expect_identical(slice_subsets(i + 1L:4L, d[, c(4L, 7L)]), i + 1L:2L)
    expect_identical(slice_subsets(i + 1L:4L, d[, c(5L, 7L)]), i + 1L:2L)
    expect_identical(slice_subsets(i + 1L:4L, d[, c(3L, 6L)]), i + 1L:4L)
    expect_identical(slice_subsets(i + 1L:4L, d[, c(3L, 7L)]), i + 1L:2L)
    expect_identical(slice_subsets(i + 1L:4L, d[, c(3L, 4L)]), i + 1L)
    expect_identical(slice_subsets(i + 1L:4L, d[, c(3L, 5L)]), i + 2L)
  }
})
test_that("Filtering with different methods", {
  m <- bind_cols(mt_sim_brownian_motion(1L:3L), aa = 6L:1L, bb = 12L:7L)
  rownames(m) <- NULL
  expect_identical(m, `rownames<-`(mt_filter_unique(m[sort(c(1L:6L, 1L:3L)), ]), 1L:6L))
  expect_identical(
    m,
    `rownames<-`(mt_filter_unique(m[sort(c(1L:6L, 1L:3L)), ],
      criterion = "sample"
    ), 1L:6L)
  )
  mm <- m[(c(1L:6L, 1L:3L)), ]
  mm$aa[7L:9L] <- NA
  expect_identical(m, `rownames<-`(mt_filter_unique(mm), 1L:6L))

  mm <- m[(c(1L:6L, 1L:3L)), ]
  mm$aa[1L:3L] <- NA
  expect_identical(m[c(4L:6L, 1L:3L), ], `rownames<-`(mt_filter_unique(mm), c(4L:6L, 1L:3L)))
  mm <- m[(c(1L:6L, 1L:3L)), ]
  mm$aa[1:3] <- NA
  mm$bb[7:9] <- NA
  expect_warning(expect_identical(mm, mt_filter_unique(mm)))
})
test_that("Filtering with additional columns", {
  m <- mt_sim_brownian_motion(1L:2L)[rep(1L:4L, 4), ]
  m$aa <- as.character(gl(2, 4))
  # COMBAK remove as.character if this fix is propagated: https://github.com/r-spatial/sf/issues/2138
  m$bb <- as.character(gl(2, 8))
  expect_identical(nrow(mt_filter_unique(select(m, -aa, -bb))), 4L)
  expect_warning(expect_identical(nrow(mt_filter_unique(m)), 16L))
  expect_silent(expect_identical(nrow(mt_filter_unique(dplyr::select(m, -bb), additional_columns = aa)), 8L))
  expect_silent(expect_identical(nrow(mt_filter_unique(dplyr::select(m, -aa),
    additional_columns = across(all_of("bb"))
  )), 8L))

  expect_silent(expect_identical(nrow(mt_filter_unique(dplyr::select(m, -aa), additional_columns = bb)), 8L))
  expect_silent(expect_identical(nrow(mt_filter_unique(m[c(1:16, 1:14), ],
    additional_columns = across(all_of(c("aa", "bb")))
  )), 16L))
  m$bb[1:12] <- NA
  expect_warning(expect_identical(nrow(mt_filter_unique(m)), 8L))
})

test_that("Resulting records are unique", {
  m <- mt_sim_brownian_motion(1L:2L)[rep(1L:4L, 4), ]
  expect_true(mt_has_unique_location_time_records(mt_filter_unique(m, criterion = "sample")))
})


test_that("first and last work", {
  m <- mt_sim_brownian_motion(1L:2L)[rep(1L:4L, 4), ]
  expect_identical(mt_unique(m, "first") |> which(), 1L:4L)
  expect_identical(mt_unique(m, "last") |> which(), 13:16)
})

test_that("criterion errors", {
  m <- mt_sim_brownian_motion(1L:2L)[rep(1L:4L, 4L), ]
  expect_error(mt_filter_unique(m, "asdf"), '`criterion` must be one of "subsets", "subsets_equal", "sample", "first", or "last", not "asdf".')
  expect_error(mt_filter_unique(m, "s"), '`criterion` must be one of "subsets", "subsets_equal", "sample", "first", or "last", not "s".')
  expect_error(mt_filter_unique(m, "sa"), 'Did you mean "sample"')
  expect_error(mt_filter_unique(m, 1L), "`criterion` must be a character vector, not the number 1.")
})


test_that("equivalance fun", {
  m <- mt_sim_brownian_motion(1L:2L, tracks = 1)[c(1, 1L:2L), ]
  m$g <- c(1, NA, 1)
  expect_identical(nrow(mt_filter_unique(m, "subsets")), 2L)
  expect_identical(nrow(mt_filter_unique(m, "subsets_equal")), 2L)

  m$geometry[[1]] <- sf::st_point(c(0, 0.00000000001))
  expect_warning(expect_identical(nrow(mt_filter_unique(m, "subsets")), 3L))
  expect_identical(nrow(mt_filter_unique(m, "subsets_equal")), 2L)
  expect_warning(expect_identical(nrow(mt_filter_unique(m, "subsets_equal", tolerance = 10^-19)), 3L))

  m <- mt_sim_brownian_motion(1L:2L, tracks = 1L)[c(1L, 1L:2L), ]
  m$g <- c(1, 1.00000000001, 1)

  expect_warning(expect_identical(nrow(mt_filter_unique(m, "subsets")), 3L))
  expect_identical(nrow(mt_filter_unique(m, "subsets_equal")), 2L)
  expect_warning(expect_identical(nrow(mt_filter_unique(m, "subsets_equal", tolerance = 10^-19)), 3L))


  m <- mt_sim_brownian_motion(1L:2L, tracks = 1L)[c(1L, 1L, 1L:2L), ]
  m$g <- c(1, 1 - 0.00000001, 1.000000000001, 1)

  expect_warning(expect_identical(nrow(mt_filter_unique(m, "subsets")), 4L))
  expect_identical(nrow(mt_filter_unique(m, "subsets_equal")), 2L)
  expect_warning(expect_identical(nrow(mt_filter_unique(m, "subsets_equal", tolerance = 10^-19)), 4L))
  expect_warning(expect_identical(nrow(mt_filter_unique(m, "subsets_equal", tolerance = 10^-10)), 3L))
})
