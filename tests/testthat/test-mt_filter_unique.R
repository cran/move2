test_that("subset function behaves as expected", {
  for (i in 0:2) {
    d <- data.frame(a = 1, b = c(1, NA), d = c(NA, 1), e = 1:4, f = 1:2)
    d <- d[c(seq_len(i), seq_len(nrow(d))), ]
    t <- mt_sim_brownian_motion(tracks = 1, t = rep(1, 4 + i))
    d <- bind_cols(t, d)

    for (j in seq_len(nrow(d)))
    {
      expect_identical(slice_subsets(j, d), j)
    }
    expect_identical(slice_subsets(i + 1:2, d[, 3:4]), i + 1L)
    expect_identical(slice_subsets(i + 3:4, d[, 3:4]), i + 3L)
    expect_identical(slice_subsets(i + 1:2, d[, c(5, 3)]), i + 2L)
    expect_identical(slice_subsets(i + 3:4, d[, c(5, 3)]), i + 4L)
    expect_identical(slice_subsets(i + 1:2, d[, 3:5]), i + 1:2)
    expect_identical(slice_subsets(i + 3:4, d[, 3:5]), i + 3:4)
    expect_identical(slice_subsets(i + 1:2, d[, c(6, 3)]), i + 1:2)
    expect_identical(slice_subsets(i + 3:4, d[, c(6, 3)]), i + 3:4)
    expect_identical(slice_subsets(i + 1:2, d[, c(6, 7)]), i + 1:2)
    expect_identical(slice_subsets(i + 3:4, d[, c(6, 7)]), i + 3:4)
    expect_identical(slice_subsets(i + 1:4, d[, c(4, 7)]), i + 1:2)
    expect_identical(slice_subsets(i + 1:4, d[, c(5, 7)]), i + 1:2)
    expect_identical(slice_subsets(i + 1:4, d[, c(3, 6)]), i + 1:4)
    expect_identical(slice_subsets(i + 1:4, d[, c(3, 7)]), i + 1:2)
    expect_identical(slice_subsets(i + 1:4, d[, c(3, 4)]), i + 1L)
    expect_identical(slice_subsets(i + 1:4, d[, c(3, 5)]), i + 2L)
  }
})
test_that("Filtering with different methods", {
  m <- bind_cols(mt_sim_brownian_motion(1:3), aa = 6:1, bb = 12:7)
  rownames(m) <- NULL
  expect_identical(m, `rownames<-`(mt_filter_unique(m[sort(c(1:6, 1:3)), ]), 1:6))
  expect_identical(
    m,
    `rownames<-`(mt_filter_unique(m[sort(c(1:6, 1:3)), ],
      criterion = "sample"
    ), 1:6)
  )
  mm <- m[(c(1:6, 1:3)), ]
  mm$aa[7:9] <- NA
  expect_identical(m, `rownames<-`(mt_filter_unique(mm), 1:6))

  mm <- m[(c(1:6, 1:3)), ]
  mm$aa[1:3] <- NA
  expect_identical(m[c(4:6, 1:3), ], `rownames<-`(mt_filter_unique(mm), c(4:6, 1:3)))
  mm <- m[(c(1:6, 1:3)), ]
  mm$aa[1:3] <- NA
  mm$bb[7:9] <- NA
  expect_warning(expect_identical(mm, mt_filter_unique(mm)))
})
test_that("Filtering with additional columns", {
  m <- mt_sim_brownian_motion(1:2)[rep(1:4, 4), ]
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
  m <- mt_sim_brownian_motion(1:2)[rep(1:4, 4), ]
  expect_true(mt_has_unique_location_time_records(mt_filter_unique(m, criterion = "sample")))
})


test_that("first and last work", {
  m <- mt_sim_brownian_motion(1:2)[rep(1:4, 4), ]
  expect_identical(mt_unique(m, "first") |> which(), 1:4)
  expect_identical(mt_unique(m, "last") |> which(), 13:16)
})

test_that("criterion errors", {
  m <- mt_sim_brownian_motion(1:2)[rep(1:4, 4), ]
  expect_error(mt_filter_unique(m, "asdf"), '`criterion` must be one of "subsets", "sample", "first", or "last", not "asdf".')
  expect_error(mt_filter_unique(m, "s"), '`criterion` must be one of "subsets", "sample", "first", or "last", not "s".')
  expect_error(mt_filter_unique(m, "sa"), 'Did you mean "sample"')
  expect_error(mt_filter_unique(m, 1L), "`criterion` must be a character vector, not the number 1.")
})
