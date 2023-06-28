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
