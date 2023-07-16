test_that("visible testing", {
  t <- c(TRUE, FALSE, NA)
  d <- expand.grid(
    "algorithm-marked-outlier" = t,
    "manually-marked-outlier" = t,
    "import-marked-outlier" = t,
    "manually-marked-valid" = t
  )
  exp <- c(
    rep(TRUE, 27), rep(FALSE, 13), TRUE, TRUE, FALSE, TRUE, TRUE,
    rep(FALSE, 4), TRUE, TRUE, FALSE, TRUE, TRUE, rep(FALSE, 13),
    TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE
  )
  expect_identical(mt_movebank_visible(d), exp)
  expect_identical(mt_filter_movebank_visible(d), d[exp, ])
  expect_identical(mt_movebank_visible(cbind(visible = rev(exp), d)), rev(exp))
  names(d) <- gsub("-", "_", names(d))
  expect_identical(mt_movebank_visible(d), exp)
  expect_identical(
    mt_movebank_visible(d |> dplyr::filter(!`import_marked_outlier`) |>
      dplyr::select(-`import_marked_outlier`)),
    exp[!(is.na(d$import_marked_outlier) | d$import_marked_outlier)]
  )

  expect_identical(mt_movebank_visible(cbind(visible = rev(exp), d)), rev(exp))
  expect_error(
    mt_movebank_visible(d |> dplyr::mutate("manually_marked_outlier" = 1)),
    "The values in the `manually_marked_outlier` column are expected to be logical"
  )
  expect_error(
    mt_movebank_visible(d |> dplyr::mutate("manually_marked_outlier" = "h")),
    "The values in the `manually_marked_outlier` column are expected to be logical"
  )
  expect_error(
    mt_movebank_visible(d |> dplyr::mutate("manually_marked_valid" = "h")),
    "The values in the `manually_marked_valid` column are expected to be logical"
  )
  expect_error(
    mt_movebank_visible(d |> dplyr::mutate("algorithm_marked_outlier" = as.character(algorithm_marked_outlier))),
    "The values in the `algorithm_marked_outlier` column are expected to be logical"
  )
  expect_error(
    mt_movebank_visible(d |> dplyr::mutate(
      "algorithm_marked_outlier" = as.character(algorithm_marked_outlier),
      "visible" = as.character(algorithm_marked_outlier)
    )),
    "The values in the `visible` column are expected to be logical"
  )
})
