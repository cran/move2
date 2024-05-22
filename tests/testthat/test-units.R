test_that("all units exist", {
  for (i in unique(move2:::mb_column_units))
  {
    expect_s3_class(units::as_units(i), "units")
  }
})
