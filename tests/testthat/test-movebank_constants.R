test_that("check for duplicated constants", {
  expect_true(all(names(mb_column_units) %in% names(movebank_column_types)))
  expect_true(all(names(mb_column_units_underscore) %in% names(mb_column_types_underscore)))
  expect_equal(anyDuplicated(names(movebank_column_types)), 0L)
  expect_equal(anyDuplicated(names(mb_column_units)),0L)
})
