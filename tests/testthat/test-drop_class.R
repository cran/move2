test_that("dropping class works", {
  m <- mt_sim_brownian_motion(1L:3L)
  expect_false(inherits(mt_set_time(m, NULL), "move2"))
  expect_s3_class(mt_set_time(m, NULL), "sf")
  expect_false(inherits(mt_set_track_id(m, NULL), "move2"))
  expect_s3_class(mt_set_track_id(m, NULL), "sf")
  expect_false(has_attr(mt_set_track_id(m, NULL), "track_id_column"))
  expect_false(has_attr(mt_set_time(m, NULL), "time_column"))
})
