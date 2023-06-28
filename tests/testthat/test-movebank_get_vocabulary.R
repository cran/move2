test_that("unknown error", {
  skip_if_offline()
  skip_on_cran()
  expect_length(movebank_get_vocabulary(c("study_id", "i_am_collaborator")), 2)
  expect_true(grepl(
    pattern = "http://vocab.nerc.ac.uk/collection/MVB/current",
    movebank_get_vocabulary("location_long", return_type = "uri")[[1]]
  ))
  expect_identical(
    movebank_get_vocabulary(c("study_id", "i_am_collaborator")),
    movebank_get_vocabulary(c("study_id", "i_am_collaborator"),
      xml = url("http://vocab.nerc.ac.uk/collection/MVB/current/",
        headers = c("Accept" = "application/rdf+xml")
      )
    )
  )
  m <- mt_read(mt_example())
  expect_named(
    movebank_get_vocabulary(m),
    unique(c(setdiff(names(m), "geometry"), names(mt_track_data(m))))
  )
  expect_gt(length(a <- movebank_get_vocabulary(return_type = "uri")), 200)
  expect_true("gps hdop" %in% names(a))
  expect_true("sensor type" %in% names(a))
  expect_true("contact person" %in% names(a))
})
