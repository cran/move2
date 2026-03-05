test_that("segments type", {
  m <- mt_sim_brownian_motion(t = 1:3)
  expect_s3_class(s <- mt_segments(m), "sfc")
  expect_identical(nrow(m), length(s))
  expect_identical(
    st_is(s, "POINT"),
    c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
  )
  expect_identical(
    st_is(s, "LINESTRING"),
    c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
  )
  expect_equal(mt_distance(m)[c(1L:2L, 4L:5L)],
    sf::st_length(s)[c(1L:2L, 4L:5L)],
    ignore_attr = TRUE
  )
  expect_identical(
    lapply(s, st_coordinates),
    lapply(mt_segments(sf::st_set_crs(m, 4326L)), st_coordinates)
  )
  expect_equal(c(lapply(s, st_coordinates)[[1]][1L, 1L:2L]), c(0, 0),
    ignore_attr = TRUE
  )
  expect_identical(s[6L], st_geometry(m)[6L])
  expect_identical(s[3L], st_geometry(m)[3L])
})
test_that("segment locations are first", {
  m <- mt_sim_brownian_motion()
  expect_equal(
    st_coordinates(m),
    do.call("rbind", lapply(
      lapply(lapply(
        mt_segments(m),
        st_coordinates
      ), head, 1L), "[", 1L:2L
    )),
    ignore_attr = TRUE
  )
  expect_identical(st_crs(m), st_crs(mt_segments(m)))

  m <- m %>% sf::st_set_crs(3857L)
  expect_equal(
    st_coordinates(m),
    do.call("rbind", lapply(lapply(
      lapply(mt_segments(m), st_coordinates),
      head, 1L
    ), "[", 1L:2L)),
    ignore_attr = TRUE
  )
  expect_identical(st_crs(m), st_crs(mt_segments(m)))
})
test_that("track column type does not matter", {
  m <- mt_sim_brownian_motion()
  expect_identical(mt_segments(m), mt_segments(mt_set_track_id(m, as.character(m$track))))
  expect_identical(mt_segments(m), mt_segments(mt_set_track_id(m, as.factor(m$track))))
  expect_identical(mt_segments(m), mt_segments(mt_set_track_id(m, letters[m$track])))
  expect_identical(mt_segments(m), mt_segments(mt_set_track_id(m, factor(letters[m$track]))))
})
