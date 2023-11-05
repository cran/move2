test_that("errors as expected", {
  expect_error(mt_aeqd_crs(center = st_point(1:3)), "unequal to tw")
  expect_error(mt_aeqd_crs(center = 1), "unequal to tw")
  expect_error(mt_aeqd_crs(center = sf::st_geometry(mt_sim_brownian_motion())), "needs the have length one")
  expect_error(mt_aeqd_crs(cbind(1:3, 1)), "The input to .mt.aeqd.crs().*should either inherit either .*sf.*sfc")
  m <- mt_read(mt_example())
  expect_error(mt_aeqd_crs(m[1:3, ]), "Not all points can be empty")
  expect_error(mt_aeqd_crs(mt_track_lines(m[!st_is_empty(m), ])), "only works for spatial points")
})
test_that("calculate crs correctly", {
  m <- mt_read(mt_example())
  expect_identical(mt_aeqd_crs(m[5, ]), mt_aeqd_crs(sf::st_transform(m[5, ], 3857)))
  expect_identical(mt_aeqd_crs(center = st_geometry(m[6, ])), mt_aeqd_crs(center = sf::st_transform(sf::st_geometry(m[6, ]), 3857)))
  expect_identical(mt_aeqd_crs(center = st_coordinates(m[6, ])[1, ]), mt_aeqd_crs(center = sf::st_geometry(sf::st_transform(m[6, ], 3857))))
  expect_identical(
    as.character(mt_aeqd_crs(center = c(33.4, 66.3)))[1],
    "+proj=aeqd +lat_0=66.300000 +lon_0=33.400000 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
  expect_identical(
    as.character(mt_aeqd_crs(center = st_point(c(33.4, 66.3))))[1],
    "+proj=aeqd +lat_0=66.300000 +lon_0=33.400000 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
  expect_identical(
    as.character(mt_aeqd_crs(center = st_sfc(st_point(c(33.4, 66.3)), crs = 4326)))[1],
    "+proj=aeqd +lat_0=66.300000 +lon_0=33.400000 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )

  expect_identical(
    as.character(mt_aeqd_crs(center = st_point(c(33.4, 66.3)), units = "km"))[1],
    "+proj=aeqd +lat_0=66.300000 +lon_0=33.400000 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
  )
  expect_identical(
    as.character(mt_aeqd_crs(st_sfc(st_point(c(33, 44)), st_point(c(33, 44)), st_point(c(23, 48)), crs = 4326), "center"))[1],
    "+proj=aeqd +lat_0=46.000000 +lon_0=28.000000 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
  expect_identical(
    as.character(mt_aeqd_crs(st_sfc(st_point(c(33, 48)), st_point(c(33, 44)), st_point(c(23, 48)), crs = 4326), "centroid"))[1],
    "+proj=aeqd +lat_0=46.759226 +lon_0=29.754012 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
})
