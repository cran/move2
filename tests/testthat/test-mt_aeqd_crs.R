test_that("errors as expected", {
  expect_error(mt_aeqd_crs(center = st_point(1L:3L)), "unequal to tw")
  expect_error(mt_aeqd_crs(center = 1L), "unequal to tw")
  expect_error(mt_aeqd_crs(center = sf::st_geometry(mt_sim_brownian_motion())), "needs the have length one")
  expect_error(mt_aeqd_crs(cbind(1L:3L, 1L)), "The input to .mt.aeqd.crs().*should either inherit either .*sf.*sfc")
  m <- mt_read(mt_example())
  expect_error(mt_aeqd_crs(m[1L:3L, ]), "Not all points can be empty")
  expect_error(mt_aeqd_crs(mt_track_lines(m[!st_is_empty(m), ])), "only works for spatial points")
})
test_that("calculate crs correctly", {
  m <- mt_read(mt_example())
  expect_identical(mt_aeqd_crs(m[5L, ]), mt_aeqd_crs(sf::st_transform(m[5L, ], 3857L)))
  expect_identical(mt_aeqd_crs(center = st_geometry(m[6L, ])), mt_aeqd_crs(center = sf::st_transform(sf::st_geometry(m[6, ]), 3857)))
  expect_identical(mt_aeqd_crs(center = st_coordinates(m[6L, ])[1L, ]), mt_aeqd_crs(center = sf::st_geometry(sf::st_transform(m[6, ], 3857))))
  expect_identical(
    as.character(mt_aeqd_crs(center = c(33.4, 66.3)))[1L],
    "+proj=aeqd +lat_0=66.300000 +lon_0=33.400000 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
  expect_identical(
    as.character(mt_aeqd_crs(center = st_point(c(33.4, 66.3))))[1L],
    "+proj=aeqd +lat_0=66.300000 +lon_0=33.400000 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
  expect_identical(
    as.character(mt_aeqd_crs(center = st_sfc(st_point(c(33.4, 66.3)), crs = 4326L)))[1L],
    "+proj=aeqd +lat_0=66.300000 +lon_0=33.400000 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )

  expect_identical(
    as.character(mt_aeqd_crs(center = st_point(c(33.4, 66.3)), units = "km"))[1L],
    "+proj=aeqd +lat_0=66.300000 +lon_0=33.400000 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
  )
  expect_identical(
    as.character(mt_aeqd_crs(st_sfc(st_point(c(33, 44)), st_point(c(33, 44)), st_point(c(23, 48)), crs = 4326L), "center"))[1L],
    "+proj=aeqd +lat_0=46.000000 +lon_0=28.000000 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
  expect_identical(
    as.character(mt_aeqd_crs(st_sfc(st_point(c(33, 48)), st_point(c(33, 44)), st_point(c(23, 48)), crs = 4326L), "centroid"))[1L],
    "+proj=aeqd +lat_0=46.759226 +lon_0=29.754012 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
})
