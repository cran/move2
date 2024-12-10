test_that("st_crop and st_intersection", {
  m <- mt_read(mt_example())
  poly <- sf::st_polygon(list(cbind(
    c(-73.96285, -73.83646, -73.77327, -73.86531, -73.96285),
    c(42.75987, 42.82641, 42.71446, 42.68517, 42.75987)
  ))) |>
    sf::st_sfc(crs = sf::st_crs(m))
  suppressWarnings(expect_identical(sf::st_intersection(m, poly) |> mt_track_id() |> unique() |> as.character(), c("F1", "F2", "F3", "M1", "M2", "M3")))
  suppressWarnings(expect_identical(sf::st_crop(m, poly) |> mt_track_id() |> unique() |> as.character(), c("F1", "F2", "F3", "M1", "M2", "M3", "M4")))
})
