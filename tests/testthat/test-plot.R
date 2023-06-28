m <- mt_sim_brownian_motion(t = 1:40)
test_that("base plotting", {
  expect_silent(plot(m))
  expect_silent(plot(suppressMessages(mt_track_lines(m))))
})
test_that("ggplot plotting", {
  suppressMessages(require(ggplot2))
  expect_silent(print(ggplot() +
    geom_sf(data = m, aes(color = track))))
  expect_silent(print(ggplot() +
    geom_sf(data = suppressMessages(mt_track_lines(m)), aes(color = track))))
  expect_silent(print(ggplot() +
    geom_sf(data = sf::st_set_crs(m, 4326), aes(color = track))))
  expect_silent(print(ggplot() +
    geom_sf(data = suppressMessages(mt_track_lines(sf::st_set_crs(m, 4326))), aes(color = track))))
  m$geometry[5:35] <- sf::st_point()
  expect_silent(print(ggplot() +
    geom_sf(data = m, aes(color = track))))
  expect_silent(print(ggplot() +
    geom_sf(data = suppressMessages(mt_track_lines(m)), aes(color = track))))
  expect_silent(print(ggplot() +
    geom_sf(data = sf::st_set_crs(m, 4326), aes(color = track))))
  expect_silent(print(ggplot() +
    geom_sf(data = suppressMessages(mt_track_lines(sf::st_set_crs(m, 4326))), aes(color = track))))
  m$col <- seq_len(nrow(m))
  m <- dplyr::filter(m, !sf::st_is_empty(m))
  expect_silent(print(ggplot() +
    geom_sf(data = m, aes(color = track))))
  expect_silent(print(ggplot() +
    geom_sf(data = suppressMessages(mt_track_lines(m)), aes(color = track))))
  expect_silent(print(ggplot() +
    geom_sf(data = m, aes(color = col))))
  expect_silent(print(ggplot() +
    geom_sf(data = suppressMessages(mt_track_lines(m, col = mean(col))), aes(color = col))))
})
