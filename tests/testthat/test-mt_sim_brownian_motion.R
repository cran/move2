test_that("move sim error checking", {
  expect_error(mt_sim_brownian_motion(sigma = list(1, 2, 3), tracks = 1:2))
  expect_error(mt_sim_brownian_motion(t = list(1, 2, 3), tracks = 2L))
  expect_error(mt_sim_brownian_motion(start_location = list(2:1, 1:2, 2:3), tracks = 1:2))
  expect_error(mt_sim_brownian_motion(start_location = list(2:1, 1:2, 1:3), tracks = 1:3))
  expect_error(mt_sim_brownian_motion(t = 1:4, tracks = "a", sigma = 1:4))
  expect_error(mt_sim_brownian_motion(t = 1:4, tracks = "a", sigma = c(1, -1, 1)))
  expect_error(mt_sim_brownian_motion(t = 1:4, tracks = "a", sigma = -1))
  expect_error(mt_sim_brownian_motion(t = c(1:4, 3), tracks = "a", sigma = 1))
  expect_no_error(mt_sim_brownian_motion(t = 1:4, tracks = "a", sigma = Vectorize(function(x) x)))
  expect_no_error(mt_sim_brownian_motion(t = 0:4, tracks = "a", sigma = Vectorize(function(x) x)))
  expect_error(mt_sim_brownian_motion(t = -1:4, tracks = "a", sigma = Vectorize(function(x) x)))
})
test_that(" duplicate track id", {
  expect_error(mt_sim_brownian_motion(tracks = c("a", "b", "b")), "There is a duplicated name in the")
})
test_that("different track input types", {
  expect_identical(
    mt_sim_brownian_motion(1, tracks = 1L) |> mt_track_id(),
    1L
  )
  expect_identical(
    mt_sim_brownian_motion(1, tracks = 2L) |> mt_track_id(),
    c(1L, 2L)
  )
  expect_identical(
    mt_sim_brownian_motion(1, tracks = 5:7) |> mt_track_id(),
    5:7
  )
  expect_identical(
    mt_sim_brownian_motion(1, tracks = "a") |> mt_track_id(),
    c("a")
  )
  expect_identical(
    mt_sim_brownian_motion(1, tracks = c("a", "l")) |> mt_track_id(),
    c("a", "l")
  )
  expect_identical(
    mt_sim_brownian_motion(1, tracks = factor(4:5)) |> mt_track_id(),
    factor(4:5)
  )
  expect_identical(
    mt_sim_brownian_motion(1, tracks = factor(4)) |> mt_track_id(),
    factor(4)
  )
})
test_that("move with timestamps of different types", {
  expect_identical(
    mt_sim_brownian_motion(as.POSIXct(1:4, origin = "1970-1-1"), tracks = 2L) |>
      mt_time(),
    as.POSIXct(c(1:4, 1:4), origin = "1970-1-1")
  )
  expect_identical(mt_sim_brownian_motion(c(1:6, 9), tracks = 1L) |> mt_time(), c(1:6, 9))
  expect_identical(
    mt_sim_brownian_motion(list(c(1:6, 9), 9:12), tracks = 2L) |> mt_time(),
    c(c(1:6, 9), 9:12)
  )
  expect_identical(
    mt_sim_brownian_motion(as.Date("2020-1-1") + 1:3, tracks = 1L) |> mt_time(),
    as.Date("2020-1-1") + 1:3
  )
})
test_that("sigma steps", {
  expect_equal((mt_sim_brownian_motion(c(1, 2, 2, 3), tracks = 1L) |> mt_distance())[2], 0, ignore_attr = TRUE)
  expect_equal((mt_sim_brownian_motion(c(1:4), sigma = c(1, 0, 4), tracks = 1L) |>
    mt_distance())[2], 0, ignore_attr = TRUE)
})
test_that("start_locations", {
  expect_identical(
    mt_sim_brownian_motion(1,
      sigma = .25, letters[1:4],
      list(c(0, 0), c(10, 0), c(0, 10), c(10, 10))
    ) |> sf::st_coordinates(),
    structure(c(0, 10, 0, 10, 0, 0, 10, 10),
      dim = c(4L, 2L),
      dimnames = list(NULL, c("X", "Y"))
    )
  )
})
test_that("function works", {
  expect_identical(mt_sim_brownian_motion(sigma = function(x) {
    return(rep(0, length(x)))
  }) |> sf::st_geometry() |> unique(), list(sf::st_point(c(0, 0))))
})

test_that("sigma has same result for function or direct", {
  expect_equal(
    {
      set.seed(3)
      mt_sim_brownian_motion(sigma = Vectorize(function(x) 5.6), t = c(1, 6, 8))
    },
    {
      set.seed(3)
      mt_sim_brownian_motion(sigma = 5.6, t = c(1, 6, 8))
    },
    tolerance = 10e-10
  )


  expect_identical(
    {
      set.seed(7)
      mt_sim_brownian_motion(sigma = Vectorize(function(x) ifelse(x < 5, 5.6, 1.2)), t = c(1, 5, 8))
    },
    {
      set.seed(7)
      mt_sim_brownian_motion(sigma = c(5.6, 1.2), t = c(1, 5, 8))
    }
  )
})
test_that("sigma values are ok", {
  expect_equal(
    sd(apply(sf::st_coordinates(mt_sim_brownian_motion(
      t = 2 * (1:100000),
      tracks = 1, sigma = 4.3
    )), 2, diff)),
    sqrt(2 * 4.3^2),
    tolerance = .005
  )
  expect_equal(sd(apply(sf::st_coordinates(mt_sim_brownian_motion(t = 1:100000, tracks = 1, sigma = 4.3)), 2, diff)),
    4.3,
    tolerance = .005
  )
})
test_that("sigma length check", {
  expect_error(
    mt_sim_brownian_motion(
      sigma = list(1:10, 1:9, 1), tracks = 2
    ),
    "is a list the length should match the length of the number of desired tracks"
  )
  expect_error(
    mt_sim_brownian_motion(
      sigma = list("a", 1), tracks = 2
    ),
    "The values of `sigma` should be <numeric>, the current class is <character"
  )
})
