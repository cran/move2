#' @importFrom rlang is_scalar_character is_scalar_integerish
#' @importFrom stats rnorm integrate
#' @importFrom utils tail
NULL

#' Simulate Brownian motion
#'
#' @description
#' Creates a `move2` object with simulated data following a Brownian motion
#'
#'
#' @param t a vector of timestamps, numeric values or times to simulate for. If multiple tracks are created this vector will
#' be used for all of them, alternatively a list with a vector per track can be provided.
#' @param sigma The Brownian motion variance movement rate  \eqn{[\frac{\sigma}{time}]} either as scalar number or a
#' vector with a number per segment. Not that this argument is the movement rate so the motion variance will be
#' adjusted for the length of the interval. Alternatively a function that is integrated over time in the simulation
#' function, this function needs to be vectorized (if needed see \code{\link[base]{Vectorize}}). If a list is provided
#' one element of the list is taken per track.
#' @param tracks Either the number of tracks or a vector containing the names of the tracks.
#' @param start_location Either one or a list of start locations, as a vector with two numbers.
#' @param track_id The identifier of the track if a single track is requested.
#'
#' @details
#' Note that when lists are provided as in input the names of these lists are ignored. Individuals are simulated by
#'  order.
#'
#' If `t` is numeric the movement rate (`sigma`) is assumed to be expressed per unit `t`, if `t` is a timestamp or a
#' date, `sigma` is assumed to be expressed per second.
#'
#' @return a move2 object
#' @export
#'
#' @examples
#' mt_sim_brownian_motion() |> plot()
#' mt_sim_brownian_motion(list(1:10, 1:100)) |>
#'   mt_track_lines() |>
#'   plot()
#' mt_sim_brownian_motion(1:200,
#'   sigma = .25, letters[1:4],
#'   list(c(0, 0), c(10, 0), c(0, 10), c(10, 10))
#' ) |>
#'   mt_track_lines() |>
#'   plot()
mt_sim_brownian_motion <- function(t = 1L:10L, # nolint
                                   sigma = 1L, tracks = 2L, start_location = c(0L, 0L), track_id = NULL) {
  if (is_scalar_integerish(tracks) && !inherits(tracks, "factor")) {
    n_tracks <- tracks
  } else {
    if (anyDuplicated(tracks)) {
      cli_abort("{qty(length(unique(tracks[duplicated(tracks)])))}There {?is a/are} duplicated name{?s} in
                the {.arg tracks} argument (duplicate{?s}: {.code {unique(tracks[duplicated(tracks)])}})",
        class = "move2_error_duplicated_track_ids"
      )
    }
    n_tracks <- length(tracks)
  }
  if (n_tracks != 1) {
    l <- list()
    for (i in seq_len(n_tracks)) {
      if (is.list(t)) {
        assert_that(length(t) == n_tracks,
          msg = "If `t` is a list the length should match the length of the number of desired tracks."
        )
        t_sub <- t[[i]]
      } else {
        t_sub <- t
      }
      if (is.list(start_location)) {
        assert_that(length(start_location) == n_tracks,
          msg = format_error("If `start_location` is a list the length should
                                       match the length of the number of desired tracks.")
        )
        start_location_sub <- start_location[[i]]
      } else {
        start_location_sub <- start_location
      }
      if (is.list(sigma)) {
        assert_that(length(sigma) == n_tracks,
          msg = "If `sigma` is a list the length should match the length of the number of desired tracks."
        )
        sigma_sub <- sigma[[i]]
      } else {
        sigma_sub <- sigma
      }
      if (length(tracks) != 1) {
        track_sub <- (tracks[i])
      } else {
        track_sub <- (i)
      }
      l[[i]] <- mt_sim_brownian_motion(
        t = t_sub, sigma = sigma_sub, start_location = start_location_sub,
        track_id = track_sub, tracks = 1L
      )
    }
    return(do.call(rbind, l))
  }
  n <- length(t)
  assert_that(length(start_location) == 2, msg = "`start_location` should have length of two")
  assert_that(is.numeric(start_location))
  assert_that(length(sigma) == 1 | length(sigma) == (n - 1))
  assert_that(all(diff(t) >= 0), msg = "Times in `t` should not be descending")
  if (is.function(sigma)) {
    r <- mapply(integrate, head(t, -1), tail(t, -1),
      MoreArgs = list(f = function(...) {
        x <- sigma(...)
        if (any(x < 0)) {
          cli_abort("The {.arg sigma} function results in negative values, they should all be positive or zero",
            class = "move2_error_negative_sigma_value_function"
          )
        }
        x^2
      })
    )
    assert_that(all(unlist(r["message", ]) == "OK"))
    sigma <- sqrt(unlist(r["value", ]))
  } else {
    if (!all(sigma >= 0L)) {
      cli_abort("All sigma values should be postive.", class = "move2_error_negative_sigma_value_argument")
    }
    if (length(sigma) == 1L) {
      sigma <- rep(sigma, n - 1L)
    }
    dt <- diff(t)
    if (inherits(dt, "difftime")) {
      dt <- as.numeric(dt, units = "secs")
    }
    if (!is.numeric(sigma)) {
      cli_abort("The values of {.code sigma} should be {.cls numeric}, the current class is {.cls {class(sigma)}}.",
        class = "move2_error_sigma_not_numeric"
      )
    }
    sigma <- sqrt((sigma^2L) * dt)
  }
  if (length(sigma) != (n - 1L)) {
    cli_abort("The length of {.arg sigma} does not correspond to the number of timestamps, sigma should have a length
              of the number of {.arg timestamps} - 1",
      class = "move2_error_sigma_length_error"
    )
  }
  x <- cumsum(c(start_location[1L], rnorm(n - 1L, mean = 0.0, sd = sigma)))
  y <- cumsum(c(start_location[2L], rnorm(n - 1L, mean = 0.0, sd = sigma)))
  if (is.null(track_id)) {
    track_id <- tracks
  }
  m <- st_as_sf(coords = c("x", "y"), x = data.frame(time = t, x, y, track = track_id)) |>
    new_move("time", track_id_column = "track", track_attributes = "")
  return(m)
}

if (FALSE) { # nolint
  mt_sim_brownian_motion_units <- function(t = 1:10, sigma = set_units(1, "m/s"),
                                           tracks = 2L, start_location = c(0.0, 0.0)) {
    if (is_scalar_integerish(tracks)) {
      n_tracks <- tracks
    } else {
      n_tracks <- length(tracks)
    }
    if (n_tracks != 1) {
      l <- list()
      for (i in seq_len(n_tracks)) {
        if (is.list(t)) {
          assert_that(length(t) == n_tracks,
            msg = "If `t` is a list the length should match the length of the number of desired tracks."
          )
          t_sub <- t[[i]]
        } else {
          t_sub <- t
        }
        if (is.list(start_location)) {
          assert_that(length(start_location) == n_tracks,
            msg = format_error("If `start_location` is a list the length should match the length of the
                                         number of desired tracks.")
          )
          start_location_sub <- start_location[[i]]
        } else {
          start_location_sub <- start_location
        }
        if (is.list(sigma)) {
          assert_that(length(sigma) == n_tracks,
            msg = "If `sigma` is a list the length should match the length of the number of desired tracks."
          )
          sigma_sub <- sigma[[i]]
        } else {
          sigma_sub <- sigma
        }
        if (length(tracks) != 1) {
          track_sub <- tracks[i]
        } else {
          track_sub <- as.character(i)
        }
        l[[i]] <- mt_sim_brownian_motion_units(
          t = t_sub, sigma = sigma_sub,
          start_location = start_location_sub, tracks = track_sub
        )
      }
      return(do.call(rbind, l))
    }
    n <- length(t)
    assert_that(length(start_location) == 2L, msg = "`start_location` should have length of two")
    assert_that(is.numeric(start_location))
    assert_that(length(sigma) == 1L | length(sigma) == (n - 1L))
    assert_that(all(diff(t) >= 0.0), msg = "Times in `t` should not be descending")
    if (is.function(sigma)) {
      r <- mapply(integrate, head(t, -1L), tail(t, -1L),
        MoreArgs = list(f = function(...) {
          x <- sigma(...)
          if (any(x < 0L)) {
            cli_abort("The sigma function results in negative values, they should all be positive or zero",
              class = "move2_error_negative_sigma_value_function"
            )
          }
          x^2L
        })
      )
      assert_that(all(unlist(r["message", ]) == "OK"))
      sigma <- sqrt(unlist(r["value", ]))
    } else {
      if (!all(sigma >= set_units(0.0, m / s))) {
        cli_abort("All sigma values should be postive.", class = "move2_error_negative_sigma_value_argument")
      }
      if (length(sigma) == 1L) {
        sigma <- rep(sigma, n - 1L)
      }
      dt <- diff(t)
      if (inherits(dt, "difftime")) {
        dt <- as.numeric(dt, units = "secs")
      }
      sigma <- sqrt((sigma^2L) * dt)
    }
    if (length(sigma) != (n - 1L)) {
      cli_abort("The length of sigma does not correspond to the number of timestamps, sigma should have a length of
                the number of timestamps - 1",
        class = "move2_error_sigma_length_error"
      )
    }
    x <- cumsum(c(start_location[1L], rnorm(n - 1L, mean = 0.0, sd = sigma)))
    y <- cumsum(c(start_location[2L], rnorm(n - 1L, mean = 0.0, sd = sigma)))

    m <- st_as_sf(coords = c("x", "y"), x = data.frame(time = t, x, y, track = tracks)) |>
      new_move("time", track_id_column = "track", track_attributes = "")
    return(m)
  }
}
