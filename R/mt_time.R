#' @importFrom rlang check_installed is_bare_integerish
#' @importFrom sf st_is_longlat
#' @importFrom assertthat is.time is.date is.flag
#' @importFrom dplyr ungroup
#' @importFrom sf st_sfc
#' @importFrom methods as
NULL

#' Retrieve/replace timestamps or get the interval duration between locations
#'
#' @description
#' * `mt_time()` retrieve timestamps
#' * `mt_time(x) <- value` and `mt_set_time(x, value)`  replace timestamps with new values, set new column to define time or rename time column
#' * `mt_time_lags()` returns time lags, i.e. duration interval between consecutive locations
#'
#' @param x a `move2` object
#'
#' @return `mt_time()` returns a vector of timestamps, depending on the type of data these can be either `POSIXct`,
#' `date`or `numeric` \cr
#' `mt_time_lags()` returns a vector of the time lags as `numeric` or \code{\link[units]{units}} depending on the type of data.
#'
#' @details
#' Time lags are calculated as the time difference to the next location.
#'
#' When calculating time lags between locations `NA` values are used for the transitions between tracks. This is
#'  because the interval between the last location of the previous track and first of the next track do not make
#'   sense.
#'
#' @export
#'
#' @family track-measures
#' @examples
#' ## in the simulated track, time is numeric, so the time lags are also numeric
#' x <- mt_sim_brownian_motion(1:3)
#' x |> mt_time()
#' x |> mt_time_lags()
#'
#' ## here the simulated track has timestamps, so the time lags have units
#' x <- mt_sim_brownian_motion(as.POSIXct((1:3) * 60^2, origin = "1970-1-1"), tracks = 1)
#' x |> mt_time()
#' x |> mt_time_lags()
#' x <- mt_sim_brownian_motion(as.Date(1:3, "1990-1-1"), tracks = 2)
#' x |> mt_time()
#' x |> mt_time_lags()
#'
#' ## units of the time lags can also be transformed, e.g. from days to hours
#' tl <- x |> mt_time_lags()
#' units::set_units(tl, h)
#'
#' x <- mt_sim_brownian_motion(t = as.POSIXct(1:3, , origin = "1970-1-1"), tracks = 2)
#' ## providing a vector with new timestamps
#' head(mt_time(x))
#' mt_time(x) <- 1:nrow(x)
#' head(mt_time(x))
#'
#' ## renaming the column defining time
#' mt_time_column(x)
#' mt_time(x) <- "my_new_time_name"
#' mt_time_column(x)
#'
#' ## setting a new column to define time
#' x$new_time <- as.POSIXct(1:6, origin = "2020-1-1")
#' mt_time(x) <- "new_time"
#' mt_time_column(x)
#' head(mt_time(x))
mt_time <- function(x) {
  # Thanks to Allan's suggestion:
  # https://stackoverflow.com/questions/74402744/r-dplyr-programatically-identify-column/74403687#74403687
  if (missing(x)) {
    .data <- tryCatch(
      {
        eval(quote(.data), parent.frame())
      },
      error = function(e) {
        cli_abort("Argument {.arg x} missing, with no default",
          class = "move2_error_no_x"
        )
      }
    )
    plchlder <- tryCatch(
      {
        eval(quote(.), parent.frame())
      },
      error = function(e) {
        cli_abort(
          "{.fun mt_time} can only be used without a {.arg x} argument inside dplyr verbs",
          class = "move2_error_only_dplyr"
        )
      }
    )
    return(.data[[mt_time_column(plchlder)]])
  }
  assert_that(mt_is_move2(x))
  time_column <- mt_time_column(x)
  assert_that(has_name(x, time_column),
    msg = format_error(
      'The "time_column" attribute indicate the timestamps should be contained in the column "{time_column}",
      this column is however not found.'
    )
  )
  times <- x[[time_column]]
  assert_valid_time(times)
  return(times)
}
#' @export
#' @rdname mt_time
`mt_time<-` <- function(x, value) {
  x <- mt_set_time(x, value)
  return(x)
}

#' @export
#' @param value either a vector with new timestamps, the name of the new column to define time as a scalar character (this column must be present in the event table), or a scalar character to rename the time column.
#' @rdname mt_time
mt_set_time <- function(x, value) {
  if (is_scalar_character(value)) {
    # Either rename or move to different column
    if (!has_name(x, value)) {
      # rename to column if it is not there
      colnames(x)[colnames(x) == mt_time_column(x)] <- value
    }
    return(mt_set_time_column(x, value))
  }
  if (length(value) == nrow(x)) {
    assert_valid_time(value)
    x[, mt_time_column(x)] <- value
    return(x)
  }
  if (is.null(value)) {
    # dropping the time column
    x[, mt_time_column(x)] <- value
    attr(x, "time_column") <- value
    class(x) <- setdiff(class(x), "move2")
    return(x)
  }
  if (length(value) != nrow(x)) {
    cli_abort("The new {.arg time} column should have the same length as the {.cls move2} object.")
  }
  cli_abort("No valid {.arg value} argument provided.")
}

assert_valid_time <- function(times) {
  assert_that(is.time(times) || is.date(times) || is.numeric(times),
    msg = format_error("The time column should be {.cls numeric}, a {.cls POSIXt} or a {.cls Date}.")
  )
}

# Potentially useful for internal use but does not doe sanity checks
mt_time_quick <- function(x) {
  return(x[[attr(x, "time_column")]])
}

# small convenience function to pad with NA units or not
pad_na <- function(x) {
  if (inherits(x, "units")) {
    return(c(x, as_units(NA, units(x))))
  } else {
    return(c(x, NA))
  }
}

#' @export
#' @rdname mt_time
#' @inheritParams mt_distance
mt_time_lags <- function(x, units) {
  assert_that(mt_is_time_ordered(x)) # this also works for empty locations
  dt <- diff(mt_time(x))
  dt[diff(as.numeric(factor(mt_track_id(x)))) != 0L] <- NA
  if (inherits(dt, "difftime")) {
    dt <- as_units(dt)
  }
  dt <- pad_na(dt)
  return(mt_change_units(dt, units))
}
