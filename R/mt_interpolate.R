#' @importFrom vctrs vec_fill_missing
#' @importFrom sf NA_crs_ st_point st_line_sample st_geometry<- st_transform
#' @importFrom dplyr if_else arrange group_modify between add_row
NULL




#' Linearly interpolate locations
#'
#' @description
#' Linear interpolation along the straight line between consecutive locations.
#'
#' @param x A `move2` object
#' @param time The times to interpolate to, if missing the interpolation is to the empty locations.
#' Alternatively if the timestamps in `x` are `POSIXct` then also an interval can be provided.
#' For details on the interval specification see \code{\link[lubridate]{floor_date}}.
#' @param max_time_lag The maximal time lag to interpolate over, if not provided any interval is interpolated
#' @param omit If the original location that do not match a value in `time` should be omitted. This only
#' takes affect when `time` is not missing.
#'
#' @details
#' Each interpolation is done along a straight path from the previous to the next location.
#' Interpolation is done with \code{\link[sf]{st_line_sample}} when there is no CRS provided and \code{\link[s2]{s2_interpolate_normalized}} when the data has a projection.
#'
#' @return A `move2` object with the interpolated locations
#' @export
#'
#' @examples
#' data <- mt_sim_brownian_motion(t = c(0, 0.6, 3, 3.5))
#' ## interpolating at specific times
#' mt_interpolate(data, c(.5, 1.5, 2.5))
#' ## interpolating to empty locations
#' data$geometry[c(1, 3)] <- sf::st_point() ## creating empty locations
#' mt_interpolate(data)
#' @examplesIf parallel::detectCores() < 9
#' fishers <- mt_read(mt_example())[1:200, ]
#' mt_interpolate(fishers, "2 hours")
#' ## omit the original records
#' mt_interpolate(fishers, "2 hours", omit = TRUE)
mt_interpolate <- function(x, time, max_time_lag, omit = FALSE) {
  assert_that(mt_is_time_ordered(x))
  assert_that(st_geometry_type(x, FALSE) == "POINT",
    msg = "The geometry colummn should contain only points."
  )

  if (missing(max_time_lag)) {
    max_time_lag <- diff(range(mt_time(x))) * 1.1
    # if missing we just make it so large it will aways evaluate to true
  }
  if (inherits(max_time_lag, "difftime")) {
    max_time_lag <- as_units(max_time_lag)
  }
  if (!missing(time)) {
    if (is_scalar_character(time) && is.time(mt_time(x))) {
      time <- seq(lubridate::floor_date(min(mt_time(x)), time), lubridate::ceiling_date(max(mt_time(x)), time), by = time)
    }

    if (!isTRUE(all.equal(class(mt_time(x)), class(time)))) {
      cli_abort(
        c("The class of time in `x` ({.cls {class(mt_time(x))}}) does not correspond to the class of the `time`
          ({.cls {class(time)}}) argument",
          i = "To interpolate both need to have the same class of time."
        ),
        class = "move2_error_interpolate_time_does_not_correspond"
      )
    }
    if (inherits(time, "POSIXt") && inherits(mt_time(x), "POSIXt") &&
      lubridate::tz(time) != lubridate::tz(mt_time(x))) {
      time <- lubridate::with_tz(time, lubridate::tz(mt_time(x)))
    }
    class(x) <- setdiff(class(x), "move2")

    x <- x |> # adding rows for times that need interpolation
      group_by(!!!syms(attr(x, "track_id_column"))) |>
      group_modify(.keep = TRUE, ~ {
        timevals <- .x[[mt_time_column(x)]]
        s <- between(time, min(timevals), max(timevals)) & !(time %in% timevals)
        xx <- .x
        class(xx) <- setdiff(class(xx), "move2")

        xx <- xx |> select(-one_of(mt_track_id_column(x)))
        if (sum(s)) {
          # COMBAK if https://github.com/r-lib/vctrs/issues/1748 is resolved if statement can be removed
          add_row(
            xx,
            !!mt_time_column(x) := time[s]
          )
        } else {
          xx
        }
      }) |>
      ungroup() |>
      dplyr_reconstruct.move2(template = x) |>
      arrange(!!!syms(attr(x, "track_id_column")), !!!syms(attr(x, "time_column")))
    class(x) <- c("move2", class(x))
    to_interpolate <- st_is_empty(x) & (mt_time(x) %in% time)
  } else {
    to_interpolate <- TRUE
  }
  # if time missing all empty points
  if (st_crs(x) != NA_crs_) {
    check_installed("s2")
  }
  res <- x |>
    select(-any_of("time")) |>
    mt_set_time("time") |>
    mutate(
      i = if_else(st_is_empty(!!!syms(attr(x, "sf_column"))), NA_integer_, seq_len(n())),
      t = if_else(is.na(.data$i), if (inherits(time, "POSIXct")) {
        as.POSIXct(NA)
      } else {
        if (inherits(time, "Date")) {
          as.Date(NA)
        } else {
          as(NA, class(time))
        }
      }, time) # time*NA needed for dplyr 1.0.10
    ) |>
    group_by(!!!syms(attr(x, "track_id_column"))) |>
    mutate(
      prv_t = vec_fill_missing(t, "down"),
      time_interval = (vec_fill_missing(t, "up") - .data$prv_t),
      prop_t = as.numeric(time - .data$prv_t, units = "mins") /
        as.numeric(.data$time_interval, units = "mins"),
      nxt_loc = st_geometry(!!x)[vec_fill_missing(.data$i, "up")],
      prv_loc = st_geometry(!!x)[vec_fill_missing(.data$i, "down")],
      time_interval = if (inherits(.data$time_interval, "difftime")) {
        as_units(.data$time_interval)
      } else {
        .data$time_interval
      },
      time_interval_in_max_time_lag = .data$time_interval <= max_time_lag,
      time_interval_in_max_time_lag = if_else(
        is.na(.data$time_interval_in_max_time_lag),
        FALSE, .data$time_interval_in_max_time_lag
      )
    ) |>
    ungroup() |>
    mutate(
      new = st_sfc(st_point(), crs = st_crs(!!!syms(attr(x, "sf_column")))),
      prop_s = !is.na(.data$prop_t) &
        to_interpolate & .data$time_interval_in_max_time_lag,
      new = replace(
        .data$new, .data$prop_s,
        if (st_crs(!!!syms(attr(x, "sf_column"))) == NA_crs_) {
          st_cast(st_sfc(mapply(st_line_sample,
            st_sfc(lapply(
              FUN = function(...) {
                st_cast(...)
              },
              mapply(c, .data$prv_loc[.data$prop_s],
                .data$nxt_loc[.data$prop_s],
                SIMPLIFY = FALSE
              ), "LINESTRING"
            )),
            sample = .data$prop_t[.data$prop_s]
          )), "POINT")
        } else {
          st_transform(
            st_as_sfc(s2::s2_interpolate_normalized(
              st_cast(
                do.call(st_sfc, c(
                  list(crs = st_crs(!!!syms(attr(x, "sf_column")))),
                  mapply(c, .data$prv_loc[.data$prop_s],
                    .data$nxt_loc[.data$prop_s],
                    SIMPLIFY = FALSE
                  )
                )), "LINESTRING"
              ),
              .data$prop_t[.data$prop_s]
            )),
            crs = st_crs(!!!syms(attr(x, "sf_column")))
          )
        }
      )
    ) |>
    pull(new)
  st_geometry(x)[!st_is_empty(res)] <- res[!st_is_empty(res)]
  if (omit && !missing(time)) {
    x <- x[mt_time(x) %in% time, ]
  }
  return(x)
}
