#' @importFrom utils head
#' @importFrom dplyr row_number n
NULL


#' Find subset of records based on time windows
#'
#' @description
#' * `mt_filter_per_interval`: returns a `move2` with the selected records
#' * `mt_per_interval`: returns a logical vector indicating the selected records
#'
#' @param x a move2 object
#' @param unit the time units to select the first record per. This can also be a multiple of units (e.g. "30 seconds").
#' For more details see \code{\link[lubridate]{floor_date}}
#' @param criterion the criterion of what record to select per time interval
#' @param ... additional arguments to \code{mt_per_interval} and \code{\link[lubridate]{floor_date}},
#'  for example the day that starts the week
#'
#' @details
#' The function selects one event per defined interval (time window). The time lag
#' between the selected events does not necessarily correspond to the defined interval. For
#' example, if the defined time interval is "1 hour" with the criterion "first", the function will select the
#' event that is closest to every full hour, so if the first event of a track is at 10:45 and the second at 11:05,
#' both of them will be selected, as they fall into different hour windows, but the time lag between them is 20 minutes.
#' When sampling down a track, the time lags mostly correspond to the defined time interval, except the first time lag,
#' and when there are gaps in the data.
#'
#'
#' @return `mt_per_interval` returns a logical vector indicating the selected records. \cr
#' `mt_filter_per_interval` returns a filtered `move2` object
#'
#' @family filter
#' @export
#' @examples
#' data <- mt_sim_brownian_motion(as.POSIXct("2022-1-1") + 1:10)
#' data |> mt_filter_per_interval(criterion = "random")
#' data |> mt_filter_per_interval(unit = "3 secs")
#' data[mt_per_interval(data, unit = "6 secs"), ]
mt_filter_per_interval <- function(x, ...) {
  x[mt_per_interval(x, ...), ]
}
#' @rdname mt_filter_per_interval
#' @export
mt_per_interval <- function(x, criterion = c("first", "random", "last"), unit = "hour", ...) {
  criterion <- rlang::arg_match(criterion)
  check_installed("lubridate", "to select the first record per time unit")
  assert_that(mt_is_time_ordered(x))
  xx <- x |>
    group_by(!!!syms(mt_track_id_column(x))) |>
    mutate(flooredDate = lubridate::floor_date(!!!syms(mt_time_column(x)), unit = unit, ...)) |>
    group_by(!!!syms(mt_track_id_column(x)), !!!syms("flooredDate"))
  return(switch(criterion,
    first = mutate(xx, sel = dplyr::row_number() == 1L),
    last = mutate(xx, sel = dplyr::row_number() == n()),
    random = mutate(xx, sel = dplyr::row_number() == sample.int(n(), 1L))
  ) |> pull("sel"))
}
