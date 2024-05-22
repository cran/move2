#' Convert trajectories into lines
#'
#' @description
#' Converts each track into one line
#'
#' @param x A move object
#'
#' @param ... Arguments passed on to the [summarise][dplyr::summarise] function
#'
#' @return A [sf::sf] object with a `LINESTRING` representing the track as geometry for each track. The
#' `track_data` for each track is included as well as the products from summarize
#'
#' @details
#' Note that all empty points are removed before summarizing. Arguments passed with `...` thus only summarize for the
#' non empty locations.
#'
#' @seealso
#' * [mt_segments()] For transforming all segments to a `LINESTRING` separately
#'
#' @examples
#' mt_sim_brownian_motion() |>
#'   mt_track_lines(
#'     n = dplyr::n(),
#'     minTime = min(time),
#'     maxTime = max(time)
#'   )
#' ## empty points are not counted in summary statistic
#' x <- mt_sim_brownian_motion(1:3)
#' x$geometry[[2]] <- sf::st_point()
#' x |> mt_track_lines(
#'   n = dplyr::n()
#' )
#' ## plot of the tracks as a line
#' mt_sim_brownian_motion(
#'   tracks = letters[1:2],
#'   start_location = list(c(0, 0), c(10, 0))
#' ) |>
#'   mt_track_lines() |>
#'   plot()
#'
#' @export
mt_track_lines <- function(x, ...) {
  e <- st_is_empty(x)
  assert_that(st_geometry_type(x, FALSE) == "POINT", msg = "The geometry column needs to contain points.")
  assert_that(mt_is_time_ordered(x))
  if (any(e)) {
    cli_inform("In total {sum(e)} empty location record{?s} {?is/are} removed before summarizing.")
    x <- x |>
      filter(!e)
  }
  x |>
    group_by(!!sym(attr(x, "track_id_column"))) |>
    summarise(do_union = FALSE, ...) |> # union F prevents point order from being mixed up
    st_cast("LINESTRING") |>
    left_join(mt_track_data(x), by = mt_track_id_column(x))
}
