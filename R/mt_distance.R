#' Return distances or speeds between locations
#'
#' @description
#' The distance or speed is calculated between consecutive locations
#'
#'
#' @param x a `move2` object. Timestamps must be ordered within tracks and only contain location data (See 'Details').
#'
#' @details
#' `mt_is_time_ordered_non_empty_points` can be used to check if the timestamps are ordered and if the object only
#' contains location data.  To omit empty locations use e.g. `dplyr::filter(x,!sf::st_is_empty(x))`.
#'
#' Distances are calculated using [sf::st_distance].
#'
#' @return a vector of the same length as the `move2` object containing the distances/speeds between locations.
#' Each element is the distance/speed to the next location.
#' The last value for each track will be `NA`. Units are included when the data have a coordinate reference system set.
#'
#' @export
#'
#' @family track-measures
#' @examples
#' ## distance between consecutive locations
#' mt_sim_brownian_motion() |>
#'   mt_distance() |>
#'   head()
#' ## When the data has a coordinate reference system set,
#' ## units are included
#' dist <- mt_sim_brownian_motion(1:4) |>
#'   sf::st_set_crs(4326L) |>
#'   mt_distance()
#' dist
#' ## transform units of output
#' units::set_units(dist, km)
#'
mt_distance <- function(x) {
  # For calculating the distance between consecutive points points need to be ordered between points,
  # not empty and all points, There is however no need for a time gap between them
  assert_that(mt_is_time_ordered_non_empty_points(x))
  empty <- st_as_sfc("POINT(EMPTY)", crs = st_crs(x))
  d <- st_distance(
    c(st_geometry(x), empty), # head and tail seem to be quite slow on geometry objects (numeric indexing is not better)
    c(empty, st_geometry(x)),
    by_element = TRUE
  )[-1]
  ids <- mt_track_id(x)
  d[diff(as.numeric(if (is.character(ids)) {
    factor(ids)
  } else {
    ids
  })) != 0] <- NA
  return(d)
}

#' @rdname mt_distance
#' @export
#' @examples
#' ## speed between consecutive locations
#' mt_sim_brownian_motion() |> mt_speed()
#' ## When projections are provided units are included
#' speed_calc <- mt_read(mt_example())[330:340, ] |>
#'   mt_speed()
#' speed_calc
#' ## transform units of output
#' units::set_units(speed_calc, m/s)
#'
#' mt_read(mt_example())[330:340, ] |>
#'   sf::st_transform("+proj=aeqd +units=km +lon_0=-73.9 +lat_0=42.7") |>
#'   mt_speed()
#'
mt_speed <- function(x) {
  assert_that(mt_is_time_ordered(x, non_zero = TRUE))
  d <- mt_distance(x)
  dt <- mt_time_lags(x)
  return(d / dt)
}
