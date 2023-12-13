#' Calculate azimuths or turn angles
#' @description
#' * `mt_azimuth`: calculates the heading/azimuth/direction of movement of each segment between consecutive locations
#'  of a track.
#' * `mt_turnangle`: calculates the relative angle between consecutive segments.
#'
#' @family track-measures
#'
#' @param x a `move2` object. Timestamps must be ordered within tracks and only contain location data and
#' it must be in a geographic coordinate system or without a coordinate reference system (See 'Details').
#'
#' @inheritParams mt_distance
#'
#' @details
#' `mt_is_time_ordered_non_empty_points` can be used to check if the timestamps are ordered and if the object only
#' contains location data. To omit empty locations use e.g. `dplyr::filter(x,!sf::st_is_empty(x))`.
#'
#' Currently the calculation of both angles is only implemented for data in a geographic coordinate system and data
#'  without coordinates reference system. To reproject the data into long/lat use e.g.
#'   `sf::st_transform(x, crs="EPSG:4326")`
#'
#' Azimuths for geographic coordinates are calculated using [lwgeom::st_geod_azimuth()]. The angles are relative to
#'  the North pole.
#'
#' @return A vector of angles, currently default is in radians (between `-pi` and `pi`).\cr
#'
#' In `mt_azimuth` north is represented by 0, positive values are movements towards the east, and negative values
#' towards the west. The last value for each track will be `NA`.
#'
#' In `mt_turnangle` negative values are left turns and positive right turns. The first and the last value for each
#' track will be `NA`.
#'
#' @export
#'
#' @examples
#' data <- mt_sim_brownian_motion()
#' mt_azimuth(data)
#' mt_turnangle(data)
#' @examplesIf parallel::detectCores() < 9
#' x <- mt_read(mt_example())[330:340, ]
#' mt_azimuth(x)
#' mt_turnangle(x)
mt_azimuth <- function(x, units) {
  assert_that(mt_is_time_ordered_non_empty_points(x))
  if (isTRUE(st_is_longlat(x)) || st_crs(x) == st_crs(NA)) {
    if (isTRUE(st_is_longlat(x))) {
      check_installed("lwgeom",
        reason = format_message("to use {.fun mt_azimuth}")
      )
      az <- lwgeom::st_geod_azimuth(x)
    }
    if (st_crs(x) == st_crs(NA)) {
      crds_diff <- apply(st_coordinates(x), 2, diff)
      az <- set_units(atan2(crds_diff[, 1L], crds_diff[, 2L]), "rad")
    }
    ids <- mt_track_id(x)
    az[diff(as.numeric(if (is.character(ids)) {
      factor(ids)
    } else {
      ids
    })) != 0L] <- NA
    az <- c(az, as_units(NA, base::units(az)))
    return(mt_change_units(az, units))
  }
  cli_abort(
    c("Currently the calculation of azimuths is not implemented for this projection.",
      i = "A possible solution might be to transform the projection with carefull consideration to longitude latitude
              (e.g. {.code st_transform(x, 4326)}."
    ),
    class = "move2_error_not_defined_for_projection"
  )
}

#' @export
#' @rdname mt_azimuth
mt_turnangle <- function(x, units) {
  az <- mt_azimuth(x)
  az <- az |> diff()
  if (inherits(az, "units")) {
    az <- c(as_units(NA, base::units(az)), az)
    pi_r <- set_units(pi, "rad")
  } else {
    az <- c(NA, az)
    pi_r <- pi
  }
  ta <- ((((az) + pi_r) %% (pi_r * 2)) - pi_r) %% (pi_r * 2)

  return(mt_change_units(ta, units))
}
