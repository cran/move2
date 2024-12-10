#' Create a AEQD coordinate reference system
#'
#' The CRS can be centered around the centroid or center of a `move2` object or a reference location
#'
#' @param x An object of the class `sf` or `sfc`, for example a `move2` to determine the center from. This argument
#' is only required if `center` is a character.
#' @param center Either the method to identify the coordinates of the center of `x` or the center as a `numeric`,
#' `POINT` or a `sf`/`sfc` of length 1. `"centroid"` calculates the centroid of a collection of points while `"center"`
#' calculates the center from the range of the locations.
#' @param units The units of the AEQD projection either `m` or `km` for meter or kilometer respectively
#'
#' @return An object of the class `crs` that can for example be used for re projecting
#' @export
#'
#' @examples
#' mt_aeqd_crs(center = c(10, 45))
#' mt_aeqd_crs(center = sf::st_point(c(10, 45)), units = "km")
#' @examplesIf parallel::detectCores() < 9
#' m <- mt_read(mt_example())
#' mt_aeqd_crs(center = sf::st_geometry(m)[5])
#' mt_aeqd_crs(m)
#' aeqd_crs <- mt_aeqd_crs(m, "center", "km")
#' aeqd_crs
#' sf::st_transform(m, aeqd_crs)
mt_aeqd_crs <- function(x, center = c("centroid", "center"), units = c("m", "km")) {
  units <- rlang::arg_match(units)
  if (is.character(center)) {
    center <- rlang::arg_match(center)
    if (inherits(x, "sf")) {
      x <- sf::st_geometry(x)
    }
    if (!inherits(x, "sfc")) {
      cli_abort("The input to {.fun mt_aeqd_crs} should either inherit either {.cls sf} or {.cls sfc}")
    }
    if (as.character(sf::st_geometry_type(x, by_geometry = FALSE)) != "POINT") {
      cli_abort("Currently {.fun mt_aeqd_crs} only works for spatial points")
    }
    if (all(sf::st_is_empty(x))) {
      cli_abort("Not all points can be empty")
    }
    center <- switch(center,
      centroid = st_coordinates(sf::st_transform(
        sf::st_centroid(sf::st_cast(sf::st_geometry(x)[!sf::st_is_empty(x)], "MULTIPOINT", ids = 1L)),
        4236L
      ))[1L, ],
      center = apply(apply(st_coordinates(sf::st_transform(x[!sf::st_is_empty(x)], 4326L)), 2L, range), 2L, mean)
    )
  }
  if (inherits(center, "sf")) {
    center <- sf::st_geometry(center)
  }
  if (inherits(center, "sfc")) {
    if (length(center) != 1) {
      cli_abort("The {.cls sfc} input to {.arg center} needs the have length one.",
        class = "move2_error_center_sfc_length"
      )
    }
    center <- st_transform(center, 4326L)[[1L]]
  }
  if (inherits(center, "POINTS")) {
    center <- st_coordinates(center)[1L, ]
  }



  if (length(center) != 2L) {
    cli_abort("The length of {.arg center} coordinates is unequal to two.",
      class = "move2_error_center_length"
    )
  }
  st_crs(sprintf(
    "+proj=aeqd +lat_0=%.6f +lon_0=%.6f +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=%s +no_defs",
    (center)[2L], (center)[1L], units
  ))
}
