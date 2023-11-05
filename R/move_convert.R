#' @importFrom tibble add_column
#' @importFrom sf as_Spatial st_coordinates
NULL



#' Convert a move2 object to a move object
#'
#' @param x a `move2` object.
#'
#' @return an object of the class `Move`/`MoveStack`
#'
#'  `to_move` converts back to a objects from the `move` package. When multiple individuals are provided a
#'  \code{\link[move]{MoveStack-class}} is created otherwise a \code{\link[move]{Move-class}} object.
#'
#' @family move2-convert
#'
#' @details
#' Note that the individuals are ordered as they occur in the event data in the created
#' \code{\link[move]{MoveStack-class}} object as the order needs to correspond there between the event and track data
#' for `move`.
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("move")) {
#'   data(leroy, package = "move")
#'   leroy_move2 <- mt_as_move2(leroy)
#'   to_move(leroy_move2)
#' }
#' }
#' @export
to_move <- function(x) {
  check_installed(c("raster", "move"), "to convert move objects.")
  assert_that(inherits(x, "move2"), msg = "`x` should be a `move2` objects.")
  assert_that(mt_is_time_ordered(x))
  empty <- st_is_empty(x)
  t <- mt_time(x)
  assert_that(inherits(t, "POSIXct"),
    msg = format_error("To generate a valid move object timestamps should be of the class {.cls POSIXct}")
  )
  d <- mt_track_data(x) |> as.data.frame()
  track_id_column <- mt_track_id_column(x)
  assert_that(has_name(d, track_id_column), msg = "`track_data` does not contain the track id column")
  # move requires the rownames of the track id to be the names of the individuals
  track_id_vector <- mt_track_id(x)
  if (!inherits(track_id_vector, "factor")) {
    track_id_vector <- factor(track_id_vector)
  }
  levels(track_id_vector) <- raster::validNames(levels(track_id_vector))
  track_id_vector_iddata <- d[[track_id_column]]
  if (!inherits(track_id_vector_iddata, "factor")) {
    track_id_vector_iddata <- factor(track_id_vector_iddata)
  }
  levels(track_id_vector_iddata) <- raster::validNames(levels(track_id_vector_iddata))

  track_id_vector <- factor(track_id_vector, levels = unique(track_id_vector))

  rownames(d) <- track_id_vector_iddata
  mt <- new(".MoveTrack", as_Spatial(x[!empty, ]),
    timestamps = t[!empty],
    idData = d[levels(track_id_vector), , drop = FALSE],
    sensor = factor(rep("unknown", sum(!empty)), levels = "unknown")
  )
  u <- new(".unUsedRecords",
    dataUnUsedRecords = as.data.frame(x[empty, ]),
    timestampsUnUsedRecords = t[empty],
    sensorUnUsedRecords = factor(rep("unknown", sum(empty)), levels = "unknown")
  )

  if (mt_n_tracks(x) == 1L) {
    new("Move", mt, u)
  } else {
    new("MoveStack", mt, u,
      trackId = track_id_vector[!empty],
      trackIdUnUsedRecords = track_id_vector[empty]
    )
  }
}
