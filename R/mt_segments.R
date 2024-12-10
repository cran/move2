#' Create a `LINESTRING` for each track segment
#'
#' @description
#' Creates a `LINESTRING` for each segment between consecutive points within a track.
#'
#' @param x A `move2` object.
#'
#' @details
#' The last location of each track is formed by a `POINT` as no segment can be formed.
#'
#' @return A `sfc` object containing `LINESTRING`s for each segment of a trajectory.
#' @export
#'
#' @seealso
#' * [mt_track_lines()] For transforming the full tracks into one `LINESTRING`.
#'
#' @examples
#' track <- mt_sim_brownian_motion()
#' mt_segments(track)
#' ## adding the segments as an attribute to the move2 object
#' track$segments <- mt_segments(track)
#' track
#'
mt_segments <- function(x) {
  assert_that(mt_is_time_ordered_non_empty_points(x))
  sfc_segments <- x |>
    mutate(
      next_location = lead(
        !!!syms(attr(x, "sf_column")) # If problem see default argument
      ),
      segment_column = st_sfc((!!!syms(attr(x, "sf_column")))),
      numeric_track_id = (!!!syms(attr(x, "track_id_column"))),
      numeric_track_id = if (is.numeric(.data$numeric_track_id)) {
        .data$numeric_track_id
      } else {
        if (is.factor(.data$numeric_track_id)) {
          as.numeric(.data$numeric_track_id)
        } else {
          as.numeric(as.factor(.data$numeric_track_id))
        }
      },
      s = c(diff(.data$numeric_track_id) == 0L, FALSE),
      segment_column = replace(.data$segment_column, .data$s, st_cast(
        do.call(st_sfc, c(
          list(crs = st_crs(x)),
          mapply(c, .data$segment_column[.data$s],
            .data$next_location[.data$s],
            SIMPLIFY = FALSE
          )
        )), "LINESTRING"
      ))
    ) |>
    pull(.data$segment_column)
  sfc_segments
}
