#' Move one or more columns to track attributes or event attributes
#' @description
#' * `mt_as_track_attribute`: move a column from the event to the track attributes
#' * `mt_as_event_attribute`: move a column from the track to the event attributes
#'
#' @param x The move2 object
#'
#' @param ... the names of columns to move, it is also possible to use \code{\link[tidyselect:language]{helpers}}.
#'
#' @param .keep a logical if the variables also kept in their original location
#'
#' @details
#'  When one or more of the selected columns contain more then one unique value per track an error is raised.
#'
#' @return An object of the class `move2` with the column(s) moved
#' @export
#' @seealso
#' * [mt_track_data()] to retrieve the track attribute table
#' * [mt_set_track_data()] to replace attribute table with new values
#' @examples
#' sim_data <- mt_sim_brownian_motion()
#' sim_data$sex <- "female"
#'
#' ## different ways to move column "sex" from event to track attribute
#' sim_data |> mt_as_track_attribute(sex)
#' sim_data |> mt_as_track_attribute(starts_with("s"))
#' sim_data |> mt_as_track_attribute(any_of(c("sex", "age")))
mt_as_track_attribute <- function(x, ..., .keep = FALSE) {
  assert_that(is_scalar_logical(.keep))
  expr <- rlang::expr(c(...))
  pos <- eval_select(expr, data = x)
  xx <- x
  class(xx) <- setdiff(class(xx), "move2")
  to_move <- xx |>
    select(...) |>
    st_drop_geometry() |>
    mutate(!!attr(x, "track_id_column") := mt_track_id(x)) |>
    distinct() |>
    tibble::as_tibble()
  assert_that(nrow(to_move) == mt_n_tracks(x), msg = "The attributes to move do not have a unique value per individual")
  assert_that(all(to_move |> pull(!!mt_track_id_column(x)) == mt_track_id(x) |> unique()),
    msg = "The order of tracks got mixed up"
  )
  if (is.integer(pos) && length(pos) == 0) {
    # there are no columns to move
    return(x)
  }
  pos <- pos[names(pos) != mt_track_id_column(x)]
  if ((is.integer(pos) && length(pos) == 0) || .keep) {
    pos <- TRUE
  } else {
    pos <- -pos
  }
  updated_data <- mt_track_data(x) |>
    left_join(to_move, by = attr(x, "track_id_column")) |>
    mt_set_track_data(x = x[, pos])
  return(updated_data)
}

#' @export
#' @rdname mt_as_track_attribute
mt_as_event_attribute <- function(x, ..., .keep = FALSE) {
  assert_that(is_scalar_logical(.keep))
  expr <- rlang::expr(c(...))
  track_data <- mt_track_data(x)
  pos <- eval_select(expr, data = track_data)
  pos <- pos[names(pos) != mt_track_id_column(x)]
  if (length(pos) != 0) {
    pos <- -pos
  } else {
    pos <- TRUE
  }
  x <- x |>
    left_join(
      track_data[, c(mt_track_id_column(x), names(pos)), drop = FALSE],
      mt_track_id_column(x)
    ) |>
    mt_set_track_data(track_data[, if (.keep) {
      TRUE
    } else {
      pos
    }, drop = FALSE])
  return(x)
}
