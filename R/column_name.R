#' Get or set the name of the column containing the `track_id` and `time`
#'
#' @description
#' * `mt_time_column()` returns the name of the column containing the timestamps
#' * `mt_track_id_column()` returns the name of the column containing the track ids
#' * `mt_set_time_column()` set the column that should be used as time column
#' * `mt_set_track_id_column()` set the column that should be used as track id column (the column has to be present in event and track table)
#'
#' @param x a `move2` object
#'
#' @return
#' `mt_time_column` and `mt_track_id_column` return character string of the column name \cr
#' `mt_set_time_column` and `mt_set_track_id_column` return an updated `move2` object
#'
#' @details
#' The set functions purely update the attribute containing the column name after checking the minimal requirements.
#'
#' For `mt_set_track_id_column()` the column has to be present in event and track table, if this is not the case consider using [mt_track_id()].
#'
#' @export
#'
#' @seealso
#' [mt_time()] to retrieve or change timestamps from each record.\cr
#' [mt_track_id()] to retrieve or change the track id from each record.
#' @rdname column_name
#' @examples
#' ## getting the column names
#' mt_sim_brownian_motion() |> mt_time_column()
mt_time_column <- function(x) {
  assert_that(has_attr(x, "time_column"),
    msg = format_error('A move object should have an "time_column" attribute identifying the column containing
                       timestamps, this attribute is not present.')
  )
  time_column <- attr(x, "time_column")
  assert_that(is_scalar_character(time_column),
    msg = format_error("The `time_column` attribute should be a {.cls character} of length one.")
  )
  time_column
}
#' @export
#' @rdname column_name
#' @examples
#' mt_sim_brownian_motion() |> mt_track_id_column()
#'
mt_track_id_column <- function(x) {
  assert_that(has_attr(x, "track_id_column"),
    msg = format_error('A move2 object should have an "track_id_column" attribute identifying the column containing
                       individual ids, this attribute is not present.')
  )
  track_id_column <- attr(x, "track_id_column")
  assert_that(is_scalar_character(track_id_column),
    msg = format_error("The `track_id_column` attribute should be a {.cls character} of length 1")
  )
  track_id_column
}
#' @export
#' @param value a character string of the new column name
#' @rdname column_name
#' @examples
#' ## setting 'time' to a new column
#' x <- mt_sim_brownian_motion()
#' x$date <- as.Date("2020-1-1") + x$time * 3
#' x |> mt_time_lags()
#' x |>
#'   mt_set_time_column("date") |>
#'   mt_time_lags()
mt_set_time_column <- function(x, value) {
  assert_that(is.string(value))
  assert_that(has_name(x, value),
    msg = format_error("The argument {.arg value} needs to be the name of a column in {.arg x}")
  )
  times <- x[[value]]
  assert_valid_time(times)
  attr(x, "time_column") <- value
  return(x)
}
#' @export
#' @rdname column_name

mt_set_track_id_column <- function(x, value) {
  assert_that(is.string(value))
  assert_that(has_name(x, value), msg = "The argument `value` needs to be the name of a column in `x`")
  track_ids <- x[[value]]
  assert_valid_track_id(track_ids)
  assert_that(has_name(mt_track_data(x), value),
    msg = format_error("The `track_data` does not have a column with the name {.val {value}}")
  )
  assert_that(!as.logical(anyDuplicated(mt_track_data(x)[, value])),
    msg = format_error(
      "There are duplicated track identifiers in the new `track_id` column ({.val {value}}) of the `track_data`"
    )
  )
  attr(x, "track_id_column") <- value
  return(x)
}
