#' @importFrom dplyr pull
NULL

#' `dplyr` functions to manipulate the track data
#'
#' @description
#' * `filter_track_data` filter data based on a track attribute (e.g. select all juveniles).
#' Based on \code{\link[dplyr]{filter}}.
#' * `select_track_data` keep or drop attributes in the track data. Based on \code{\link[dplyr]{select}}.
#' * `mutate_track_data` create or modify attributes in the track data. Based on \code{\link[dplyr]{mutate}}.
#' * `group_by_track_data` group by one or more attribute of the track data (e.g. group by sex, by taxon, by life stage,
#' etc). Based on \code{\link[dplyr]{group_by}}.
#'
#'
#' @param .data the `move2` object
#' @param ... The identifiers of one or more tracks to select or selection criteria based on track attributes
#' @param .track_id A vector of the ids of the tracks to select
#' @param .add see original function docs \code{\link[dplyr]{group_by}}
#' @param .drop see original function docs \code{\link[dplyr]{group_by}}
#'
#' @rdname dplyr-track
#'
#' @return a `move2` object
#' @export
#'
#' @examples
#' ## simulating a move2 object with 4 tracks
#' data <- mt_sim_brownian_motion(tracks = letters[1:4])
#'
#' ## retaining tracks "b" and "d"
#' data |>
#'   filter_track_data(.track_id = c("b", "d"))
#'
#' ## adding the attribute "sex" to the track data
#' data <- data |>
#'   mutate_track_data(sex = c("m", "f", "f", "m"))
#'
#' ## retaining tracks of females
#' data |> filter_track_data(sex == "f")
#'
filter_track_data <- function(.data, ..., .track_id = NULL) {
  new_id_data <- mt_track_data(.data) |> filter(...)
  if (!is.null(.track_id)) {
    new_id_data <- new_id_data |> filter(!!sym(attr(.data, "track_id_column")) %in% .env[[".track_id"]])
  }
  new_tracks <- new_id_data |>
    select(!!sym(attr(.data, "track_id_column"))) |>
    pull()
  ids <- mt_track_id(.data)
  .data <- .data |> filter(ids %in% new_tracks)
  .data <- mt_set_track_data(.data, new_id_data)
  return(.data)
}


#' @rdname dplyr-track
#' @export
select_track_data <- function(.data, ...) {
  new_id_data <- mt_track_data(.data) |> select(...)
  track_id_column <- mt_track_id_column(.data)
  assert_that(has_name(mt_track_data(.data), track_id_column),
    msg = format_error("The `track_id_column` attribute indicate the track ids should be contained in the column
                       {.val {track_id_column}} of the `track_data`, this column is however not found in the
                       `track_data`.")
  )
  # ensure track id column is retained
  if (!track_id_column %in% names(new_id_data)) {
    new_id_data[, track_id_column] <- mt_track_data(.data) |> pull(!!track_id_column)
  }
  return(mt_set_track_data(.data, new_id_data))
}

#' @rdname dplyr-track
#' @export
mutate_track_data <- function(.data, ...) {
  new_id_data <- mt_track_data(.data) |> mutate(...)
  return(mt_set_track_data(.data, new_id_data))
}

#' @rdname dplyr-track
#' @export
group_by_track_data <- function(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data)) {
  x <- .data
  track_id_column <- mt_track_id_column(x)
  class(x) <- setdiff(class(x), c("move2", "sf"))
  new_df <- mt_track_data(.data) |>
    dplyr::right_join(st_drop_geometry(x[, mt_track_id_column(.data)]), by = track_id_column) |>
    group_by(..., .add = .add, .drop = .drop)
  new_df[, mt_track_id_column(.data)] <- NULL
  x <- bind_cols(x, ungroup(new_df)) |> group_by(..., .add = .add, .drop = .drop)
  dplyr_reconstruct.move2(x, template = .data)
}
