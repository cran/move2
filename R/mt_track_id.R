#' @importFrom tidyselect eval_select
#' @importFrom rlang := .data
#' @importFrom dplyr distinct summarise left_join
#' @importFrom sf st_cast st_geometry_type st_linestring
#' @importFrom methods new
#' @importFrom stats setNames
NULL

#' Setting and retrieving the track data in `move2` objects
#'
#' @description
#' * `mt_track_data()` retrieve track attribute table
#' * `mt_set_track_data()` replace the attribute table
#'
#' @param x the `move2` object
#' @param data the new track data. This `data.frame` must contain the column
#' with the track ids, the column name must be the same as in the `move2` object.
#'
#' @return `mt_track_data` returns a data.frame containing the track attribute data.\cr
#'   `mt_set_track_data` returns the `move2` object with updated track data
#' @export
#'
#' @examples
#' mt_sim_brownian_motion() |>
#'   mutate_track_data(sex = c("f", "m")) |>
#'   mt_track_data()
mt_track_data <- function(x) {
  assert_that(has_attr(x, "track_data"),
    msg = "A move2 object should have an `track_data` attribute containing a data frame with track metrics,
    this attribute is not present."
  )
  data <- attr(x, "track_data")
  assert_that(inherits(data, "data.frame"),
    msg = "The `track_data` should inherit from `data.frame`."
  )
  return(data)
}

#' @export
#' @rdname mt_track_data
#' @examples
#' x <- mt_sim_brownian_motion(1:2, tracks = letters[1:4])
#' mt_set_track_data(x, data.frame(track = letters[1:4], age = 2:5))
mt_set_track_data <- function(x, data) {
  assert_that(mt_is_move2(x))
  assert_that(inherits(data, "data.frame"),
    msg = format_error("{.arg data} should be a {.cls data.frame} or {.cls tibble}")
  )
  ids_full_dataset <- (unique(mt_track_id(x)))
  track_id_column <- mt_track_id_column(x)
  assert_that(has_name(data, track_id_column),
    msg = format_error("The `track_id_column` attribute indicate the track ids should be contained in the column
                       {.val {track_id_column}} of data, this column is however not found in {.arg data}.")
  )
  assert_that(all(data[[track_id_column]] %in% ids_full_dataset),
    msg = format_error("All track ids in the `track_id_column` ({.val {track_id_column}}) of the  track data frame
                       should correspond to tracks present in the {.cls move2} object.")
  )
  assert_that(all(ids_full_dataset %in% data[[track_id_column]]),
    msg = "All individuals in the move object should be in the associated track data."
  )
  attr(x, "track_data") <- data
  return(x)
}



#' Retrieve the column with track ids or get the number of tracks
#' @description
#' * `mt_track_id()` retrieve track ids
#' * `mt_track_id(x) <- value` and `mt_set_track_id(x, value)`  replace track ids with new values, set new column to define tracks or rename track id column
#' * `mt_n_tracks()` returns the number of tracks
#'
#' @param x a `move2` object
#'
#' @details
#' The vector containing the new track ids must be of the same length as the event table.
#'
#' To set a new column defining the track ids, this column has to be present in
#' the event table. See examples.
#'
#' When changing the track ids with new values that results in the combination
#' of several tracks, the track attributes of these tracks are also combined.
#' This is done by creating a lists within each column. See examples.
#'
#'
#'
#' @return `mt_track_id` returns a vector of the length of the number of locations that indicated the points belonging to one track. \cr
#'  `mt_n_tracks` returns the number of tracks.
#'
#' @export
#' @examples
#' x <- mt_read(mt_example())
#' mt_n_tracks(x)
#' unique(mt_track_id(x))
#' mt_track_id(x) |> table()
mt_track_id <- function(x) {
  if (missing(x)) {
    .data <- tryCatch(
      {
        eval(quote(.data), parent.frame())
      },
      error = function(e) {
        cli_abort("Argument {.arg x} missing, with no default", class = "move2_error_no_x")
      }
    )
    plchlder <- tryCatch(
      {
        eval(quote(.), parent.frame())
      },
      error = function(e) {
        cli_abort(
          "{.fun mt_track_id} can only be used without a {.arg x} argument inside dplyr verbs",
          class = "move2_error_only_dplyr"
        )
      }
    )
    return(.data[[mt_track_id_column(plchlder)]])
  }
  assert_that(mt_is_move2(x))
  track_id_column <- mt_track_id_column(x)
  assert_that(has_name(x, track_id_column),
    msg = format_error("The `track_id_column` attribute indicate the track ids should be contained in the column
                       {.val {track_id_column}}, this column is however not found.")
  )
  track_ids <- x[[track_id_column]]
  assert_valid_track_id(track_ids)
  return(track_ids)
}

assert_valid_track_id <- function(track_ids) {
  assert_that(
    is.factor(track_ids) |
      is_bare_integerish(track_ids) | is.character(track_ids) |
      inherits(track_ids, "integer64"),
    msg =
      "Track id(s) should be of the type integer, integer64, character or factor."
  )
  assert_that(!anyNA(track_ids),
    msg = "Track id(s) should not contain NA values."
  )
}

#' @export
#' @rdname mt_track_id
`mt_track_id<-` <- function(x, value) {
  x <- mt_set_track_id(x, value)
  return(x)
}


#' @export
#' @param value either a vector with new track id values, the name of the new column to define track ids as a scalar
#' character (this column must be present in either the event or track table), or a scalar character to rename the track
#'  id column. IF `value` is `NULL` the move2 class is dropped and a object of the class `sf` is returned.
#' @rdname mt_track_id
#' @examples
#' x <- mt_sim_brownian_motion(t = 1:10, tracks = 2) |>
#'   dplyr::mutate(attrib_evnt = gl(4, 5, labels = c("XX", "YY", "TT", "ZZ"))) |>
#'   mutate_track_data(attrib_trk = c("a", "b"))
#'
#' ## providing a vector with new track ids
#' unique(mt_track_id(x))
#' mt_track_id(x) <- c(rep("track_1", 10), rep("track_2", 10))
#' unique(mt_track_id(x))
#'
#' ## renaming the track id column
#' mt_track_id_column(x)
#' mt_track_id(x) <- "my_new_track_name"
#' mt_track_id_column(x)
#'
#' ## setting a new column to define track ids
#' ## 1. when this column is present in the track table it has to be
#' ## moved to the event table
#' names(mt_track_data(x))
#' x <- mt_as_event_attribute(x, "attrib_trk")
#' mt_track_id(x) <- "attrib_trk"
#' mt_track_id_column(x)
#' unique(mt_track_id(x))
#'
#' ## 2. using an existing column in the event table
#' mt_track_id(x) <- "attrib_evnt"
#' mt_track_id_column(x)
#' unique(mt_track_id(x))
#'
#' ## example of track data attributes being combined
#' m <- mt_sim_brownian_motion(1:3, tracks = letters[5:8]) |>
#'   mutate_track_data(sex = c("f", "f", "m", "m"), age = c(4, 4, 5, 6), old_track = track)
#' new_m <- m |> mt_set_track_id(c(rep("a", 6), rep("b", 6)))
#' mt_track_data(new_m)
mt_set_track_id <- function(x, value) {
  if (is_scalar_character(value) && !has_name(x, value)) {
    if (has_name(mt_track_data(x), value)) {
      x <- x |>
        mt_as_event_attribute(all_of(value), .keep = FALSE) |>
        mt_set_track_id(value = value)
      return(x)
    }
    # Simple rename no further logic is needed
    colnames(x)[colnames(x) == mt_track_id_column(x)] <- value
    d <- mt_track_data(x)
    colnames(d)[colnames(d) == mt_track_id_column(x)] <- value
    attr(x, "track_id_column") <- value
    x <- mt_set_track_data(x, d)
    return(mt_set_track_id_column(x, value)) # set again to assert validity
  }
  if (is.null(value)) {
    # dropping the track_id column
    x[, mt_track_id_column(x)] <- value
    attr(x, "track_id_column") <- value
    class(x) <- setdiff(class(x), "move2")
    return(x)
  }
  if (has_name(mt_track_data(x), value) && value != mt_track_id_column(x)) {
    # both the track data and event data contain the new column name.
    cli_abort("Both the {.arg track_data} and the event data contain the column {.code {value}},
              therefore it is unclear what are the correct track identifiers.
              To resolve this omit one of the two {.code {value}} columns.
              Alternatively {.fn mt_set_track_id_column} could be used
              (e.g. {.code mt_set_track_id_column({rlang::caller_arg(x)}, {rlang::caller_arg(value)})}), this however
              does not check if all tracks have associated {.var track_data}.",
      class = "move2_error_two_track_id_columns"
    )
  }
  new_column_name <- ifelse(is_scalar_character(value), value, mt_track_id_column(x))
  if (is_scalar_character(value)) {
    # if value it the name of the new column retrieve its values
    value <- x[, value, drop = TRUE]
  }
  # check if the values make sense
  if (length(value) != nrow(x)) {
    cli_abort("The new `track_id` column should have the same length as the {.cls move2} object.")
  }
  assert_valid_track_id(value)
  mapping <- distinct(data.frame(old = mt_track_id(x), new = value))
  new_track_data <- merge(mapping, mt_track_data(x),
    by.x = "old",
    by.y = mt_track_id_column(x), all.x = TRUE, suffixes = c(".x", "")
  )

  new_track_data[, 1L] <- NULL
  # now we have matched the old name is no longer needed, by index because name might have changed in merge
  if (new_column_name %in% colnames(new_track_data)[-1L]) {
    new_track_data[, new_column_name] <- NULL # prevent duplicated column names
  }
  colnames(new_track_data)[1L] <- new_column_name

  if (anyDuplicated(new_track_data[, new_column_name])) {
    # some individuals combine previous data generate list columns to retain this duplicated data
    data <- unique(new_track_data[, new_column_name, drop = FALSE])
    for (i in setdiff(colnames(new_track_data), new_column_name)) {
      l <- unname(split(
        new_track_data[, i],
        new_track_data[, new_column_name]
      )[data[, new_column_name]])
      if (all(unlist(lapply(l, length)) == 1L)) {
        l <- unlist(l)
      }
      data[[i]] <- l
    }
    new_track_data <- data
  }

  # no valid df created therefore empty track data
  if (!all(unique(value) %in% new_track_data[, new_column_name])) {
    new_track_data <- setNames(data.frame(unique(value)), new_column_name)
  }
  if (inherits(mt_track_data(x), "tbl")) {
    # to ensure class of track data does not change
    new_track_data <- dplyr::as_tibble(new_track_data)
  }

  # now fit the `move2` object by adding the column setting the attributes and data (not attribute is set twice first
  # time to enable data setting), second time does some quick checks
  x[, new_column_name] <- value
  attr(x, "track_id_column") <- new_column_name
  x <- mt_set_track_data(x, new_track_data)
  return(mt_set_track_id_column(x, new_column_name))
}





#' @rdname mt_track_id
#' @export
mt_n_tracks <- function(x) {
  mt_track_id(x) |>
    unique() |>
    length()
}

#' @export
"[[<-.move2" <- function(x, i, value) {
  x <- structure(NextMethod(), class = c("move2", setdiff(
    class(x),
    "move2"
  )))
  x
}

#' @export
"[<-.move2" <- function(x, i, j, value) {
  x <- NextMethod()
  class(x) <- unique(c("move2", class(x)))
  return(x)
}

#' @export
"[.move2" <- function(x, i, j, ..., drop = FALSE) { # nolint
  time_column_name <- mt_time_column(x)
  track_id_column_name <- mt_track_id_column(x)
  class(x) <- setdiff(class(x), "move2")
  xx <- NextMethod()
  if (drop) {
    class(xx) <- setdiff(class(xx), "move2")
    return(xx)
  } else {
    xx <- structure(xx, class = c("move2", setdiff(
      class(xx),
      "move2"
    )))
    # FIX adding columns back now only works without I index int
    if (missing(i) && !missing(j)) {
      i <- TRUE
    }
    fix <- TRUE
    if (nargs() == 2L && missing(j) && nrow(x) == nrow(xx)) {
      # forexample x["colname"]
      fix <- !(all(vapply(xx, inherits,
        what = "sfc",
        FUN.VALUE = logical(1L)
      )) &
        length(i) == ncol(x) && is.logical(i))
      i <- TRUE
      # fix for code in ggplot2 coord-sf.R (line 67), where using '[' is used to update projections
    }
    if (!time_column_name %in% names(xx) && fix) {
      xx[, time_column_name] <- (x[[time_column_name]])[i]
    }
    if (!track_id_column_name %in% names(xx) && fix) {
      xx[, track_id_column_name] <- (x[[track_id_column_name]])[i]
    }
    xx <- dplyr_reconstruct.move2(xx, x)
    return(xx)
  }
}
