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
#' *
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

#' Convert trajectories into lines
#'
#' @description
#' Converts each track into one line
#'
#' @param x A move object
#'
#' @param ... Arguments passed on to the [summarise][dplyr::summarise] function
#'
#' @return A [sf::sf] object with a line representing the track as geometry for each track.
#'
#' @details
#' Note that all empty points are removed before summarizing. Arguments passed with `...` thus only summarize for the
#' non empty locations.
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
    left_join(mt_track_data(x))
}


#' Move one or more columns to track attributes or event attributes
#' @description
#' * `mt_as_track_attribute`: move a column from the event to the track attributes
#' * `mt_as_event_attribute`: move a column from the track to the event attributes
#'
#' @param x The move2 object
#'
#' @param ... the names of columns to move, it is also possible to use \code{\link[tidyselect:language]{helpers}}.
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
#' # different ways to move column "sex" from event to track attribute
#' sim_data |> mt_as_track_attribute(sex)
#' sim_data |> mt_as_track_attribute(starts_with("s"))
#' sim_data |> mt_as_track_attribute(any_of(c("sex", "age")))
mt_as_track_attribute <- function(x, ...) {
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
  if (is.integer(pos) && length(pos) == 0) {
    pos <- TRUE
  } else {
    pos <- -pos
  }
  updated_data <- mt_track_data(x) |>
    select(-!!attr(x, "track_id_column")) |>
    #  tibble::as_tibble() |>
    bind_cols(to_move) |>
    mt_set_track_data(x = x[, pos])
  return(updated_data)
}

#' @export
#' @rdname mt_as_track_attribute
mt_as_event_attribute <- function(x, ...) {
  expr <- rlang::expr(c(...))
  track_data <- mt_track_data(x)
  pos <- eval_select(expr, data = track_data)
  pos <- pos[names(pos) != mt_track_id_column(x)]
  x <- x |>
    left_join(
      track_data[, c(mt_track_id_column(x), names(pos)), drop = FALSE],
      mt_track_id_column(x)
    ) |>
    mt_set_track_data(track_data[, ifelse(length(pos) == 0, TRUE, -pos), drop = FALSE])
  return(x)
}

#' Retrieve the column with track ids or get the number of tracks
#' @description
#' * `mt_track_id()` retrieve track ids
#' * `mt_track_id(x) <- value` and `mt_set_track_id(x, value)`  replace track ids with new values
#' * `mt_n_tracks()` returns the number of tracks
#'
#' @param x a `move2` object
#'
#' @details
#' When changing the track ids with new values that results in the combination
#' of several tracks, the track attributes of these tracks are also combined.
#' This is done by creating a lists within each column. See examples.
#'
#'
#' @return `mt_track_id` returns a vector of the length of the number of locations that indicated the points belonging to one track. \cr
#'  `mt_n_tracks` returns the number of tracks.
#'
#' @export
#' @examples
#' x <- mt_read(mt_example())
#' mt_n_tracks(x)
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
#' @param value either the new track id values or the name of the new track id column as a scalar character.
#' When the column is present then that column is used, otherwise the existing track id column is renamed
#' @rdname mt_track_id
#' @examples
#' x <- mt_sim_brownian_motion()
#' x$new_id <- gl(4, 5)
#' x |> mt_set_track_id("new_id")
#' mt_track_id(x) <- gl(4, 5)
#' ## example of track data attributes being combined
#' m <- mt_sim_brownian_motion(1:3, tracks = letters[5:8]) |>
#'   mutate_track_data(sex = c("f", "f", "m", "m"), age = c(4, 4, 5, 6), old_track = track)
#' new_m <- m |> mt_set_track_id(c(rep("a", 6), rep("b", 6)))
#' mt_track_data(new_m)
mt_set_track_id <- function(x, value) {
  if (is_scalar_character(value) && !has_name(x, value)) {
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

  new_track_data[, 1] <- NULL
  # now we have matched the old name is no longer needed, by index because name might have changed in merge
  if (new_column_name %in% colnames(new_track_data)[-1]) {
    new_track_data[, new_column_name] <- NULL # prevent duplicated column names
  }
  colnames(new_track_data)[1] <- new_column_name

  if (anyDuplicated(new_track_data[, new_column_name])) {
    # some individuals combine previous data generate list columns to retain this duplicated data
    data <- unique(new_track_data[, new_column_name, drop = FALSE])
    for (i in setdiff(colnames(new_track_data), new_column_name)) {
      l <- split(
        new_track_data[, i],
        new_track_data[, new_column_name]
      )[data[, new_column_name]]
      if (all(unlist(lapply(l, length)) == 1)) {
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
    if (nargs() == 2 && missing(j) && nrow(x) == nrow(xx)) {
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
