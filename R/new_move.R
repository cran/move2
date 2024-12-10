#' @importFrom dplyr summarise_all transmute_all n_distinct
#' @importFrom tidyselect any_of all_of
#' @importFrom utils URLencode
#' @importFrom rlang is_installed
NULL

#' Create a new move2 object
#'
#' @description Create a new move2 object from a `data.frame`, `sf`, `telemetry`, `telemetry` list, `track_xyt`, `Move` or `MoveStack` object
#' @param x A `data.frame`, `sf`, `telemetry`, `telemetry` list, `track_xyt`, `Move` or `MoveStack` object
#' @export
mt_as_move2 <- function(x, ...) UseMethod("mt_as_move2")

#' @param time_column The name of the column in `x` containing timestamps
#' @param track_id_column The name of the column in `x` containing the track identities
#' @param track_attributes The name(s) of the column(s) that contain track level attributes
#' @param ... Additional arguments passed to \code{\link[sf]{st_as_sf}} if `x` is a `data.frame`, see the details below
#' for more information
#'
#' @details
#' Frequently used arguments to \code{\link[sf]{st_as_sf}} are:
#' * `coords` a character vector indicating the columns used as coordinates, the length is generally two, for `x` and
#'  `y`, but can also be more if `z` is included
#' * `sf_column_name` if a geometry column is present the name of the geometry column to use as coordinates as a
#'  character scalar
#' * `crs` the coordinate reference system to use, either as character, number or a `crs` object for more details see
#' \code{\link[sf]{st_crs}}
#' * `na.fail` normally when the coordinate columns are converted to spatial points `NA` values cause an error, if set
#' to `FALSE` empty points are allowed
#'
#' @return A `move2` object
#' @export
#' @name mt_as_move2
#' @family move2-convert
#'
#' @examples
#' ## create a move2 object from a data.frame and defining projection
#' n <- 5
#' data <- data.frame(
#'   x = cumsum(rnorm(n)), y = cumsum(rnorm(n)),
#'   time = seq(n), track = "a"
#' )
#' mt_as_move2(data,
#'   coords = c("x", "y"), time_column = "time",
#'   track_id_column = "track"
#' ) |> sf::st_set_crs(4326L)
#'
#' ## Dealing with empty coordinates:
#' ## If the data frame contains NA coordinates, the coords argument in sf
#' ## will fail. An alternative is to first create an sfc column,
#' ## or to use the na.fail argument
#' nn <- 3
#' data <- data.frame(
#'   x = c(cumsum(rnorm(n)), rep(NA, nn)), y = c(cumsum(rnorm(n)), rep(NA, nn)),
#'   time = seq(n + nn), track = "a",
#'   sensor = c(rep("sensor1", n), rep("sensor2", nn)),
#'   sensor2values = c(rep(NA, n), runif(nn))
#' )
#' mt_as_move2(data,
#'   coords = c("x", "y"),
#'   na.fail = FALSE,
#'   time_column = "time",
#'   track_id_column = "track"
#' )
#'
#' ## create a move2 object from a sf object
#' data$geometry <- sf::st_sfc(apply(data[, c("x", "y")], 1, sf::st_point, simplify = FALSE))
#' mt_as_move2(data,
#'   sf_column_name = c("geometry"), time_column = "time",
#'   track_id_column = "track"
#' )
#'
mt_as_move2.sf <- function(x, time_column, track_id_column, track_attributes = "", ...) {
  x <- x |>
    new_move(time_column = time_column, track_id_column = track_id_column) |>
    mt_as_track_attribute(any_of(track_attributes))
  assert_valid_time(mt_time(x))
  return(x)
}
#' @export
#' @name mt_as_move2
mt_as_move2.data.frame <- function(x, time_column, track_id_column, track_attributes = "", ...) {
  x <- st_as_sf(x, ...)
  mt_as_move2(
    x = x,
    time_column = time_column,
    track_id_column = track_id_column,
    track_attributes = track_attributes, ...
  )
}

#' @export
#' @name mt_as_move2
mt_as_move2.track_xyt <- function(x, time_column, track_id_column, track_attributes = "", ...) {
  if (!has_name(x, "id")) {
    x$id <- "unnamed"
  }
  NextMethod(x,
    coords = c("x_", "y_"),
    time_column = "t_",
    track_id_column = "id",
    crs = if (is.null(attr(x, "crs"))) {
      NA_crs_
    } else {
      attr(x, "crs")
    }
  )
}



#' @export
#' @name mt_as_move2
mt_as_move2.telemetry <- function(x, time_column, track_id_column, track_attributes = "", ...) {
  track <- if (is.null(x@info$identity)) {
    "unnamed"
  } else {
    as.factor(x@info$identity)
  }
  x <- x |> add_column(track = track, .name_repair = "unique")
  track_id_column <- tail(colnames(x), 1L)
  if ("timestamp" %in% colnames(x)) {
    if (!all.equal(as.numeric(x$timestamp), x$t)) {
      cli_warn(c("The ctmm contains both a `t` and `timestamp` column. The information in these columns differs,
                 as generally the `timestamp` column contains {.cls POSIXct} this one is selected as the time column"),
        class = "move2_warning_telemetry_time_column_confusion"
      )
    }
    time_column <- "timestamp"
  } else {
    time_column <- "t"
  }
  crs <- ifelse(is.null(x@info$projection),
    NA_crs_,
    x@info$projection
  )

  x <- as(x, "data.frame")
  NextMethod(x,
    coords = c("x", "y"),
    time_column = time_column,
    track_id_column = track_id_column,
    crs = crs
  )
}
#' @export
#' @name mt_as_move2

mt_as_move2.list <- function(x, time_column, track_id_column, track_attributes = "", ...) {
  if (!all(unlist(lapply(x, inherits, "telemetry")))) {
    cli_abort("Import from lists is only allowed when all objects in the list are from the type telemetry",
      class = "move2_error_list_import_not_all_telemetry"
    )
  }
  mt_stack(lapply(x, mt_as_move2), .track_combine = "rename")
}

#' @export
#' @name mt_as_move2
mt_as_move2..MoveTrack <- function(x, ...) {
  check_installed("move", "to convert move objects.")
  if (inherits(x, "Move")) {
    track <- rep(rownames(move::idData(x)), move::n.locs(x))
  } else {
    track <- move::trackId(x)
  }
  t <- move::timestamps(x)
  x_sf <- st_as_sf(x)
  # try to prevent duplicating timestamp column so if it exist and is identical to timestamp attribute use it
  if ("timestamp" %in% names(x) && all(x$timestamp == t)) {
    time_column <- "timestamp"
  } else {
    x_tmp <- x_sf |> add_column(timestamp = t, .name_repair = "unique")
    time_column <- setdiff(colnames(x_tmp), colnames(x_sf))
    x_sf <- x_tmp
  }
  x_tmp <- x_sf |> add_column(track = track, .name_repair = "unique")
  track_id_column <- setdiff(colnames(x_tmp), colnames(x_sf))
  x_sf <- x_tmp
  res <- structure(x_sf,
    time_column = time_column,
    track_id_column = track_id_column,
    track_data = move::idData(x, drop = FALSE) |>
      add_column("{track_id_column}" := rownames(move::idData(x, drop = FALSE))), # nolint
    class = c("move2", class(x_sf))
  )
  if (!identical(move::citations(x), character(0L))) {
    res <- res |> mutate_track_data(citation = move::citations(x))
  }
  if (!identical(move::licenseTerms(x), character(0L))) {
    res <- res |> mutate_track_data(license = move::licenseTerms(x))
  }
  return(res)
}

new_move <- function(sf, time_column = "timestamp", track_id_column, track_attributes = "") {
  if (!(time_column %in% colnames(sf))) {
    cli_abort("The {.arg time_column} argument needs to be the name of a column in the dataset", class = "move2_error_no_time_column_in_sf")
  }
  if (!(track_id_column %in% colnames(sf))) {
    cli_abort("The {.arg track_id_column} argument needs to be the name of a column in the dataset", class = "move2_error_not_track_id_column_in_sf")
  }
  r <- structure(sf,
    time_column = time_column,
    track_id_column = track_id_column,
    class = c("move2", class(sf))
  )
  l <- data.frame(unique(mt_track_id(r)))
  names(l) <- track_id_column
  r <- mt_set_track_data(r, l) |>
    mt_as_track_attribute(any_of(track_attributes))
  return(r)
}


#' @export
print.move2 <- function(x, ..., n = getOption("sf_max_print", default = 10L)) {
  avgdur <- mean(do.call(c, lapply(lapply(split(mt_time(x), mt_track_id(x), drop = TRUE), range), diff)))
  if (is_installed("lubridate")) {
    avgdur <- lubridate::make_difftime(as.numeric(avgdur, units = "secs"))
  }
  cat(format_message(
    "A {.cls move2} with `track_id_column` {.val {mt_track_id_column(x)}} and `time_column` {.val {mt_time_column(x)}}"
  ), "\n", sep = "")
  cat(format_message("Containing {mt_n_tracks(x)} track{?s} lasting {?on average} {format(avgdur, digits=3)} in a"), "\n", sep = "")
  NextMethod(n = n)
  # Print individual data
  track_data <- mt_track_data(x)
  if (n > 0L) {
    if (inherits(track_data, "tbl_df")) {
      if (nrow(track_data) > n) {
        cat(paste("First", n, "track features:\n"))
      } else {
        cat(paste("Track features:\n"))
      }
      print(track_data, ..., n = n)
    } else {
      y <- track_data
      if (nrow(y) > n) {
        cat(paste("First", n, "track features:\n"))
        y <- track_data[1L:n, , drop = FALSE]
      } else {
        cat(paste("Track features:\n"))
      }
      print.data.frame(y, ...)
    }
  }
  invisible(x)
}
