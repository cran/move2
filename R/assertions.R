#' @importFrom assertthat on_failure<- see_if
#' @importFrom rlang %||%
NULL


#' Functions for asserting properties of a `move2` object
#'
#' @description
#' * `mt_is_track_id_cleaved()` asserts all tracks are grouped in the data set, they occur consecutively.
#' * `mt_is_time_ordered()` checks if all tracks are groups and if timestamps within a track are ascending (i.e. the
#'  time differences between successive locations are equal or above 0).
#' * `mt_has_unique_location_time_records()` checks if all records with a location have a unique timestamp (i.e. checks
#' for duplicated timestamps within a track).
#' * `mt_is_time_ordered_non_empty_points()` this assertion combines the `mt_is_time_ordered()` and
#' `mt_has_no_empty_points()` assertions and thus ensures that each record has a location and timestamps are ordered.
#' * `mt_has_no_empty_points()` asserts all geometries are points and that there are no empty points.
#' * `mt_is_move2()` asserts `x` inherits the class `move2`
#'
#' @param x a `move2` object
#' @param non_zero If `TRUE` only intervals longer than 0 are considered ordered (i.e. no coinciding timestamps),
#'  if `FALSE` also 0 intervals are considered ordered
#'
#' @return a logical value if the asserted property is `TRUE` or `FALSE`
#'
#' @details For these functions an \code{\link[assertthat]{on_failure}} error
#' function is defined. This results in meaningful error messages when the
#' function is used in combination with \code{\link[assertthat]{assert_that}}. These functions can also be used in
#' normal logical operations as `TRUE` or `FALSE` is returned.
#'
#'
#' @rdname assertions
#' @export
#' @examples
#' ## examples of what to do if assertion if FALSE
#' n <- 8
#' data <- data.frame(
#'   x = cumsum(rnorm(n)), y = cumsum(rnorm(n)),
#'   time = seq(n), track = sample(c("a", "b"), size = n, replace = TRUE)
#' )
#' data <- rbind(data, data[sample(nrow(data), 2), ]) # adding duplicate timestamps
#' mv <- mt_as_move2(data,
#'   coords = c("x", "y"),
#'   time_column = "time",
#'   track_id_column = "track"
#' )
#' mv$geometry[c(1, 3)] <- sf::st_point() # adding empty locations
#'
#' mt_is_track_id_cleaved(mv)
#' mv <- dplyr::arrange(mv, mt_track_id(mv))
#'
#' mt_is_time_ordered(mv)
#' mv <- dplyr::arrange(mv, mt_track_id(mv), mt_time(mv))
#'
#' mt_has_unique_location_time_records(mv)
#' mv <- mt_filter_unique(mv)
#'
#' mt_has_no_empty_points(mv)
#' mv <- dplyr::filter(mv, !sf::st_is_empty(mv))
#'
#' ## example of using the assertions with assertthat
#' if (requireNamespace("assertthat")) {
#'   m <- mt_sim_brownian_motion(t = 1:2, tracks = 2)
#'   assertthat::see_if(mt_is_track_id_cleaved(m))
#'   assertthat::see_if(mt_is_track_id_cleaved(m[c(3, 1, 2, 4), ]))
#'   assertthat::see_if(mt_is_time_ordered(m[c(2:1, 3, 4), ]))
#'   assertthat::see_if(mt_has_unique_location_time_records(m[c(1, 1, 2, 3, 4), ]))
#'   assertthat::see_if(mt_is_move2(m$time))
#' }
#'
mt_is_track_id_cleaved <- function(x) {
  # factor numeric is needed to account for character, factor and numeric returns
  individual_changes <- sum(diff(as.numeric(factor(mt_track_id(x)))) != 0L)
  return((individual_changes + 1L) == mt_n_tracks(x))
}
on_failure(mt_is_track_id_cleaved) <- function(call, env) {
  movetrk <- eval(call$x, envir = env)
  x <- rle(as.character(mt_track_id(movetrk)))$values # nolint
  format_error(c("Not all tracks are grouped in {.arg {as_label(call$x)}}.",
    i = "The following {qty(length(unique(x[duplicated(x)])))}track{?s} occu{?rs/r} at multiple places in
                 the data set: {.var {unique(x[duplicated(x)])}}.",
    i = "This can be resolved resolved by ordering the data e.g.:
                 {.run {as_label(call$x)}[order(mt_track_id({as_label(call$x)})),]} "
  ))
}


#' @export
#' @rdname assertions
mt_is_time_ordered <- function(x, non_zero = FALSE) {
  assert_that(mt_is_track_id_cleaved(x))
  t <- mt_time(x)
  id <- mt_track_id(x)
  if (is.character(id)) {
    id <- factor(id)
  }
  return(all(ifelse(non_zero, `>`, `>=`)(as.numeric(diff(t)), 0L) |
    diff(as.numeric(id)) != 0L))
}
on_failure(mt_is_time_ordered) <- function(call, env) {
  x <- eval(call[["x"]], envir = env)
  non_zero <- eval(call$non_zero %||% formals(mt_is_time_ordered)$non_zero, envir = env)
  t <- mt_time(x)
  id <- mt_track_id(x)
  if (is.character(id)) {
    id <- factor(id)
  }

  i <- which(!(ifelse(non_zero, `>`, `>=`)(as.numeric(diff(t)), 0L) | # nolint
    diff(as.numeric(id)) != 0L))[1L]
  format_error(c(
    e = "Not all timestamps in {.arg {as_label(call$x)}} are ordered within track.",
    i = ifelse(non_zero,
      "It is required that all record have a positive time difference compared to the previous one.",
      "It is required that all subsequent records have an equal or later timestamps."
    ),
    i = " The first offending record is of track: {.val {id[i]}} at time: {.val {t[i]}}
                 (record: {.val {i}}), the next record has an earlier {if(non_zero){'or equal'}} timestamp."
  ))
}


#' @export
#' @rdname assertions
mt_has_unique_location_time_records <- function(x) { # nolint
  empty_points <- st_is_empty(x)
  t <- mt_time(x)
  if (inherits(t, "POSIXt")) { # pre convert POSIXt to speedup
    t <- as.numeric(t)
  }
  duplicates <- duplicated(data.frame(mt_track_id(x), t, empty_points))
  duplicates[empty_points] <- FALSE
  return(!any(duplicates))
}
on_failure(mt_has_unique_location_time_records) <- function(call, env) { # nolint
  x <- eval(call$x, envir = env)
  empty_points <- st_is_empty(x)
  t <- mt_time(x)
  if (inherits(t, "POSIXt")) {
    t <- as.numeric(t)
  }
  duplicates <- duplicated(cbind(mt_track_id(x), t, empty_points))
  duplicates[empty_points] <- FALSE
  format_error(c("Not all (non empty) locations within a track have a unique timestamp.",
    i = "In total there {?is/are} {sum(duplicates)} duplicat{?ed/e} timestamp{?s}.",
    i = "The first duplicated record is from track {mt_track_id(x)[duplicates][1]} at time {mt_time(x)[duplicates][1]}
    (record: {which(duplicates)[1]}).",
    i = "The {.fun mt_filter_unique} function might help to resolve this."
  ))
}

#' @export
#' @rdname assertions
mt_is_time_ordered_non_empty_points <- function(x, non_zero = FALSE) { # nolint
  mt_has_no_empty_points(x) && mt_is_time_ordered(x, non_zero = non_zero)
}
on_failure(mt_is_time_ordered_non_empty_points) <- function(call, env) { # nolint
  x <- eval(call$x, envir = env)
  a <- see_if(mt_has_no_empty_points(x))
  if (!a) {
    return(attr(a, "msg"))
  }
  a <- see_if(mt_is_time_ordered(x))
  return(attr(a, "msg"))
}
#' @export
#' @rdname assertions
mt_has_no_empty_points <- function(x) {
  all(st_is(x, "POINT")) & !any(st_is_empty(x))
}
on_failure(mt_has_no_empty_points) <- function(call, env) {
  x <- eval(call$x, envir = env)
  i <- which(!(st_is(x, "POINT") & !st_is_empty(x))) # nolint
  format_error(c("Not all locations are non empty points.",
    i = "{qty(length(i))}At least the following record{?s} {?is/are} not{? a/ } non-empty point{?s}:
                 {.val {i}}"
  ))
}
#' @export
#' @rdname assertions

mt_is_move2 <- function(x) {
  inherits(x, "move2")
}
on_failure(mt_is_move2) <- function(call, env) {
  format_error("The input does not inherit the {.cls move2} class")
}
