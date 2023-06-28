#' @importFrom utils head
#' @importFrom dplyr row_number n
NULL


#' Find subset of records based on time windows
#'
#' @description
#' * `mt_filter_per_interval`: returns a `move2` with the selected records
#' * `mt_per_interval`: returns a logical vector indicating the selected records
#'
#' @param x a move2 object
#' @param unit the time units to select the first record per. This can also be a multiple of units (e.g. "30 seconds").
#' For more details see \code{\link[lubridate]{floor_date}}
#' @param criterion the criterion of what record to select per time interval
#' @param ... additional arguments to \code{mt_per_interval} and \code{\link[lubridate]{floor_date}},
#'  for example the day that starts the week
#'
#' @return `mt_per_interval` returns a logical vector indicating the selected records. \cr
#' `mt_filter_per_interval` returns a filtered `move2` object
#'
#' @family filter
#' @export
#' @examples
#' data <- mt_sim_brownian_motion(as.POSIXct("2022-1-1") + 1:10)
#' data |> mt_filter_per_interval(criterion = "random")
#' data |> mt_filter_per_interval(unit = "3 secs")
#' data[mt_per_interval(data, unit = "6 secs"), ]
mt_filter_per_interval <- function(x, ...) {
  x[mt_per_interval(x, ...), ]
}
#' @rdname mt_filter_per_interval
#' @export
mt_per_interval <- function(x, criterion = c("first", "random", "last"), unit = "hour", ...) {
  criterion <- rlang::arg_match(criterion)
  check_installed("lubridate", "to select the first record per time unit")
  assert_that(mt_is_time_ordered(x))
  xx <- x |>
    group_by(!!!syms(mt_track_id_column(x))) |>
    mutate(flooredDate = lubridate::floor_date(!!!syms(mt_time_column(x)), unit = unit, ...)) |>
    group_by(!!!syms(mt_track_id_column(x)), !!!syms("flooredDate"))
  return(switch(criterion,
    first = mutate(xx, sel = dplyr::row_number() == 1),
    last = mutate(xx, sel = dplyr::row_number() == n()),
    random = mutate(xx, sel = dplyr::row_number() == sample.int(n(), 1))
  ) |> pull("sel"))
}

#' Identify records that are not outliers according to the logic used in movebank
#'
#' @description
#' * `mt_filter_movebank_visible`: returns a `move2` object with all visible data, i.e., excluding all records marked
#' as outliers according to the logic used in movebank (See _Details_)
#' * `mt_movebank_visible`: indicates with `TRUE` the visible records, and with `FALSE` those marked as outliers
#' according to the logic used in movebank (See _Details_)
#'
#'
#' @param x a move2 object
#'
#' @details
#' These functions rely on the columns 'visible', 'algorithm_marked_outlier', 'import_marked_outlier',
#'  'manually_marked_outlier', and/or 'manually_marked_valid'. All of them are expected to be logical. More details
#'   can be found in the \href{http://vocab.nerc.ac.uk/collection/MVB/current/MVB000209/}{movebank vocabulary}
#'
#' @return `mt_movebank_visible`returns a logical vector indicating the records that are valid.\cr
#' `mt_filter_movebank_visible` returns a filtered `move2` object
#' @export
#' @family filter
#' @examples
#' m <- mt_read(mt_example())
#' table(mt_movebank_visible(m))
#' mt_filter_movebank_visible(m)
mt_filter_movebank_visible <- function(x) {
  x[mt_movebank_visible(x), ]
}
#' @export
#' @rdname mt_filter_movebank_visible
mt_movebank_visible <- function(x) {
  if ("visible" %in% colnames(x)) {
    if (!is.logical(x$visible)) {
      cli_abort(
        class = "move2_error_visible_not_logical",
        "The values in the {.arg visible} column are expected to be logical"
      )
    }
    return(x$visible)
  } else {
    res <- rep(TRUE, nrow(x))
    for (col in c(
      "manually-marked-outlier", "algorithm-marked-outlier", "import-marked-outlier",
      "manually_marked_outlier", "algorithm_marked_outlier", "import_marked_outlier"
    )) {
      if (col %in% colnames(x)) {
        l <- x |> pull(col)
        if (!is.logical(l)) {
          cli_abort(
            class = "move2_error_outlier_col_not_logical",
            "The values in the {.arg {col}} column are expected to be logical"
          )
        }
        res <- res & (is.na(l) | !l)
      }
    }

    for (col in c("manually-marked-valid", "manually_marked_valid")) {
      if (col %in% colnames(x)) {
        l <- x |> pull(col)
        if (!is.logical(l)) {
          cli_abort(
            class = "move2_error_valid_col_not_logical",
            "The values in the {.arg {col}} column are expected to be logical"
          )
        }
        res[l] <- TRUE
      }
    }
    return(res)
  }
}
