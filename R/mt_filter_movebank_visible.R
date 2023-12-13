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
#' @examplesIf parallel::detectCores() < 9
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
