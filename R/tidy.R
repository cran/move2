#' @importFrom dplyr filter select group_by slice
#' @importFrom sf NA_agr_
NULL

#' @export
filter.move2 <- function(.data, ..., .dots) { # nolint
  # note this relates to a problem with inheritance in sf: https://github.com/r-spatial/sf/issues/1852
  template <- .data
  class(.data) <- setdiff(class(.data), "move2")
  x <- NextMethod()
  x <- dplyr_reconstruct.move2(x, template)
  x
}

#' @export
select.move2 <- function(.data, ...) { # nolint
  # For the time being re conserve time and trackid columsn
  time_column_name <- mt_time_column(.data)
  track_id_column_name <- mt_track_id_column(.data)

  # Return a sf data frame is the time or track info column is no longer there
  class(.data) <- setdiff(class(.data), "move2")
  x <- NextMethod()
  if (!time_column_name %in% names(x)) {
    x[, time_column_name] <- .data |> pull(!!time_column_name)
  }
  if (!track_id_column_name %in% names(x)) {
    x[, track_id_column_name] <- .data |> pull(!!track_id_column_name)
  }
  dplyr_reconstruct.move2(x, .data)
}

#' @export
group_by.move2 <- function(.data, ..., add = FALSE) { # nolint
  template <- .data
  class(.data) <- setdiff(class(.data), "move2")
  x <- NextMethod()
  dplyr_reconstruct.move2(x, template)
}


#' @export
mutate.move2 <- function(.data, ..., .dots) { # nolint
  template <- .data
  class(.data) <- setdiff(class(.data), "move2")
  x <- NextMethod()
  dplyr_reconstruct.move2(x, template)
}

#' @export
slice.move2 <- function(.data, ..., .dots) { # nolint
  template <- .data
  class(.data) <- setdiff(class(.data), "move2")
  x <- NextMethod()
  dplyr_reconstruct.move2(x, template)
}

#' @export
ungroup.move2 <- function(x, ...) { # nolint
  template <- x
  class(x) <- setdiff(class(x), "move2")
  x <- NextMethod()
  dplyr_reconstruct.move2(x, template)
}


rowwise.move2 <- function(x, ...) {
  template <- x
  class(x) <- setdiff(class(x), "move2")
  x <- NextMethod()
  dplyr_reconstruct.move2(x, template)
}



#' @export
arrange.move2 <- function(.data, ..., .dots) { # nolint
  template <- .data
  class(.data) <- setdiff(class(.data), "move2")
  x <- NextMethod()
  xx <- dplyr_reconstruct.move2(x, template)
  time_column_name <- attr(xx, "time_column")
  track_id_column_name <- attr(xx, "track_id_column")
  assert_that(time_column_name %in% names(x), track_id_column_name %in% names(x))
  # this error should not occur, with arranging columns should not disappear

  return(xx)
}
# dplyr extending functions: https://dplyr.tidyverse.org/reference/dplyr_extending.html
# Dont work as sf doesnt not implement them

# #' @export
dplyr_row_slice.move2 <- function(data, i, ...) { # nolint
  x <- vctrs::vec_slice(data, i)
  dplyr_reconstruct.move2(x, data)
}


# #' @export
dplyr_reconstruct.move2 <- function(data, template) { # nolint
  if (inherits(template, "tbl_df") && !inherits(data, "tbl_df")) {
    data <- dplyr::as_tibble(data)
  }
  if (!inherits(data, "sf") && inherits(template, "sf")) {
    assert_that(has_attr(template, "sf_column"))
    data <- st_as_sf(data, sf_column_name = attr(template, "sf_column"), agr = if (has_attr(template, "agr")) {
      n <- setdiff(names(data), attr(template, "sf_column"))
      t <- attr(template, "agr")[n]
      names(t) <- n
      t
    } else {
      NA_agr_
    })
  }
  time_column_name <- attr(template, "time_column")
  track_id_column_name <- attr(template, "track_id_column")
  # Return a sf data frame is the time or track info column is no longer there
  if (!time_column_name %in% names(data) || !track_id_column_name %in% names(data)) {
    return(data)
  }
  attr(data, "time_column") <- time_column_name
  attr(data, "track_id_column") <- track_id_column_name
  if (!inherits(data, "move2")) {
    class(data) <- unique(c("move2", class(data)))
  }

  # ensuring filtered tracks do get omitted from track data
  tracks <- data |>
    mt_track_id() |>
    unique()
  data <- template |>
    mt_track_data() |>
    filter(!!sym(attr(data, "track_id_column")) %in% tracks) |>
    mt_set_track_data(x = data)

  return(data)
}

group_split.move2 <- function(.tbl, ..., .keep = TRUE) { # nolint
  t <- .tbl
  class(.tbl) <- setdiff(class(.tbl), "move2")
  if (inherits(.tbl, "rowwise_df")) {
    lapply(dplyr::group_split(.tbl, ...), dplyr_reconstruct.move2, template = t)
  } else {
    lapply(dplyr::group_split(.tbl, ..., .keep = .keep), dplyr_reconstruct.move2, template = t)
  }
}


#' @export
left_join.move2 <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) { # nolint
  class(x) <- setdiff(class(x), "move2")
  dplyr_reconstruct.move2(NextMethod(), template = x)
}
