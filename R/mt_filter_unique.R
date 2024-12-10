#' @importFrom dplyr group_data enquo
NULL

#' Filter out duplicated records from a `move2` object
#'
#' @description
#' * `mt_filter_unique`: returns a `move2` from which duplicated records have been removed
#' * `mt_unique`: returns a logical vector indicating the unique records
#' By default columns that have a duplicated timestamps and track identifier are filtered
#'
#' @param x The `move2` object to filter
#' @param criterion The criterion to decide what records to filter out. For more information see _Details_ below.
#' @param additional_columns In some cases different sensors or tracking devices
#' might have the same combination of time and track identifier.
#' It might, for example, be desirable to retain records from an accelerometer
#' and gps recorded at the same time.
#' This argument can be used to indicate additional column to include in the grouping
#' within which the records should not be duplicated.
#' See the examples below for its usage.
#' @param ... Arguments passed on to the `mt_unique` function like `criterion` and arguments to the equivalence function
#' when one of the `"subsets"` criteria is use, this allows for example controlling the `tolerance` ([base::all.equal()])
#'
#'
#' @details
#'
#' To make an informed choice of how to remove duplicates, we recommend to first try to understand why the data set has duplicates.
#'
#' Several methods for filtering duplicates are available the options can be controlled through the `criterion`
#' argument:
#'
#' * `"subsets"`: Only records that are a subset of other records are omitted.
#' Some tracking devices first transmit an smaller dataset that does not contain all information, therefore some
#' records may be the same as others only containing additional `NA` values.
#' This strategy only omits those (duplicated) records. As a result duplicates that contain unique information are
#' retained, the dataset is thus not guaranteed to not have unique records afterwards.
#' * `"subsets_equal"`: The same as `"subsets"` however not exact equivalence is tested using [base::identical()] but
#'  rather [base::all.equal()] is used. This makes it possible to allow for small numeric differences to be considered
#'  equal. This can however reduce speed considerably.
#' * `"sample"`: In this case one record is randomly selected from the duplicated records.
#' * `"first"`: Select the first location from a set of duplicated locations. Note that reordering the data will affect
#'  which record is selected. For movebank data no specific order is enforced, ensure that the order of the locations is like you expect (same goes for `"last"`).
#' * `"last"`: Select the last location from a set of duplicated locations.
#'
#' @return `mt_unique`returns a logical vector indicating the unique records.\cr
#' `mt_filter_unique` returns a filtered `move2` object
#' @export
#'
#'
#' @family filter
#' @examples
#' m <- mt_sim_brownian_motion(1:2)[rep(1:4, 4), ]
#' m$sensor_type <- as.character(gl(2, 4))
#' m$sensor_type_2 <- as.character(gl(2, 8))
#' table(mt_unique(m, "sample"))
#' mt_filter_unique(m[, c("time", "track", "geometry")])
#' mt_filter_unique(m[, c("time", "track", "geometry", "sensor_type")],
#'   additional_columns = sensor_type
#' )
#' if (requireNamespace("dplyr")) {
#'   mt_filter_unique(m, additional_columns = across(all_of(c("sensor_type", "sensor_type_2"))))
#' }
#' mt_filter_unique(m, "sample")
#' mt_filter_unique(m, "first")
#' m$sensor_type[1:12] <- NA
#' mt_filter_unique(m[, c("time", "track", "geometry", "sensor_type")])
#'
#' @examplesIf requireNamespace("dplyr")
#' ## Sometimes it is desirable to not consider specific columns for finding
#' ## the unique records. For example the record identifier like `event_id`
#' ## in movebank This can be done by reducing the data.frame used to identify
#' ## the unique records e.g.:
#' m$event_id <- seq_len(nrow(m))
#' m[mt_unique(m |> dplyr::select(-event_id, -ends_with("type_2"))), ]
#' ## Note that because we subset the full original data.frame the
#' ## columns are not lost
#'
#' @examplesIf parallel::detectCores() < 9 && requireNamespace("dplyr")
#' ## This example is to retain the duplicate entry which contains the least
#' ## number of columns with NA values
#' require(dplyr)
#' mv <- mt_read(mt_example())
#' mv <- dplyr::bind_rows(mv, mv[1:10, ])
#' mv[, "eobs:used-time-to-get-fix"] <- NA
#' mv_no_dup <- mv %>%
#'   mutate(n_na = rowSums(is.na(pick(everything())))) %>%
#'   arrange(n_na) %>%
#'   mt_filter_unique(criterion = "first") %>%
#'   arrange(mt_track_id()) %>%
#'   arrange(mt_track_id(), mt_time())
mt_filter_unique <- function(x, ...) {
  x[mt_unique(x, ...), ]
}

# COMBAK remove as.character if this fix is propagated: https://github.com/r-spatial/sf/issues/2138
#' @export
#' @rdname mt_filter_unique
mt_unique <- function(x, criterion = c("subsets", "subsets_equal", "sample", "first", "last"), additional_columns = NULL, ...) {
  criterion <- rlang::arg_match(criterion)
  assert_that(inherits(x, "move2"))
  additional_columns <- enquo(additional_columns)
  x_grp <- group_by(x, across(all_of(c(
    mt_track_id_column(x),
    mt_time_column(x)
  ))), !!additional_columns)
  switch(criterion,
    first = return(seq_len(nrow(x)) %in% lapply(group_data(x_grp)$.rows, head, n = 1L)),
    last = return(seq_len(nrow(x)) %in% lapply(group_data(x_grp)$.rows, tail, n = 1L)),
    sample = {
      return(seq_len(nrow(x)) %in% unlist(lapply(group_data(x_grp)$.rows, resample)))
    },
    subsets = {
      d <- group_data(x_grp)$.rows
      rws <- (l <- lapply(d, slice_subsets, x = x)) |>
        unlist() |>
        sort()
      if (any(unlist(lapply(l, length)) != 1)) {
        rlang::warn(
          "After removing all records that are subsets of other records there are still remaining duplicates.",
          class = "move2_warning_duplicates_remaining"
        )
      }
      return(seq_len(nrow(x)) %in% rws)
    },
    subsets_equal = {
      d <- group_data(x_grp)$.rows
      rws <- (l <- lapply(d, slice_subsets, x = x, equivalance_fun = function(...) {
        isTRUE(all.equal(...))
      }, ...)) |>
        unlist() |>
        sort()
      if (any(unlist(lapply(l, length)) != 1)) {
        rlang::warn(
          "After removing all records that are subsets of other records there are still remaining duplicates.",
          class = "move2_warning_duplicates_remaining"
        )
      }
      return(seq_len(nrow(x)) %in% rws)
    }
  )
}


# support function mt_unique
resample <- function(x) x[sample.int(length(x), size = 1)]


slice_subsets <- function(rws, x, equivalance_fun = identical, ...) {
  if (length(rws) == 1) {
    return(rws)
  }
  mt_track_id(x) <- NULL

  rws <- rws[!duplicated(x[rws, TRUE])]
  if (length(rws) == 1) {
    return(rws)
  }
  # now there are only rows left that differ
  e <- expand.grid(as.character(rws), as.character(rws))
  e <- e[e[, 1] != e[, 2], ]
  l <- lapply(rws, function(i, x) {
    x[i, TRUE]
  }, x = x)
  names(l) <- rws
  r <- apply(e, 1, function(i, xx, ...) {
    all(mapply(equivalance_fun, xx[[i[1]]], xx[[i[2]]], ...) | is.na(xx[[i[2]]]))
  }, xx = l, ...) # true when second record is equal or NA compared to first
  # remove records that are duplicated (in case using all.equal and pass first duplication test)
  r[duplicated(cbind(t(apply(e, 1, sort)), r)) & r] <- FALSE
  # omit all records that are subset of other
  return(rws[!(rws %in% unique(e[r, 2]))])
}
