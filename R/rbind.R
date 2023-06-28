#' @export
rbind.move2 <- function(..., deparse.level = 1) { # nolint
  dots <- list(...)
  dots <- dots[!unlist(lapply(dots, is.null))]
  all_track_ids <- unlist(lapply(lapply(dots, mt_track_id), unique))
  if (anyDuplicated(all_track_ids)) {
    dups <- all_track_ids[duplicated(all_track_ids)] # nolint
    cli_abort("There{qty(length(dups))} {?is a/are} duplicated track id{?s} (e.g. {.code {dups}}),
              these prevent binding the {.cls move2} objects.",
      class = "move2_error_rbind_duplicated_track_ids"
    )
  }
  res <- do.call(rbind, lapply(dots, function(x) {
    class(x) <- setdiff(class(x), "move2")
    x
  }))
  track_data <- do.call(rbind, lapply(dots, mt_track_data))
  id_col <- unique(unlist(lapply(dots, mt_track_id_column)))
  assert_that(is.string(id_col))
  attr(res, "track_id_column") <- id_col
  time_col <- unique(unlist(lapply(dots, attr, "time_column")))
  assert_that(is_scalar_character(time_col), msg = "The time column needs to have the same name in all objects.")
  attr(res, "time_column") <- time_col
  class(res) <- c("move2", setdiff(class(res), "move2"))
  res <- mt_set_track_data(res, track_data)
  res
}
