#' Combine multiple `move2` objects into one
#'
#' @description This function does a similar job to [dplyr::bind_rows()], when columns are missing of any of the
#' objects, they are added.
#'
#' @param ... Either a list of `move2` objects to combine or the objects to combine as separate arguments
#' @param .track_combine A character string indicating the way duplicated tracks should be resolved. By default ("check_unique")
#' an error is raised if different objects contain tracks with the same name. With "merge" and "merge_list" tracks with the same name can be merged, and with "rename" non unique tracks can be renamed.
#' @param .track_id_repair The way in which names should be repaired when renaming is done, see [vctrs::vec_as_names()]
#'  for more details on each option
#'
#' @inherit mt_read return
#' @details
#'
#' An attempt is made to combine objects that have a different `track_id_column` or `time_column`, however this is
#'  only done if it can be done without data loss.
#'
#' When objects are too different (e.g. different projection or different types of time columns that cannot
#' be combine) and error is raised.
#' When tracks have the same name in different objects to combine this will results in an error.
#'
#' When merging several tracks, the track attributes of these tracks are also combined.
#' For track data this can result in conflicts. With "merge" unique values are selected if not one unique value is present a warning is raised.
#' With "merge_list" a list column is created for each track attribute that can be summarized later.
#'
#' @seealso rbind
#'
#' @export
#' @examples
#' a <- mt_sim_brownian_motion(1:2, tracks = c("a", "b"))
#' b <- mt_sim_brownian_motion(1:2, tracks = c("g", "h"))
#' mt_stack(a, b)
#'
#' ## having different columns does not cause problems
#' a$extra_data <- 1:nrow(a)
#' mt_stack(list(a, b))
#'
#' @examplesIf parallel::detectCores() < 9
#' ## Combining different datasets works
#' fishers <- mt_read(mt_example(), n_max = 100, col_select = c(
#'   "eobs:used-time-to-get-fix",
#'   "location-long", "location-lat", "timestamp", "individual-local-identifier"
#' ))
#'
#' ## Objects to stack need to have the same CRS, use either st_set_crs
#' ## or st_transform depending what is appropriate
#' random_track <- mt_sim_brownian_motion(
#'   t = as.POSIXct("1970-1-1") + 1:3,
#'   tracks = factor(letters[1:2])
#' ) |> sf::st_set_crs(4326)
#' mt_time(random_track) <- "timestamp"
#' mt_stack(
#'   random_track,
#'   fishers
#' )
#' track_1 <- mt_sim_brownian_motion(tracks = letters[1:3], t = 1:3) |>
#'   mutate_track_data(sex = "f")
#' track_2 <- mt_sim_brownian_motion(tracks = letters[3:4], t = 4:6) |>
#'   mutate_track_data(sex = c("f", "m"))
#' mt_stack(track_1, track_2,
#'   .track_combine = "merge_list"
#' )
#' mt_stack(track_1, track_2,
#'   .track_combine = "merge"
#' )
#' \donttest{
#' if (requireNamespace("units")) {
#'   males <- tail(filter_track_data(
#'     fishers,
#'     grepl("M", `individual-local-identifier`)
#'   ), 5)
#'   females <- filter_track_data(
#'     fishers,
#'     grepl("F", `individual-local-identifier`)
#'   )
#'   females$`eobs:used-time-to-get-fix` <- units::set_units(
#'     females$`eobs:used-time-to-get-fix`,
#'     "hours"
#'   )
#'   females <- tail(females, 5)
#'   ## combining with different units works correctly (units are unified with correct conversion)
#'   mt_stack(males, females)
#' }
#' }
mt_stack <- function(..., # nolint cyclo complexity to reduce
                     .track_combine = c("check_unique", "merge", "merge_list", "rename"),
                     .track_id_repair = c(
                       "unique", "universal", "unique_quiet",
                       "universal_quiet"
                     )) {
  dots <- rlang::list2(...)
  if (length(dots) == 1 && rlang::is_bare_list(dots[[1]])) {
    dots <- dots[[1]]
  }
  assert_that(all(unlist(lapply(dots, inherits, "move2"))),
    msg = "Not all objects inherit the `move2` class."
  )
  if (inherits(
    tryCatch(do.call(vctrs::vec_ptype_common, lapply(dots, mt_track_id)),
      error = function(c) {
        msg <- conditionMessage(c)
        invisible(structure(msg, class = "try-error"))
      }
    ), "try-error"
  )) {
    dots <- lapply(dots, function(x) {
      if (is_bare_integerish(mt_track_id(x)) |
        inherits(mt_track_id(x), "integer64")) {
        x <- mt_set_track_id(x, as.factor(mt_track_id(x)))
      }
      return(x)
    })
  }
  track_ids <- do.call(vctrs::vec_c, lapply(lapply(dots, mt_track_id), unique))
  .track_combine <- rlang::arg_match(.track_combine)
  assert_that(!is.na(.track_combine))

  if (anyDuplicated(track_ids)) {
    if (.track_combine == "check_unique") {
      cli_abort(
        class = "move2_error_duplicated_track_id",
        c(
          x = "There {qty(sum(duplicated(track_ids)))}{?is a/are} duplicated track identifier{?s} in the {.cls move2}
          objects to combine (e.g. {.code {track_ids[duplicated(track_ids)]}}).",
          i = "An explicit choice needs to be made if individuals with the same name need to be merged or renamed by
          using the {.arg .track_combine} argument."
        )
      )
    }
    if (.track_combine == "rename") {
      .track_id_repair <- rlang::arg_match(.track_id_repair)

      new_track_ids <- vctrs::vec_as_names(as.character(track_ids), repair = .track_id_repair)
      names(new_track_ids) <- track_ids
      n_tracks <- lapply(dots, mt_n_tracks) |> unlist()
      new_track_ids_split <- split(new_track_ids, rep(seq_len(length(dots)), n_tracks))
      dots <- mapply(function(x, names) {
        mt_track_id(x) <- dplyr::recode(mt_track_id(x), !!!names)
        x
      }, dots, new_track_ids_split, SIMPLIFY = FALSE)
    }
  }
  track_id_column_name <- unlist(lapply(dots, mt_track_id_column))
  if (length(unique(track_id_column_name)) != 1) {
    all_col_names <- unlist(
      mapply(c, lapply(dots, colnames),
        lapply(lapply(dots, mt_track_data), colnames),
        SIMPLIFY = FALSE
      )[track_id_column_name != track_id_column_name[1]]
    )
    if (track_id_column_name[1] %in% all_col_names) {
      cli_abort(
        x = "The {.code track_id_column} differs between the objects to stack and renaming would overwrite
                existing data.",
        class = "move2_error_no_unique_track_id_column"
      )
    }
    dots <- lapply(dots, mt_set_track_id, value = track_id_column_name[1])
    cli_warn("The {.code track_id_column} differs between the objects to stack, for successfull stacking all
             {.code track_id_column} attributes have been renamed to {.code {track_id_column_name[1]}}")
  }
  time_column_name <- unlist(lapply(dots, mt_time_column))
  if (length(unique(time_column_name)) != 1) {
    all_col_names <- unlist(
      mapply(c, lapply(dots, colnames),
        lapply(lapply(dots, mt_track_data), colnames),
        SIMPLIFY = FALSE
      )[time_column_name != time_column_name[1]]
    )
    if (time_column_name[1] %in% all_col_names) {
      cli_abort("The {.code time_column} differs between the objects to stack and renaming would overwrite
                existing data.",
        class = "move2_error_no_unique_time_column"
      )
    }
    dots <- lapply(dots, mt_set_time, value = time_column_name[1])
    cli_warn("The {.code time_column} differs between the objects to stack, for successfull stacking all
             {.code time_column} attributes have been renamed to {.code {time_column_name[1]}}",
      class = "move2_warning_differing_time_column"
    )
  }
  dots <- lapply(dots, function(x) {
    class(x) <- setdiff(class(x), "move2")
    x
  })

  stacked <- dplyr::bind_rows(dots)
  new_track_data <- dplyr::bind_rows(lapply(dots, mt_track_data))
  if (.track_combine == "merge_list") {
    new_track_data <- summarise(new_track_data, across(
      everything(),
      ~ list(.x)
    ), .by = unname(track_id_column_name[1]))
  }
  if (.track_combine == "merge" && anyDuplicated(new_track_data[, track_id_column_name[1]])) {
    summary_unique <- summarise(
      new_track_data,
      across(everything(), ~ (length(unique(.x)) != 1)),
      .by = unname(track_id_column_name[1])
    ) |>
      select(-track_id_column_name[1]) |>
      summarise(across(everything(), ~ any(.x))) |>
      unlist()
    if (any(summary_unique)) {
      cli_warn(
        "The column{?s} {.field {names(summary_unique)[summary_unique]}}, {?does/do} not have one unique value per track,
               therefore these values are replaced by {.code NA}. Consider using {.code .track_combine='merge_list'}
               and use a informed merge strategy for the track data to combine the list columns and avoid information loss.",
        class = "move2_warning_no_unique_value_in_merge_track_data"
      )
    }
    new_track_data <- summarise(
      new_track_data,
      across(everything(), ~ if_else(length(unique(.x)) == 1, head(.x, n = 1), NA)),
      .by = unname(track_id_column_name[1])
    )
  }
  attr(dots[[1]], "track_data") <- new_track_data
  stacked <- dplyr_reconstruct.move2(stacked, dots[[1]])
  return(stacked)
}
