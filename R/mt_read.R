#' @importFrom vroom vroom
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr mutate bind_cols lead
#' @importFrom assertthat has_name has_attr
#' @importFrom sf st_as_sf st_crs
#' @importFrom units set_units as_units
#' @importFrom utils unzip
NULL

#' Reading files downloaded from movebank
#'
#' @param file The file path to read or a R connection (for details see \code{\link[base]{connections}}).
#' Files can either be csv files from movebank or zip files that are created using Env-DATA.
#'
#' @param ... Arguments passed on to \code{\link[vroom]{vroom}}, for example `col_select`
#'
#' @details
#'
#' Files can be `gz` compressed and if the same columns are present multiple files can be read simultaneously.
#' Using the pipe command in R and some command line tools it is possible to select specific days or months.
#'
#' When using the `col_select` argument of \code{\link[vroom]{vroom}} it is possible to speed up file reading
#' considerably while reducing memory consumption.
#' Especially columns containing acceleration values can become quite large.
#'
#' For files that contain both a \code{individual-local-identifier} and a \code{tag-local-identifier} column a
#' check is preformed if individuals have been wearing multiple tags over time. If this is the case tracks are
#' created based on the combination of both id's. A new column names `individual-tag-local-identifier` in created,
#' which will correspond to the track ids. This somewhat resembles the movebank logic however the track ids do
#' not necessarily correspond to the deployments in movebank as this information is not contained in exported csv's.
#'
#' @return An object of the class `move2`
#'
#' @seealso
#' * [mt_example()] for the path to an example file.
#' @export
#' @examples
#' path_fishers <- mt_example()
#' @examplesIf parallel::detectCores() < 9
#' mt_read(path_fishers)
#'
#' ## Reduce the mount of data read this might provide memory advantages
#' ## and speed up reading
#' mt_read(path_fishers, col_select = c(
#'   "location-long", "location-lat",
#'   "timestamp", "individual-local-identifier"
#' ))
#' ## Read Galapagos Albatross data that has been annotated
#' mt_read(mt_example("Galapagos_Albatrosses-1332012225316982996.zip"))
#' ## Notice this produces a warning as some units are not recognized
#' ## This can be prevented by installing the units
#' \dontrun{
#' units::install_unit("gC", "g", "Grams of carbon")
#' }
#'
#' \dontrun{
#' ## Reading can also be manipulted to speed up or reduce memory consumption
#' ## Here we assume the Galapagos albatross data has been downloaded
#' mt_read("~/Downloads/Galapagos Albatrosses.csv")
#' ## Exclude the column 'eobs:accelerations-raw'
#' mt_read("~/Downloads/Galapagos Albatrosses.csv",
#'   col_select = (!`eobs:accelerations-raw`)
#' )
#' ## Only read records from July 2008 using a system pipe where the data
#' ## is already filtered before reading into R
#' mt_read(pipe('cat "~/Downloads/Galapagos Albatrosses.csv" | grep "2008-07\\|time"'))
#' }
mt_read <- function(file, ...) {
  if (is_scalar_character(file) && grepl("\\.zip$", file)) {
    f <- grep("csv$", unzip(file, list = TRUE)$Name, value = TRUE)
    assert_that(is_scalar_character(f), msg = "Env-DATA zipfiles are only expected to contain one csv file")
    data <- mt_read(unz(file, f), ...)

    readme <- vroom::vroom_lines(unz(file, "readme.txt"))
    readme_vars <- split(readme, cumsum(grepl("^Name: ", readme)))[-1]
    var_names <- sub("Name: ", "", unlist(lapply(readme_vars, head, n = 1L)))
    if (!all(s <- var_names %in% colnames(data))) {
      cli_warn(
        c(
          "There{qty(sum(!s))} {?is/are} {sum(!s)} column{?s} in {.file readme.txt} that are not the csv.",
          "These are: {.var {var_names[!s]}}"
        ),
        class = "move2_warning_no_match_csv_readme"
      )
    }
    if (length(s <- setdiff(colnames(data), c(var_names, names(movebank_column_types), "geometry")))) {
      cli_warn(c("There {qty(length(s))}{?is/are} {length(s)} column{?s} in the csv that {?is/are} not in {.file readme.txt} or the movebank vocabulary. {?This is/These are}: {.var {s}}"),
        class = "move2_warning_no_match_for_vocabulary"
      )
    }
    units <- lapply(readme_vars, grep, pattern = "^Unit: ", value = TRUE) |>
      unlist() |>
      sub(pattern = "^Unit: ", replacement = "")
    names(units) <- var_names
    units_list <- lapply(units, function(x) try(as_units(x), silent = TRUE))

    if (any(ss <- unlist(lapply(units_list, inherits, "try-error")))) {
      cli_warn("The following {qty((unique(units[ss])))}unit{?s} {?is/are} not parsible: {.var {(unique(units[ss]))}}.
           Th{?is/ese} belong{?s/} to: {.var {names(units[ss])}}.
           Therefore these variables will not have units defined.",
        class = "move2_warning_unparsable_units_reading"
      )
    }
    for (i in which(!ss)) {
      data[[names(units_list)[i]]] <- set_units(data[[names(units_list)[i]]], units_list[[i]], mode = "standard")
    }
    return(data)
  }
  data <- suppressWarnings(
    classes = "vroom_mismatched_column_name",
    vroom::vroom(file, delim = ",", ..., col_types = movebank_column_types)
  )
  absent_columns <- movebank_minimal_columns %in% names(data)
  if (!all(absent_columns)) {
    cli_abort(
      class = "move2_error_read_columns_missing",
      c("Not all columns that are expected are present in the file",
        i =
          "The following column{?s} {?is/are} expected but not present:
        {.arg {movebank_minimal_columns[!absent_columns]}}"
      )
    )
  }

  crds <- c("location-long", "location-lat") # Here potentially height can be added
  data <- st_as_sf(data,
    coords = crds,
    crs = st_crs(4326L), na.fail = FALSE
  )

  for (i in names(mb_column_units)[names(mb_column_units) %in% names(data)]) {
    # this is considerably faster then set_units or units<- for frames with millions of records
    data[[i]] <- structure(data[[i]],
      class = "units",
      units = attr(as_units(mb_column_units[i]), "units")
    )
    # units are always doubles see: https://github.com/r-quantities/units/issues/324
    storage.mode(data[[i]]) <- "double"
  }
  track_id_column <- "individual-local-identifier"
  if (all(c("individual-local-identifier", "tag-local-identifier") %in% names(data))) {
    if (any(colSums(table(data$`individual-local-identifier`, data$`tag-local-identifier`) != 0) != 1)) {
      cli_inform("There are multiple tags used for one individual,
                 therefore tracks are defined by the unique combination of tags and individuals.
                 To do so a new column is created with the name {.code individual-tag-local-identifier}.
                 Note this can differ from the deployments as defined in movebank.
                 Currently downloaded csv`s do not contain deployment information.")
      data[["individual-tag-local-identifier"]] <- factor(paste(
        sep = "_",
        data[["individual-local-identifier"]],
        data[["tag-local-identifier"]]
      ))
      if (anyDuplicated(
        unique(sf::st_drop_geometry(data)[, c(
          "individual-local-identifier",
          "tag-local-identifier", "individual-tag-local-identifier"
        )])$`individual-tag-local-identifier`
      )) {
        cli_abort("Combining tag and individual identifiers does not result in unique names",
          class = "move2_error_no_unique_identifiers_when_combining"
        )
      }
      track_id_column <- "individual-tag-local-identifier"
    }
  }
  data <- new_move(data,
    track_id_column = track_id_column,
    track_attributes = movebank_track_attributes
  )
  if ("tag-local-identifier" %in% names(data)) {
    data <- data |> mt_as_track_attribute(any_of(c("tag-local-identifier")))
  }
  if (track_id_column == "individual-tag-local-identifier") {
    data <- data |> mt_as_track_attribute(any_of(c("individual-local-identifier")))
  }
  return(data)
}

#' Get path to `move2` example data
#'
#' The `move2` package comes with an example data files that is directly downloaded from
#' \href{https://www.movebank.org}{movebank}.
#'
#' @param file The name of the file for which the path needs to be retrieved.
#' @details The fisher example dataset is the study "Martes pennanti LaPoint New York" (study id: `69258089`), shared under
#'  the CC-BY-NC license.
#' For more information on the data see LaPoint et al. (2013) Landscape Ecology. doi: 10.1007/s10980-013-9910-0 .
#' This csv file is gz compressed for reduction in package size.
#'
#' The Galapagos Albatrosses (study id: `2911040`) dataset annotated with environmental data using the Env-DATA system.
#' For this dataset two individuals (`4261-2228` & `2131-2131`) were selected. Data was annotated with wind and
#' an productivity variables.
#' @export
#' @return The path to the example file of the `move2` package
#' @seealso
#' * [mt_read()] to read in the file
#' @examples
#'
#' ## Get path to one example
#' mt_example()
#' mt_example("Galapagos_Albatrosses-1332012225316982996.zip")
mt_example <- function(file = c("fishers.csv.gz", "Galapagos_Albatrosses-1332012225316982996.zip")) {
  file <- rlang::arg_match(file)
  system.file("extdata", file, package = "move2", mustWork = TRUE)
}
