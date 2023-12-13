# https://github.com/movebank/movebank-api-doc/blob/master/movebank-api.md#displaying-data-using-google-maps

#' @importFrom rlang cnd_signal is_scalar_integerish is_scalar_character catch_cnd abort .data format_error_bullets sym
#' @importFrom rlang syms is_scalar_vector is_scalar_logical as_label .env
#' @importFrom sf st_geometry st_distance st_is st_is_empty st_as_sfc st_sf
#' @importFrom assertthat assert_that is.string
#' @importFrom dplyr filter_ rename select_if rename_with across everything
#' @importFrom vroom vroom_lines
#' @importFrom bit64 as.integer64
#' @importFrom cli cli_warn cli_inform format_message format_error cli_abort qty
NULL

utils::globalVariables("where") # for tidy select

#' Download data from movebank
#'
#' @description
#' * `movebank_download_study` downloads a complete study from Movebank by the study id or name.
#' * `movebank_download_deployment` downloads all tag, individual and deployment information and merges it into one
#' `data.frame`
#' * `movebank_download_study_info` downloads all study level information, either for all studies, one study with
#' the argument `id` or a subset, for example, `license_type = "CC_0"`
#' * `movebank_retrieve` is a more flexible function for retrieving information directly from the api.
#' * `movebank_get_study_id` using a character string retrieve the associated study id.
#'
#'
#' @param study_id the study id as a number or a character string can be used to identify a study. This character string
#' needs to be unique enough to identify one and only one study. Argument applicable to all functions.
#' @param attributes a character vector with the event data attributes to download. By default `"all"` are downloaded,
#' this make it slightly slower, to speed up `NULL` can be used as it reduces it to the minimal set of required
#' attributes (only for location data). Alternatively a vector of attributes can be provided (the minimal ones are
#' automatically added). Argument applicable to `movebank_download_study` and `movebank_retrieve`. See 'Details' for more information.
#' @param remove_movebank_outliers if `TRUE` outliers according to the movebank logic are removed. This should
#' correspond to the `visible` attribute in movebank. Argument applicable to `movebank_download_study` and `movebank_retrieve`.
#' @param convert_spatial_columns if `TRUE` column pairs containing spatial data will be converted to an `sfc` column. Argument applicable to all functions.
#'
#' @param ... arguments added to the
#' [movebank api](https://github.com/movebank/movebank-api-doc/blob/master/movebank-api.md) call. See 'Details' for some
#' common arguments.
#'
#' @param handle the curl handle used to perform the api call, generally this is extracted from the system keyring if
#' correctly set up with `movebank_store_credentials`. Argument applicable to all functions.
#'
#'
#' @details
#' Caution, when downloading data with `movebank_download_study` without specifying the sensor in the argument `sensor_type_id`
#' (see below), all data of all sensors will be downloaded, but only the attributes of location sensors will be
#' included. We recommend to always specify the sensor(s) to ensure that all associated attributes are downloaded.
#' Use e.g. `movebank_download_study_info(study_id=my_study_id)$sensor_type_ids` to find out which sensors are
#' available in a given study.
#' `attributes = "all"` is the default, and it will include only location sensor attributes if no sensor is specified
#' in `sensor_type_id`. When sensors are specified, it will download all associated attributes of all sensors.
#' `attributes = NULL` should only be used when downloading location data (by specifying the sensor), as only timestamp,
#' location and track id is downloaded. To specify only a subset of attributes to download, check the list of attributes
#' available for a specific sensor (e.g. GPS) in a given study, use
#' `movebank_retrieve(entity_type = "study_attribute", study_id = myStudyID, sensor_type_id = "gps")$short_name`
#' (more details in "Downloading data from movebank" vignette).
#'
#' The api is quite flexible for adjusting requests. This is elaborately documented in the
#' [movebank api documentation](https://github.com/movebank/movebank-api-doc/blob/master/movebank-api.md). To identify the available arguments, please note that `movebank_download_study` is based on the entity_type "event", `movebank_download_study_info` on the entity_type "study" and `movebank_download_deployment` on the entity_type "deployment", "individual" and "tag".
#' Here a list of a few arguments that are common for the entity_type "event":
#'
#' * `sensor_type_id` can be used to restrict the download to specific sensors. It can be either a character or and
#' integer with the tag_type. For a full list of options see: `movebank_retrieve(entity_type='tag_type')`, values from
#' the `id` and `external_id` columns are valid.
#' * `timestamp_start` and `timestamp_end` can be used to limit the temporal range to download.
#' This argument can either be formatted as a `POSIXct` timestamp, `Date` or a character string
#' (e.g. `"20080604133046000"`(`yyyyMMddHHmmssSSS`))
#' * `event_reduction_profile` might be useful to reduce the data downloaded (e.g. daily locations) possible values
#'  are character strings (e.g. `"EURING_01"`). For details see the movebank api
#' [documentation](https://github.com/movebank/movebank-api-doc/blob/master/movebank-api.md#get-reduced-event-data).
#' Note that for the time being the required attributes need to be explicitly stated (e.g. `attributes = NULL`)
#' as `"all"` does not work with the current movebank api.
#' * `individual_local_identifier` for selecting one or more individuals by the local identifier
#'
#' For more elaborate usage see `vignette("movebank", package='move2')`
#'
#' @return
#' `movebank_download_study` returns a `move2` object. \cr
#' `movebank_retrieve`, `movebank_download_deployment`, `movebank_download_study_info` return a `data.frame`/`tbl`.\cr
#' `movebank_get_study_id` returns a \code{\link[bit64:integer64]{big integer}}.
#'
#'
#' @family movebank-download
#' @export
#' @examples
#' \dontrun{
#' ## download entire study (all data of all sensors)
#' movebank_download_study_info(study_id = myStudyID)$sensor_type_ids
#' movebank_download_study(2911040, sensor_type_id = c("gps", "acceleration"))
#'
#' ## download data of one individual
#' movebank_download_study(2911040,
#'   individual_local_identifier = "unbanded-160"
#' )
#' ## download gps data for multiple individuals
#' movebank_download_study(2911040,
#'   sensor_type_id = "gps",
#'   individual_local_identifier = c("1094-1094", "1103-1103")
#' )
#' movebank_download_study(2911040,
#'   sensor_type_id = "gps",
#'   individual_id = c(2911086, 2911065)
#' )
#' ## download acceleration data of one or several individuals
#' movebank_download_study(2911040,
#'   sensor_type_id = "acceleration",
#'   individual_local_identifier = "1094-1094"
#' )
#' ## download data of a specific time window and sensor
#' movebank_download_study(2911040,
#'   sensor_type_id = "gps",
#'   timestamp_start = as.POSIXct("2008-08-01 00:00:00"),
#'   timestamp_end = as.POSIXct("2008-08-03 00:00:00")
#' )
#'
#' ## download study filtered to one location per day
#' ## (see movebank api documentation for options)
#' ## also possible to add specific columns in "attributes"
#' movebank_download_study(2911040,
#'   sensor_type_id = "gps",
#'   event_reduction_profile = "EURING_01",
#'   attributes = NULL
#' )
#' ## download data associated to tag, individual and deployment
#' movebank_download_deployment(2911040)
#' ## download study information for all studies
#' movebank_download_study_info()
#' ## download study information for all studies where you have
#' ## access to download the data
#' movebank_download_study_info(i_have_download_access = TRUE)
#' ## download study information for a specific study
#' movebank_download_study_info(id = 2911040)
#' ## get study id
#' movebank_get_study_id(study_id = "Galapagos Albatrosses")
#' ## Find studies you can download and have a creative commons zero license
#' ## Note "CC_BY" is also frequently used
#' movebank_download_study_info(
#'   license_type = "CC_0",
#'   i_have_download_access = TRUE,
#'   attributes = c("name", "id")
#' )
#' }
#'
movebank_download_study <- function(study_id, attributes = "all", # nolint
                                    ...,
                                    remove_movebank_outliers = TRUE) {
  study_id <- movebank_get_study_id(study_id, ...)

  assert_that(is.flag(remove_movebank_outliers), !is.na(remove_movebank_outliers))


  data <- movebank_retrieve(
    entity_type = "event",
    study_id = study_id, attributes = mb_include_minimal_attributes(attributes), ...
  )
  if (is_scalar_character(attributes) &&
    attributes == "all" &&
    !any((c("location_long", "location_lat") %in% colnames(data)))) {
    # if only sensor data is downloaded add NA locations
    data$location_long <- NA_real_
    data$location_lat <- NA_real_
  }

  if (nrow(data) == 0) {
    cli_abort(
      class = "move2_error_no_data_found",
      "The request did not result in any data, this might be caused by trying to retrieve a none existant individual.
        Or no data is available for download in this study for your user account."
    )
  }
  if (all(is.na(data$deployment_id))) {
    cli_abort(
      class = "move2_error_no_deployed_data",
      c(e = "Even though {sum(is.na(data$deployment_id))} event record{?s} were downloaded, none seem to be deployed
        ({.code deployment_id} is {.code NA} for all records).", i = "This can be resolved by defining deployments in
        movebank ({.url https://www.movebank.org/cms/movebank-content/deployment-manager}).")
    )
  }
  if (anyNA(data$deployment_id)) {
    cli_inform(
      class = "move2_inform_omit_deployment_id_na",
      c(i = "In total {sum(is.na(data$deployment_id))} record{?s} were omitted as they were not deployed (the
        {.code deployment_id} was {.code NA}).")
    )
    data <- data |>
      filter(!is.na(.data$`deployment_id`))
  }
  if (all(is.na(data$location_long)) && all(is.na(data$location_lat))) {
    data$geometry <- st_as_sfc(list(st_point()), crs = st_crs(4326L))[rep(1, nrow(data))]
    data <- st_sf(
      data
    ) |> dplyr::select(-any_of(c("location_long", "location_lat")))
  } else {
    data <- data |>
      st_as_sf(
        coords = c("location_long", "location_lat"),
        crs = st_crs(4326L), na.fail = FALSE,
        sf_column_name = "geometry"
      )
  }
  track_data <- movebank_download_deployment(study_id = study_id, ...) |>
    filter(.data$deployment_id %in% unique(data$deployment_id))
  dots <- rlang::list2(...)
  dots[["rename_columns"]] <- TRUE
  study_info <- rlang::exec("movebank_download_study_info",
    id = study_id, !!!dots
  ) |>
    select(-any_of(c("sensor_type_ids", "study_id"))) |>
    #  rename(study_id = all_of("id")) |>
    # FIX maybe remove where then dependency on tidyselect can disappear
    select(
      where(
        ~ !all(is.na(.x))
      )
    )
  if (nrow(study_info) != 1) {
    # This error should not occur, just a check to be sure
    cli_abort("Unexpected event, more then one row of study info",
      class = "move2_error_movebank_api_multiple_rows_of_study_info"
    ) # nocov
  }
  track_id_column <- "deployment_id"
  if ("individual_local_identifier" %in% colnames(data) &&
    (data |>
      group_by(!!!syms("individual_local_identifier")) |>
      st_drop_geometry() |>
      summarise(cnt = length(unique(!!!syms("deployment_id"))) == 1) |>
      pull("cnt") |> all()) &&
    !any(data |> st_drop_geometry() |> filter(!is.na(!!!syms("deployment_id"))) |>
      pull("individual_local_identifier") |> is.na())
  # if individuals id's are not filled out we fall back to local ids, this can atleast happen when individuals have no
  # name in movebank and there is only one individual
  ) {
    # there is only one deployment per individual
    if (anyDuplicated(track_data$individual_local_identifier)) {
      cli_abort("While there is only one deployment per individual in the event data, there are multiple deployments
                  per individual in the track level data",
        class = "move2_error_movebank_api_individuals_duplicated_in_track_data"
      )
    }
    track_id_column <- "individual_local_identifier"
  }
  res <- new_move(data, track_id_column = track_id_column) |>
    mt_set_track_data(bind_cols(track_data, study_info))
  if (remove_movebank_outliers) {
    res <- res |> filter(mt_movebank_visible(res))
  }
  duplicated_columns <- setdiff(
    intersect(
      names(res),
      names(mt_track_data(res))
    ),
    c(mt_track_id_column(res), mt_time_column(res))
  )
  if ((res |> tibble::as_tibble() |>
    select(any_of(c(duplicated_columns, mt_track_id_column(res)))) |>
    group_by(across(any_of(mt_track_id_column(res)))) |>
    dplyr::summarise(across(everything(), n_distinct)) |>
    select(any_of(duplicated_columns)) == 1) |> all()) {
    res <- res |> select(-any_of(duplicated_columns))
  }
  if ("individual_taxon_canonical_name" %in% names(res) &&
    "taxon_canonical_name" %in% names(mt_track_data(res))) {
    res <- res |> select(-all_of("individual_taxon_canonical_name"))
  }
  return(res)
}

#' @rdname movebank_download_study
#' @export
movebank_download_study_info <- function(...) {
  movebank_retrieve(entity_type = "study", ...)
}
#' @rdname movebank_download_study
#' @export
movebank_download_deployment <- function(study_id, ...) {
  dots <- rlang::list2(...)
  study_id <- movebank_get_study_id(study_id, ...)

  dots[["progress"]] <- FALSE
  dots[["rename_columns"]] <- TRUE

  deployment_data <- rlang::exec("movebank_retrieve",
    entity_type = "deployment", study_id = study_id,
    !!!dots
  )
  individual_data <- rlang::exec("movebank_retrieve",
    entity_type = "individual", study_id = study_id,
    !!!dots
  ) |> select(-any_of("sensor_type_ids"))
  tag_data <- rlang::exec("movebank_retrieve",
    entity_type = "tag", study_id = study_id, !!!dots
  ) |>
    select(-any_of("sensor_type_ids")) # prevent duplicated sensor type id info
  deployment_table <- rlang::exec("movebank_retrieve",
    entity_type = "deployment",
    study_id = study_id,
    attributes = c("id", "tag_id", "individual_id"),
    !!!dots
  ) # |>    rename(deployment_id = all_of("id"))
  res <- deployment_table |>
    left_join(deployment_data, "deployment_id") |>
    left_join(individual_data, "individual_id", suffix = c("", "_individual")) |>
    left_join(tag_data, "tag_id", suffix = c("", "_tag")) |>
    mutate(study_id = as.integer64(study_id)) |>
    select_if(~ !all(is.na(.)))
  res
}

#' @rdname movebank_download_study
#' @param rename_columns if `TRUE` column names of properties that are repeated in the api output (e.g. `id`,
#' `local_identifier` and `comments`) will be appended with the `entity_type` (e.g. "tag", "individual"). Argument applicable to `movebank_download_study`, `movebank_download_study_info`, `movebank_retrieve`.
#' @param entity_type the entity type of the data requested from movebank (e.g. `"study"`, `"tag"`, `"event"`).
#' Alternatively it can be the complete api url for testing purposes. Argument applicable to `movebank_retrieve`.
#' @param omit_derived_data derived data (e.g. `timestamp_start`, `timestamp_end`, `number_of_events`
#' and `number_of_deployments`) is omitted from the result. The default is `TRUE` as this data quickly becomes
#' unrepresentative if the results are processed. However in some occasions it might be worth retrieving it,
#' for example if you want to identify deployment periods without downloading all data. Argument applicable to `movebank_download_study` and `movebank_retrieve`.
#' @param progress if `TRUE` a progress bar will be displayed. More details can be found here \code{\link[vroom]{vroom}}. Argument applicable to `movebank_download_study` and `movebank_retrieve`.
#' @export

movebank_retrieve <- function(entity_type = NA, ..., handle = movebank_handle(), # nolint cyclo complexity to reduce
                              rename_columns = FALSE, omit_derived_data = TRUE, convert_spatial_columns = TRUE,
                              progress = vroom::vroom_progress()) {
  assert_that(is.flag(rename_columns), !is.na(rename_columns))
  assert_that(is.flag(omit_derived_data), !is.na(omit_derived_data))
  assert_that(is.flag(convert_spatial_columns), !is.na(convert_spatial_columns))
  check_installed(c("curl"), "for retrieving information from movebank.")
  if (is_scalar_character(entity_type) && grepl("^http", entity_type)) {
    url <- entity_type
    assert_that(
      isFALSE(rename_columns),
      "When `entity_type` is an url the columns cant be renamed, so `rename_columns` should be false."
    )
  } else {
    url <- movebank_construct_url(entity_type = entity_type, ...)
  }
  con <- curl::curl(url, handle = handle)
  # We define too many columns and remove the warning about that
  issue <- catch_cnd(suppressWarnings(
    classes = c("vroom_mismatched_column_name", "vroom_parse_issue"),
    data_dl <- vroom::vroom(con, col_types = mb_column_types_underscore, delim = ",", progress = progress, trim_ws = F)
  ))
  # use vroom and not readr as the later misses col_big_integer, readr cannot be silenced and reads from connections
  # Parse any issues that might have occurred
  if (!is.null(issue)) {
    check_installed(c("xml2"), "for investigating error messages from movebank")
    close(con)
    error_return <- curl::curl_fetch_memory(url, handle = handle)
    error_message_content <- gsub(
      "\n+\n", "\n",
      xml2::xml_text(xml2::read_html(gsub(
        "<br>", "\n",
        rawToChar(error_return$content)
      )))
    )
    if (error_return$status_code == 401) {
      cli_abort(
        c(
          "No valid movebank credentials provided (status code 401)",
          "Please ensure your movebank credentials are valid"
        ),
        class = "move2_error_movebank_api_401_no_valid_credentials", url = url
      )
    } else if (error_return$status_code == 500) {
      cli_abort(
        class = "move2_error_movebank_api_500_internal_server_error",
        c(
          "There has been an internal movebank server error",
          paste(
            "The following exception was communicated:",
            sub(pattern = ".*Exception: ", "", strsplit(error_message_content, "\n")[[1]][1])
          ),
          "This error can occur when non-existing attributes are requested as part of the {.arg attributes} argument,
          but also other movebank internal error can give the same result",
          "The full error message can be printed with {.code message(rlang::last_error()$error)}",
          "The requested url can be retrieved with: {.code rlang::last_error()$url}"
        ),
        error = error_message_content,
        url = url
      )
    } else if (error_return$status_code == 403) {
      rlang::abort(
        sub(
          "Apache.*", "",
          sub(
            ".*Description ",
            "", error_message_content
          )
        ),
        body = format_error_bullets(c(i = sub(
          "Description.*",
          "", sub(
            ".*Message ", "",
            error_message_content
          )
        ))), url = url,
        class = "move2_error_movebank_api_403_no_access"
      )
    } else {
      cli_abort(
        c(
          "There is an unknown download error",
          "The status message was: {error_return$status_code}",
          "The full error message can be printed with {.code message(rlang::last_error()$error)}",
          "The requested url can be retrieved with: {.code rlang::last_error()$url}"
        ),
        error = error_message_content,
        class = "move2_error_movebank_api_general_error", url = url
      )
    }
  }
  tmp <- suppressWarnings(data_dl[1, 1, drop = TRUE], classes = c("vroom_parse_issue"))
  if (is_scalar_character(tmp)) {
    if (grepl("^<p>By accepting this document the user agrees to the following:</p>", tmp)) {
      check_installed(c("digest", "xml2"), "to create a hash for accepting licence terms.")
      error_return <- curl::curl_fetch_memory(url, handle = handle)
      error_message_content <- gsub(
        "\n+\n",
        "\n",
        xml2::xml_text(xml2::read_html(gsub(
          "<br>",
          "\n",
          rawToChar(error_return$content)
        )))
      )

      cli_abort("An attempt is made to download a study without having accepted the study specific license terms",
        body = format_error_bullets(c(i = sprintf(
          "%s\nThis can be resolved by accepting the license terms. In R is done by adding the following to a
          download request: 'license-md5'='%s'. Alternatively the study can be once downloaded from movebank directly.",
          error_message_content,
          digest::digest( # sub("\n$","",
            rawToChar(error_return$content),
            serialize = FALSE
          )
        ))), url = url,
        class = "move2_error_movebank_api_license_not_accepted"
      )
    }
  }
  p <- suppressWarnings(vroom::problems(data_dl), classes = c("vroom_parse_issue"))
  if (nrow(p) != 0) {
    cli_warn(c("{.fun vroom} finds reading problems with the movebank specification.",
      i = "This might relate to the returned data not fitting the expectation of the movebank data format specified in
      the package.",
      i = "For retrieving the specific problem you can enable `global_entrace` using {.run rlang::global_entrace()}
      then run the command and use {.code rlang::last_warnings()[[1]]$problems} to retrieve the problems.",
      i = "The requested url can then be retrieved with: {.code rlang::last_warnings()[[1]]$url}",
      i = "Alternatively in some cases you might be able to retrieve the problems calling {.fun vroom::problems} on the
      result of the function call that produced the warning."
    ), problems = p, url = url, class = "move2_warning_movebank_api_format_not_parsed")
  }
  if (rename_columns) {
    data_dl <- data_dl |>
      rename_with(
        .cols = any_of(c("id", "local_identifier", "comments", "number_of_deployments")),
        .fn = function(x, y) paste(x, y, sep = "_"), x = entity_type
      )
  }
  if (omit_derived_data) {
    data_dl <- data_dl |> select(-any_of(c(
      "timestamp_end", "timestamp_start",
      "number_of_events", "number_of_deployments"
    )))
  }
  for (i in names(mb_column_units_underscore)[names(mb_column_units_underscore) %in% names(data_dl)]) {
    data_dl[[i]] <- structure(data_dl[[i]],
      class = "units",
      units = attr(as_units(mb_column_units_underscore[i]), "units")
    )
    # units are always doubles see: https://github.com/r-quantities/units/issues/324
    storage.mode(data_dl[[i]]) <- "double"
  }
  if (convert_spatial_columns) {
    data_dl <- movebank_convert_spatial_cols(data_dl)
  }
  return(data_dl)
}

#' @export
#' @rdname movebank_download_study
movebank_get_study_id <- function(study_id, ...) {
  if (is_scalar_character(study_id)) {
    dots <- rlang::list2(...)
    if ("handle" %in% names(dots)) {
      study_id_data <- movebank_download_study_info(
        handle = dots[["handle"]],
        attributes = c("name", "id"),
        rename_columns = TRUE
      )
    } else {
      study_id_data <- movebank_download_study_info(
        attributes = c("name", "id"),
        rename_columns = TRUE
      )
    }
    study_id <- study_id_data |>
      rename(id = "study_id") |>
      dplyr::filter(grepl(pattern = study_id, .data$name, fixed = TRUE)) |>
      pull("id")
    if (!is_scalar_vector(study_id)) {
      if (length(study_id) < 1) {
        cli_abort(c(
          e = "The argument {.arg study_id} currently does not match any study.",
          i = "Check if a correct name is provided."
        ), class = "move2_error_movebank_api_no_study_name_match")
      }
      cli_abort(c(
        e = "The argument {.arg study_id} matches more then one study.",
        i = "{.arg study_id} should only match one study. The currently matched studies are: {.arg {study_id_data$name[study_id_data$study_id %in% study_id]}}"
      ), class = "move2_error_movebank_api_name_matches_multiple_studies")
    }
  }

  if (!(is_scalar_integerish(study_id) || (inherits(study_id, "integer64") && is_scalar_vector(study_id)))) {
    cli_abort(
      class = "move2_error_no_valid_study_id",
      c(
        e = "The {.arg study_id} is not an whole number (e.g. {.cls integer} or {.cls integer64}), therefore
        it can not be interpreted as a {.code study_id}. Nor is it a scalar character that can be converted to a {.code study_id} by searching for the name of
        the study.",
        i = 'This error can occur because some R functions (e.g. {.fun lapply} and {.fun for}) do not retain the
        {.cls integer64} class. This can be resolved by adding the {.cls integer64} again (e.g.
        {.code class(id)<-"integer64"}). '
      )
    )
  }

  return(study_id)
}
