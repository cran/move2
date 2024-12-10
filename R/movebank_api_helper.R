mb_include_minimal_attributes <- function(attributes, to_add=NULL) {
  mb_study_minimal_attributes <- c("deployment_id", "location_long", "location_lat", "timestamp", to_add)
  if (is.null(attributes)) {
    attributes <- mb_study_minimal_attributes
  } else {
    assert_that(is.character(attributes))
    if (attributes[1L] != "all") {
      attributes <- unique(c(
        attributes,
        mb_study_minimal_attributes
      ))
    }
  }
  return(attributes)
}
movebank_convert_spatial_cols <- function(df) {
  for (i in names(movebank_spatial_column_pairs)) {
    if (all(movebank_spatial_column_pairs[[i]] %in% colnames(df))) {
      if (nrow(df) == 0L) {
        df[[i]] <- sf::st_sfc(crs = 4326L)
      } else {
        df[[i]] <- sf::st_sfc(
          apply(st_drop_geometry(df)[, movebank_spatial_column_pairs[[i]]], 1L,
            sf::st_point,
            simplify = FALSE
          ),
          crs = 4326L
        )
      }
      df <- df |> select(-all_of(movebank_spatial_column_pairs[[i]]))
    }
  }
  df
}
movebank_construct_url <- function(entity_type = NA, ...) {
  assert_that(is.string(entity_type))
  assert_that(entity_type %in% movebank_valid_entity_types,
    msg = format_error("The value provided for {.arg entity_type} ({.val {entity_type}}) is not in the list of valid
                       entity types: {.val {movebank_valid_entity_types}}")
  )
  extra_args <- list(...)
  assert_that(!is.null(names(extra_args)) | length(extra_args) == 0L,
    msg = "All arguments must be named to construct a valid url for a movebank request"
  )
  assert_that(all(names(extra_args) != ""),
    msg = "All arguments must be named to construct a valid url for a movebank request"
  )
  if ("sensor_type_id" %in% names(extra_args) && is.character(extra_args[["sensor_type_id"]])) {
    if (!all(extra_args[["sensor_type_id"]] %in% (movebank_tag_type_table |> pull("external_id")))) {
      cli_abort("The character string for sensor type id is not a valid movebank sensor type",
        class = "move2_error_movebank_api_not_valid_sensor_type_id"
      )
    }
    extra_args[["sensor_type_id"]] <- movebank_tag_type_table$id[
      movebank_tag_type_table$external_id %in% extra_args[["sensor_type_id"]]
    ]
  }
  for (i in c("timestamp_start", "timestamp_end")) {
    if (i %in% names(extra_args) &&
      (inherits(extra_args[[i]], "POSIXct") || inherits(extra_args[[i]], "Date"))) {
      if (inherits(extra_args[[i]], "Date")) {
        extra_args[[i]] <- as.POSIXct(extra_args[[i]])
      }
      extra_args[[i]] <- sub(
        "\\.", "",
        strftime(extra_args[[i]],
          format = "%Y%m%d%H%M%OS3", tz = "UTC"
        )
      )
    }
  }

  assert_that(
    all(unlist(
      lapply(
        extra_args,
        function(x) is.numeric(x) | is.character(x) | is.logical(x)
      )
    )),
    msg = format_error("Additional arguments to should be either a {.cls character}, {.cls logical} or {.cls numeric}")
  )
  extra_args <- lapply(extra_args, function(x) {
    if (is.logical(x)) {
      return(as.character(x))
    } else {
      return(x)
    }
  })
  extra_args <- lapply(extra_args, function(x) {
    if (is.character(x)) {
      URLencode(x, reserved = TRUE)
    } else {
      x
    }
  })
  extra_args <- paste0(names(extra_args), "=", unlist(lapply(extra_args, paste, collapse = ",")))
  api_url <- getOption("move2_movebank_api_url")
  assert_that(is_scalar_character(api_url),
    msg = format_error("The option {.var move2_movebank_api_url} should contain a scalar {.cls character}")
  )
  url <- sprintf(
    "%s?entity_type=%s%s",
    api_url,
    entity_type,
    sub("&=", "", paste0("&", extra_args, collapse = ""))
  )
  assert_that(is_scalar_character(url))
  # there is still a region where an other internal error occurs:
  # https://github.com/movebank/movebank-api-doc/issues/8
  # https://github.com/movebank/movebank-api-doc/issues/8#issuecomment-1355109348
  url_length <- nchar(url)
  if (url_length >= 8202L) {
    cli_abort("The constructed url for the request to the movebank API gets too long ({url_length} characters).
              It should be less then 8202 characters. This can be caused by requesting for example a long vector of
              identifiers.", url = url)
  }
  return(url)
}
