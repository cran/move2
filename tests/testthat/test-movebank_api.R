test_that("to_download_names", {
  expect_identical(to_download_names("tag-firmware"), "tag_firmware")
  expect_identical(to_download_names("argos:lc"), "argos_lc")
  expect_identical(to_download_names("bar:barometric-height"), "barometric_height")
})
test_that("User on save needs to be specified", {
  expect_error(
    movebank_store_credentials(),
    "Please call the `movebank_store_credentials..` function with the `username` argument specified"
  )
  expect_error(
    movebank_store_credentials(password = "sadf"),
    "Please call the `movebank_store_credentials..` function with the `username` argument specified"
  )
})
test_that("User pwd combination works", {
  expect_error(
    movebank_store_credentials("move2_user", "sdf"),
    "The provided `username` and `password` combination fail."
  )
})
test_that("input movebank_retrieve checks", {
  expect_error(movebank_retrieve(omit_derived_data = NA), ".is.na.omit_derived_data. is not TRUE")
  expect_error(
    movebank_retrieve(omit_derived_data = 1),
    "omit_derived_data is not a flag \\(a length one logical vector\\)"
  )
  expect_error(
    movebank_retrieve(omit_derived_data = c(TRUE, FALSE)),
    "omit_derived_data is not a flag \\(a length one logical vector\\)"
  )
  expect_error(movebank_retrieve(rename_columns = NA), ".is.na.rename_columns. is not TRUE")
  expect_error(movebank_retrieve(rename_columns = 1), "rename_columns is not a flag \\(a length one logical vector\\)")
  expect_error(
    movebank_retrieve(rename_columns = c(TRUE, FALSE)),
    "rename_columns is not a flag \\(a length one logical vector\\)"
  )
  expect_error(movebank_retrieve(entity_type = "asdf"), "is not in the list of valid entity types")
  expect_error(movebank_retrieve(entity_type = NA_character_), "is not in the list of valid entity types")
  expect_error(
    movebank_retrieve(entity_type = LETTERS),
    "entity_type is not a string \\(a length one character vector\\)."
  )
  expect_error(movebank_retrieve(entity_type = 1), "entity_type is not a string \\(a length one character vector\\).")
  expect_error(
    movebank_retrieve(entity_type = c("tag", "event")),
    "entity_type is not a string \\(a length one character vector\\)."
  )
})
test_that("Remove user", {
  withr::with_options(c(keyring_backend = "env"), {
    expect_warning(movebank_remove_credentials(), "There are no keys to remove from the keyring.")
    movebank_store_credentials("move2_user", "sdf", force = TRUE)
    movebank_store_credentials("move2_user2", "sdf2", force = TRUE)
    expect_error(movebank_handle(), 'There are 2 keys found with the service name "movebank"')
    expect_message(movebank_remove_credentials(), "There are 2 keys removed from the keyring.")
  })
})
test_that("Test url construction", {
  expect_identical(
    movebank_construct_url("study", i_am_owner = TRUE),
    "https://www.movebank.org/movebank/service/direct-read?entity_type=study&i_am_owner=TRUE"
  )
  expect_error(
    movebank_construct_url("event", individual_id = 1:10000),
    "The constructed url for the request to the movebank API gets too long .48979 characters.. It should be less then 8202 characters."
  )
  expect_identical(
    movebank_construct_url("event", timestamp_start = as.POSIXct("2018-1-1 01:13:23", tz = "UTC")),
    "https://www.movebank.org/movebank/service/direct-read?entity_type=event&timestamp_start=20180101011323000"
  )
  expect_identical(
    movebank_construct_url("event", timestamp_end = as.POSIXct("2018-1-1 01:13:23", tz = "UTC")),
    "https://www.movebank.org/movebank/service/direct-read?entity_type=event&timestamp_end=20180101011323000"
  )
  expect_identical(
    movebank_construct_url("event",
      timestamp_start = as.POSIXct("2018-1-1 00:13:23", tz = "UTC"),
      timestamp_end = as.POSIXct("2018-1-1 01:13:23", tz = "UTC")
    ),
    paste0(
      "https://www.movebank.org/movebank/service/direct-read?entity_type=event&",
      "timestamp_start=20180101001323000&timestamp_end=20180101011323000"
    )
  )
  expect_identical(
    movebank_construct_url("event",
      timestamp_start = as.POSIXct("2018-1-1 01:13:23", tz = "UTC-1"),
      timestamp_end = as.POSIXct("2018-1-1 03:13:23", tz = "UTC-2")
    ),
    paste0(
      "https://www.movebank.org/movebank/service/direct-read?entity_type=event&",
      "timestamp_start=20180101001323000&timestamp_end=20180101011323000"
    )
  )
  expect_identical(
    movebank_construct_url("event",
      timestamp_start = as.POSIXct("2018-1-1 00:13:23", tz = "UTC"),
      timestamp_end = as.POSIXct("2018-1-1 01:13:23", tz = "UTC")
    ),
    paste0(
      "https://www.movebank.org/movebank/service/direct-read?entity_type=event&",
      "timestamp_start=20180101001323000&timestamp_end=20180101011323000"
    )
  )


  expect_error(movebank_download_study(2911040,
    `individual_id` = c(2911086, 2911065),
    sensor_type_id = "accelerometer"
  ), class = "move2_error_movebank_api_not_valid_sensor_type_id")
  expect_identical(
    movebank_construct_url("event",
      timestamp_start = as.POSIXct("2018-1-1 00:00:00", tz = "UTC"),
      timestamp_end = as.POSIXct("2018-1-2 00:00:00", tz = "UTC")
    ),
    movebank_construct_url("event",
      timestamp_start = as.Date("2018-1-1"),
      timestamp_end = as.Date("2018-1-2")
    )
  )
  expect_identical(
    movebank_construct_url("event", sensor_type_id = "gps"),
    "https://www.movebank.org/movebank/service/direct-read?entity_type=event&sensor_type_id=653"
  )
  expect_identical(
    movebank_construct_url("event", sensor_type_id = c("gps", "acceleration")),
    "https://www.movebank.org/movebank/service/direct-read?entity_type=event&sensor_type_id=653,2365683"
  )
  expect_error(
    movebank_construct_url("event", sensor_type_id = c("gps", "acceleration", "asdf")),
    "The character string for sensor type id is not a valid movebank sensor type"
  )
  expect_identical(
    movebank_construct_url("event",
      study_id = 22,
      sensor_type_id = "gps"
    ),
    paste0(
      "https://www.movebank.org/movebank/service/direct-read?entity_type=event&",
      "study_id=22&sensor_type_id=",
      movebank_tag_type_table$id[movebank_tag_type_table$external_id == "gps"]
    )
  )
  expect_error(movebank_construct_url("event",
    study_id = 22,
    sensor_type_id = "accelerometer"
  ), class = "move2_error_movebank_api_not_valid_sensor_type_id")
})
test_that("no login", {
  withr::with_options(c(keyring_backend = "env"), {
    expect_error(movebank_handle(), "No login for movebank found")
  })
})
test_that("Download api license acceptance message", {
  skip_if_offline()
  skip_on_cran()
  skip_if_no_mbpwd()
  withr::with_envvar(
    list("movebank:move2_user" = Sys.getenv("MBPWD")),
    withr::with_options(list(keyring_backend = "env"), {
      expect_identical(movebank_get_study_id("Martes pennanti LaPoint") |> as.integer(), 6925808L)
      expect_error(movebank_get_study_id("LBBG"), "The argument `study_id` matches more then one study")
      expect_error(movebank_get_study_id("LBsfwerwxczvsdfBG"), "The argument `study_id` currently does not match any study")
      expect_s3_class(a <- movebank_download_study_info(attributes = c("go_public_date", "id")), "tbl")
      expect_identical(a |> pull("go_public_date") |> lubridate::tz(), "UTC")
      expect_error(
        movebank_download_study(1245488040),
        "An attempt is made to download a study without having accepted the study specific license terms"
      )
      expect_no_warning(
        m <- movebank_download_study(2911040,
          `individual_id` = c(2911086, 2911065),
          sensor_type_id = "acceleration"
        )
      )
      expect_s3_class(m, "tbl")
      expect_true(has_name(m, "geometry"))
      expect_false(has_name(m, "location_long"))
      expect_false(has_name(m, "location_lat"))
    })
  )
})

test_that("movebank_download_study_info", {
  skip_if_offline()
  skip_on_cran()
  skip_if_no_mbpwd()
  withr::with_envvar(
    list("movebank:move2_user" = Sys.getenv("MBPWD")),
    withr::with_options(list(keyring_backend = "env"), {
      expect_identical(nrow(a <- movebank_download_study_info(study_id = 2911040)), 1L)
      expect_identical(movebank_download_study_info(study_id = "Galapagos Alba"), a)
    })
  )
})
test_that("Test if workaround for NA coordinates can be omitted (#63)", {
  show_failure(expect_warning(expect_warning(
    expect_warning(expect_warning(st_as_sf(
      data.frame(location_long = NA_real_, location_lat = NA_real_, a = letters),
      coords = c("location_long", "location_lat"),
      crs = st_crs(4326L), na.fail = FALSE,
      sf_column_name = "geometry"
    )))
  )))
})
test_that("Download deployment", {
  skip_if_offline()
  skip_on_cran()
  skip_if_no_mbpwd()
  withr::with_envvar(
    list("movebank:move2_user" = Sys.getenv("MBPWD")),
    withr::with_options(list(keyring_backend = "env"), {
      data <- movebank_download_deployment(2911040,
        `individual_id` = c(2911086, 2911065)
      )
      expect_identical(data$individual_id, structure(c(1.43825720931085e-317, 1.43826758468941e-317), class = "integer64"))
      expect_true("deploy_on_location" %in% colnames(data))
      expect_false("deploy_longitude" %in% colnames(data))
      expect_false("deploy_latitude" %in% colnames(data))
      expect_s3_class(data$deploy_on_location, "sfc")
      data <- movebank_download_deployment(2911040,
        `individual_id` = c(2911086, 2911065), convert_spatial_columns = FALSE
      )
      expect_true("deploy_on_longitude" %in% colnames(data))
      expect_true("deploy_on_latitude" %in% colnames(data))
      expect_false("deploy_on_location" %in% colnames(data))
      show_failure(expect_error(
        movebank_download_study(604906833),
        paste0(
          "by trying to retrieve a none existant ",
          "individual. Or no data is available for download in"
        )
      )) # allow for failure as study might change properties
    })
  )
})


test_that("retrieve with url", {
  skip_if_offline()
  skip_on_cran()
  skip_if_no_mbpwd()
  withr::with_envvar(
    list("movebank:move2_user" = Sys.getenv("MBPWD")),
    withr::with_options(list(keyring_backend = "env"), {
      expect_identical(
        m <- movebank_retrieve("https://www.movebank.org/movebank/service/direct-read?entity_type=individual&study_id=2911040"),
        movebank_retrieve("individual", study_id = 2911040L)
      )
      expect_gt(nrow(m), 5L)
      expect_gt(ncol(m), 5L)
      expect_error(
        movebank_retrieve("https://www.movebank.org/movebank/service/direct-read?entity_type=individual&study_id=2911040",
          rename_columns = TRUE
        ),
        "When `entity_type` is an url the columns can't be renamed"
      )
    })
  )
})


test_that("can download using handle directly", {
  skip_if_offline()
  skip_on_cran()
  skip_if_no_mbpwd()
  expect_s3_class(movebank_download_study(2911040,
    attributes = NULL,
    handle = movebank_handle("move2_user", Sys.getenv("MBPWD"))
  ), "move2")
  expect_gt(movebank_download_study(2911040,
    attributes = NULL, timestamp_start = as.POSIXct("2008-06-6 01:30:24"),
    handle = movebank_handle("move2_user", Sys.getenv("MBPWD"))
  ) |>
    mt_time() |>
    max(), as.POSIXct("2008-06-6 01:30:24"))
  expect_identical(
    suppressMessages(movebank_download_study("artes pennanti LaPoint New York",
      attributes = "eobs_battery_voltage",
      handle = movebank_handle("move2_user", Sys.getenv("MBPWD"))
    )),
    suppressMessages(movebank_download_study(6925808L,
      attributes = "eobs_battery_voltage",
      handle = movebank_handle("move2_user", Sys.getenv("MBPWD")), timestamp_start = as.POSIXct("2007-06-6 01:30:24")
    ))
  )
  # At least temporary suppress warnings due to having only empty coordinates in creation of bounding box
  suppressWarnings(expect_true(movebank_download_study(2911040L,
    sensor_type_id = 2365683L, individual_id = 2911092L,
    handle = movebank_handle("move2_user", Sys.getenv("MBPWD"))
  ) |>
    sf::st_coordinates() |>
    is.na() |>
    all()))
})
test_that("No credentials 401", {
  skip_if_offline()
  skip_on_cran()
  expect_error(
    movebank_download_study_info(handle = curl::new_handle()),
    "No valid movebank credentials provided .status code 401."
  )
})
test_that("server error 500", {
  skip_if_offline()
  skip_on_cran()
  skip_if_no_mbpwd()

  expect_error(
    movebank_retrieve(entity_type = "sensor", handle = movebank_handle("move2_user", Sys.getenv("MBPWD"))),
    "There has been an internal movebank server error"
  )
})
test_that("sensors list uptodate", {
  skip_if_offline()
  skip_on_cran()
  skip_if_no_mbpwd()

  expect_identical(
    movebank_tag_type_table[, "external_id"],
    movebank_retrieve(
      "entity_type" = "tag_type",
      attributes = "external_id",
      handle = movebank_handle(
        "move2_user",
        Sys.getenv("MBPWD")
      )
    )
  )
})

test_that("unknown error", {
  skip_if_offline()
  skip_on_cran()
  skip_if_no_mbpwd()
  withr::with_options(list(move2_movebank_api_url = "https://www.movebank.org/movebank/service/direct-red"), {
    expect_error(
      movebank_download_study(1245488040L, handle = movebank_handle("move2_user", Sys.getenv("MBPWD"))),
      "There is an unknown download error"
    )
    expect_error(
      movebank_download_study(1245488040L, handle = movebank_handle("move2_user", Sys.getenv("MBPWD"))),
      "The status message was: 404"
    )
  })
})
test_that("error on wron specification of study id", {
  expect_error(
    movebank_download_deployment(letters),
    "is not an whole number .e.g. <integer> or <integer64>., therefore it can not be interpreted as"
  )

  expect_error(
    movebank_download_study(letters),
    "is not an whole number .e.g. <integer> or <integer64>., therefore it can not be interpreted as"
  )
})
test_that("Local test of argos gps study", {
  skip_if_offline()
  skip_on_cran()
  skip_if_no_mbpwd()
  skip_if(!keyring::has_keyring_support())
  skip_if(!inherits(keyring::default_backend(), "backend_secret_service"))
  skip_if(!keyring::default_backend()$is_available())
  skip_if(keyring::key_list(
    getOption("move2_movebank_key_name"),
    keyring = getOption("move2_movebank_keyring")
  )$username != "bart")
  expect_message(
    tmp <- movebank_download_study(8849883L),
    "[0-9][0-9] records were omitted as they were not deployed"
  )
  expect_identical(
    tmp$event_id[2L],
    (dta <- movebank_retrieve("event",
      study_id = 8849883L, attributes = "all",
      convert_spatial_columns = FALSE
    ))$event_id[2L]
  )
  expect_equal(
    unlist(dta[2L, c("location_long", "location_lat")]),
    sf::st_coordinates(tmp)[2L, ],
    ignore_attr = TRUE
  )
  expect_equal(
    unlist(dta[70031L, c("location_long", "location_lat")]),
    sf::st_coordinates(tmp)[tmp$event_id == dta$event_id[70031L], ],
    ignore_attr = TRUE
  )
  expect_s3_class(tmp$argos_location_1, "sfc")
  expect_s3_class(tmp$argos_location_2, "sfc")
  expect_identical(st_crs(tmp$argos_location_1), st_crs(4326L))
  expect_false("argos_location_2" %in% colnames(dta))
  expect_false("argos_location_1" %in% colnames(dta))
  expect_false("argos_lat2" %in% colnames(tmp))
  expect_false("argos_lat1" %in% colnames(tmp))
  expect_false("argos_lon2" %in% colnames(tmp))
  expect_false("argos_lon1" %in% colnames(tmp))
  expect_type(dta$argos_lon1, "double")
  expect_type(dta$argos_lon2, "double")
  expect_type(dta$argos_lat1, "double")
  expect_type(dta$argos_lat2, "double")
})
