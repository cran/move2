## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

knitr::knit_hooks$set(time_it = local({
  now <- NULL
  function(before, options) {
    if (before) {
      # record the current time before each chunk
      now <<- Sys.time()
    } else {
      # calculate the time difference after a chunk
      res <- units::as_units(difftime(Sys.time(), now))
      # return a character string to show the time
      paste(
        '<div style="text-align: right; margin-top: -10px;"><font size="-3">This code took:',
        format(res, digits = 3, nsmall = 1), "</font></div>"
      )
    }
  }
}))
krba <- getOption("keyring_backend")
options("keyring_backend" = "env")

## ----setup--------------------------------------------------------------------
library(move2)

## ---- eval=F------------------------------------------------------------------
#  movebank_store_credentials("myUserName", "myPassword")

## ---- echo=F------------------------------------------------------------------
movebank_store_credentials("asf", "adsf", force = TRUE)

## -----------------------------------------------------------------------------
movebank_remove_credentials()

## ---- eval=F------------------------------------------------------------------
#  ## store credentials for the most used account.
#  movebank_store_credentials("myUserName", "myPassword")
#  
#  ## store credentials for another movebank account
#  movebank_store_credentials("myUserName_2", "myPassword_2", key_name = "myOtherAccount")

## ----credentials, eval=TRUE, echo=FALSE---------------------------------------
movebank_store_credentials("myUserName", "myPassword", force = TRUE)
movebank_store_credentials("myUserName_2", "myPassword_2",
  key_name = "myOtherAccount", force = TRUE
)

## ----option_setting-----------------------------------------------------------
options("move2_movebank_key_name" = "myOtherAccount")

## ----options_setting_2--------------------------------------------------------
options("move2_movebank_key_name" = "movebank")

## ---- eval=FALSE--------------------------------------------------------------
#  keyring::key_list()
#  #   service           username
#  # 1 movebank          myUserName
#  # 2 myOtherAccount    myUserName_2

## ----remove_credentials-------------------------------------------------------
## for the default account
movebank_remove_credentials()

## for an account with a key name
movebank_remove_credentials(key_name = "myOtherAccount")

## ---- eval=FALSE--------------------------------------------------------------
#  keyring::key_list()

## ---- echo=FALSE--------------------------------------------------------------
options("keyring_backend" = krba)
if (Sys.info()["user"] != "bart") {
  if (Sys.getenv("MBPWD") != "") {
    options(keyring_backend = "env")
    move2::movebank_store_credentials("move2_user", Sys.getenv("MBPWD"))
  } else {
    knitr::opts_chunk$set(eval = FALSE)
  }
}

## -----------------------------------------------------------------------------
library(dplyr, quietly = TRUE)

## ----cc_studies, time_it=TRUE-------------------------------------------------
movebank_retrieve(entity_type = "study", license_type = "CC_0") |>
  select(id, name, number_of_deployed_locations) |>
  filter(!is.na(number_of_deployed_locations))

## ----studies, eval=FALSE------------------------------------------------------
#  movebank_download_study_info(license_type = "CC_0")

## ----full_dl, time_it=TRUE----------------------------------------------------
movebank_download_study(2911040, sensor_type_id = "gps")

## ----quick_download, time_it=TRUE---------------------------------------------
movebank_download_study(1259686571, sensor_type_id = "gps", attributes = NULL)

## ----study_attrs, time_it=T---------------------------------------------------
movebank_retrieve(
  entity_type = "study_attribute",
  study_id = 2911040,
  sensor_type_id = "gps"
)$short_name
movebank_download_study(
  study_id = 2911040,
  sensor_type_id = "gps",
  attributes = c(
    "height_above_ellipsoid",
    "eobs_temperature"
  )
)

## ----gps_sensor, time_it=TRUE-------------------------------------------------
movebank_download_study(1259686571, sensor_type_id = 653)

## ----acc_study_download, time_it=TRUE-----------------------------------------
movebank_download_study(2911040, sensor_type_id = "acceleration")

## ----retrieve_sensors---------------------------------------------------------
movebank_retrieve(
  entity_type = "tag_type",
  attributes = c("external_id", "id")
)

## ----download_lbbg, time_it=TRUE----------------------------------------------
movebank_download_study("LBBG_JUVENILE",
  sensor_type_id = "gps",
  timestamp_start = as.POSIXct("2021-02-03 00:00:00"),
  timestamp_end = as.POSIXct("2021-03-03 00:00:00")
)

## ----galapagos_deployment, time_it=T------------------------------------------
movebank_download_deployment("Galapagos Albatrosses")

## ----advance, time_it=TRUE----------------------------------------------------
movebank_retrieve("event",
  study_id = 1259686571,
  tag_local_identifier = "193967", attributes = "all"
) %>%
  filter(is.na(deployment_id))

