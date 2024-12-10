## ----include = FALSE--------------------------------------------------------------
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

## ----setup------------------------------------------------------------------------
library(move2)

## ----eval=F-----------------------------------------------------------------------
#  movebank_store_credentials("myUserName", "myPassword")

## ----echo=F-----------------------------------------------------------------------
movebank_store_credentials("asf", "adsf", force = TRUE)

## ---------------------------------------------------------------------------------
movebank_remove_credentials()

## ----eval=F-----------------------------------------------------------------------
#  ## store credentials for the most used account.
#  movebank_store_credentials("myUserName", "myPassword")
#  
#  ## store credentials for another movebank account
#  movebank_store_credentials("myUserName_2", "myPassword_2", key_name = "myOtherAccount")

## ----credentials, eval=TRUE, echo=FALSE-------------------------------------------
movebank_store_credentials("myUserName", "myPassword", force = TRUE)
movebank_store_credentials("myUserName_2", "myPassword_2",
  key_name = "myOtherAccount", force = TRUE
)

## ----option_setting---------------------------------------------------------------
options("move2_movebank_key_name" = "myOtherAccount")

## ----options_setting_2------------------------------------------------------------
options("move2_movebank_key_name" = "movebank")

## ----eval=FALSE-------------------------------------------------------------------
#  keyring::key_list()
#  #   service           username
#  # 1 movebank          myUserName
#  # 2 myOtherAccount    myUserName_2

## ----remove_credentials-----------------------------------------------------------
## for the default account
movebank_remove_credentials()

## for an account with a key name
movebank_remove_credentials(key_name = "myOtherAccount")

## ----eval=FALSE-------------------------------------------------------------------
#  keyring::key_list()

## ----echo=FALSE-------------------------------------------------------------------
options("keyring_backend" = krba)
if (Sys.info()["user"] != "bart") {
  if (Sys.getenv("MBPWD") != "") {
    options(keyring_backend = "env")
    move2::movebank_store_credentials("move2_user", Sys.getenv("MBPWD"))
  } else {
    knitr::opts_chunk$set(eval = FALSE)
  }
}

## ----message=FALSE----------------------------------------------------------------
library(dplyr)

## ----study_info, time_it=TRUE-----------------------------------------------------
movebank_download_study_info()

## ----study_info_2, eval=F---------------------------------------------------------
#  movebank_download_study_info(i_have_download_access = TRUE)

## ----study_info_3, eval=F---------------------------------------------------------
#  movebank_download_study_info(i_am_owner = TRUE)

## ----study_info_4, eval=F---------------------------------------------------------
#  movebank_download_study_info(license_type = "CC_0")

## ----study_info_5, eval=F---------------------------------------------------------
#  movebank_download_study_info(id = 2911040)

## ----galapagos_deployment, time_it=T----------------------------------------------
movebank_download_deployment("Galapagos Albatrosses")

## ----get_studyid, time_it=T-------------------------------------------------------
movebank_get_study_id(study_id = "Galapagos Albatrosses")

## ----download_allsensors, time_it=T-----------------------------------------------
movebank_download_study_info(study_id = 2911040)$sensor_type_ids
movebank_download_study(
  study_id = 2911040,
  sensor_type_id = c("gps", "acceleration")
)

## ----download_oneindv, time_it=T--------------------------------------------------
movebank_download_study(
  study_id = "Galapagos Albatrosses",
  sensor_type_id = "gps",
  individual_local_identifier = "unbanded-160"
)

## ----download_multiindv, time_it=T------------------------------------------------
movebank_download_study(
  study_id = 2911040,
  sensor_type_id = "gps",
  individual_local_identifier = c("1094-1094", "1103-1103")
)

## ----download_multiindv_2, eval=F-------------------------------------------------
#  ## it is also possible to use the numerical identifiers
#  movebank_download_study(
#    study_id = 2911040,
#    sensor_type_id = "gps",
#    individual_id = c(2911086, 2911065)
#  )

## ----download_acc, time_it=T------------------------------------------------------
movebank_download_study(2911040,
  sensor_type_id = "acceleration",
  individual_local_identifier = "1094-1094"
)

## ----retrieve_sensors-------------------------------------------------------------
movebank_retrieve(entity_type = "tag_type")

## ----download_time_win, time_it=T-------------------------------------------------
movebank_download_study(2911040,
  sensor_type_id = "gps",
  timestamp_start = as.POSIXct("2008-08-01 00:00:00"),
  timestamp_end = as.POSIXct("2008-08-02 00:00:00")
)

## ----quick_download, time_it=TRUE-------------------------------------------------
movebank_download_study(1259686571, sensor_type_id = 653, attributes = NULL)

## ----study_attrs, time_it=T-------------------------------------------------------
## get all attributes available for a specific study and sensor
movebank_retrieve(
  entity_type = "study_attribute",
  study_id = 2911040,
  sensor_type_id = "gps"
)$short_name

movebank_download_study(
  study_id = 2911040,
  sensor_type_id = "gps",
  attributes = c("height_above_ellipsoid", "eobs_temperature")
)

## ----advance, time_it=TRUE--------------------------------------------------------
movebank_retrieve("event",
  study_id = 1259686571,
  tag_local_identifier = "193967",
  attributes = "all"
) %>%
  filter(is.na(deployment_id))

