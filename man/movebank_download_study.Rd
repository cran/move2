% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/movebank_api.R
\name{movebank_download_study}
\alias{movebank_download_study}
\alias{movebank_download_study_info}
\alias{movebank_download_deployment}
\alias{movebank_retrieve}
\alias{movebank_get_study_id}
\title{Download data from movebank}
\usage{
movebank_download_study(
  study_id,
  attributes = "all",
  ...,
  remove_movebank_outliers = TRUE
)

movebank_download_study_info(...)

movebank_download_deployment(study_id, ...)

movebank_retrieve(
  entity_type = NA,
  ...,
  handle = movebank_handle(),
  rename_columns = FALSE,
  omit_derived_data = TRUE,
  convert_spatial_columns = TRUE,
  progress = vroom::vroom_progress()
)

movebank_get_study_id(study_id, ...)
}
\arguments{
\item{study_id}{the study id as a number or a character string can be used to identify a study. This character string
needs to be unique enough to identify one and only one study. Argument applicable to all functions.}

\item{attributes}{a character vector with the event data attributes to download. By default \code{"all"} are downloaded,
this make it slightly slower, to speed up \code{NULL} can be used as it reduces it to the minimal set of required
attributes (only for location data). Alternatively a vector of attributes can be provided (the minimal ones are
automatically added). Argument applicable to \code{movebank_download_study} and \code{movebank_retrieve}. See 'Details' for more information.}

\item{...}{arguments added to the
\href{https://github.com/movebank/movebank-api-doc/blob/master/movebank-api.md}{movebank api} call. See 'Details' for some
common arguments.}

\item{remove_movebank_outliers}{if \code{TRUE} outliers according to the movebank logic are removed. This should
correspond to the \code{visible} attribute in movebank. Argument applicable to \code{movebank_download_study} and \code{movebank_retrieve}.}

\item{entity_type}{the entity type of the data requested from movebank (e.g. \code{"study"}, \code{"tag"}, \code{"event"}).
Alternatively it can be the complete api url for testing purposes. Argument applicable to \code{movebank_retrieve}.}

\item{handle}{the curl handle used to perform the api call, generally this is extracted from the system keyring if
correctly set up with \code{movebank_store_credentials}. Argument applicable to all functions.}

\item{rename_columns}{if \code{TRUE} column names of properties that are repeated in the api output (e.g. \code{id},
\code{local_identifier} and \code{comments}) will be appended with the \code{entity_type} (e.g. "tag", "individual"). Argument applicable to \code{movebank_download_study}, \code{movebank_download_study_info}, \code{movebank_retrieve}.}

\item{omit_derived_data}{derived data (e.g. \code{timestamp_start}, \code{timestamp_end}, \code{number_of_events}
and \code{number_of_deployments}) is omitted from the result. The default is \code{TRUE} as this data quickly becomes
unrepresentative if the results are processed. However in some occasions it might be worth retrieving it,
for example if you want to identify deployment periods without downloading all data. Argument applicable to \code{movebank_download_study} and \code{movebank_retrieve}.}

\item{convert_spatial_columns}{if \code{TRUE} column pairs containing spatial data will be converted to an \code{sfc} column. Argument applicable to all functions.}

\item{progress}{if \code{TRUE} a progress bar will be displayed. More details can be found here \code{\link[vroom]{vroom}}. Argument applicable to \code{movebank_download_study} and \code{movebank_retrieve}.}
}
\value{
\code{movebank_download_study} returns a \code{move2} object. \cr
\code{movebank_retrieve}, \code{movebank_download_deployment}, \code{movebank_download_study_info} return a \code{data.frame}/\code{tbl}.\cr
\code{movebank_get_study_id} returns a \code{\link[bit64:integer64]{big integer}}.
}
\description{
\itemize{
\item \code{movebank_download_study} downloads a complete study from Movebank by the study id or name.
\item \code{movebank_download_deployment} downloads all tag, individual and deployment information and merges it into one
\code{data.frame}
\item \code{movebank_download_study_info} downloads all study level information, either for all studies, one study with
the argument \code{id} or a subset, for example, \code{license_type = "CC_0"}
\item \code{movebank_retrieve} is a more flexible function for retrieving information directly from the api.
\item \code{movebank_get_study_id} using a character string retrieve the associated study id.
}
}
\details{
Caution, when downloading data with \code{movebank_download_study} without specifying the sensor in the argument \code{sensor_type_id}
(see below), all data of all sensors will be downloaded, but only the attributes of location sensors will be
included. We recommend to always specify the sensor(s) to ensure that all associated attributes are downloaded.
Use e.g. \code{movebank_download_study_info(study_id=my_study_id)$sensor_type_ids} to find out which sensors are
available in a given study.
\code{attributes = "all"} is the default, and it will include only location sensor attributes if no sensor is specified
in \code{sensor_type_id}. When sensors are specified, it will download all associated attributes of all sensors.
\code{attributes = NULL} should only be used when downloading location data (by specifying the sensor), as only timestamp,
location and track id is downloaded. To specify only a subset of attributes to download, check the list of attributes
available for a specific sensor (e.g. GPS) in a given study, use
\code{movebank_retrieve(entity_type = "study_attribute", study_id = myStudyID, sensor_type_id = "gps")$short_name}
(more details in "Downloading data from movebank" vignette).

The api is quite flexible for adjusting requests. This is elaborately documented in the
\href{https://github.com/movebank/movebank-api-doc/blob/master/movebank-api.md}{movebank api documentation}. To identify the available arguments, please note that \code{movebank_download_study} is based on the entity_type "event", \code{movebank_download_study_info} on the entity_type "study" and \code{movebank_download_deployment} on the entity_type "deployment", "individual" and "tag".
Here a list of a few arguments that are common for the entity_type "event":
\itemize{
\item \code{sensor_type_id} can be used to restrict the download to specific sensors. It can be either a character or and
integer with the tag_type. For a full list of options see: \code{movebank_retrieve(entity_type='tag_type')}, values from
the \code{id} and \code{external_id} columns are valid.
\item \code{timestamp_start} and \code{timestamp_end} can be used to limit the temporal range to download.
This argument can either be formatted as a \code{POSIXct} timestamp, \code{Date} or a character string
(e.g. \code{"20080604133046000"}(\code{yyyyMMddHHmmssSSS}))
\item \code{event_reduction_profile} might be useful to reduce the data downloaded (e.g. daily locations) possible values
are character strings (e.g. \code{"EURING_01"}). For details see the movebank api
\href{https://github.com/movebank/movebank-api-doc/blob/master/movebank-api.md#get-reduced-event-data}{documentation}.
Note that for the time being the required attributes need to be explicitly stated (e.g. \code{attributes = NULL})
as \code{"all"} does not work with the current movebank api.
\item \code{individual_local_identifier} for selecting one or more individuals by the local identifier
}

For more elaborate usage see \code{vignette("movebank", package='move2')}
}
\examples{
\dontrun{
## download entire study (all data of all sensors)
movebank_download_study_info(study_id = myStudyID)$sensor_type_ids
movebank_download_study(2911040, sensor_type_id = c("gps", "acceleration"))

## download data of one individual
movebank_download_study(2911040,
  individual_local_identifier = "unbanded-160"
)
## download gps data for multiple individuals
movebank_download_study(2911040,
  sensor_type_id = "gps",
  individual_local_identifier = c("1094-1094", "1103-1103")
)
movebank_download_study(2911040,
  sensor_type_id = "gps",
  individual_id = c(2911086, 2911065)
)
## download acceleration data of one or several individuals
movebank_download_study(2911040,
  sensor_type_id = "acceleration",
  individual_local_identifier = "1094-1094"
)
## download data of a specific time window and sensor
movebank_download_study(2911040,
  sensor_type_id = "gps",
  timestamp_start = as.POSIXct("2008-08-01 00:00:00"),
  timestamp_end = as.POSIXct("2008-08-03 00:00:00")
)

## download study filtered to one location per day
## (see movebank api documentation for options)
## also possible to add specific columns in "attributes"
movebank_download_study(2911040,
  sensor_type_id = "gps",
  event_reduction_profile = "EURING_01",
  attributes = NULL
)
## download data associated to tag, individual and deployment
movebank_download_deployment(2911040)
## download study information for all studies
movebank_download_study_info()
## download study information for all studies where you have
## access to download the data
movebank_download_study_info(i_have_download_access = TRUE)
## download study information for a specific study
movebank_download_study_info(id = 2911040)
## get study id
movebank_get_study_id(study_id = "Galapagos Albatrosses")
## Find studies you can download and have a creative commons zero license
## Note "CC_BY" is also frequently used
movebank_download_study_info(
  license_type = "CC_0",
  i_have_download_access = TRUE,
  attributes = c("name", "id")
)
## Download list of own studies
movebank_download_study_info(i_am_owner = TRUE)
}

}
\seealso{
Other movebank-download: 
\code{\link{movebank_handle}()},
\code{\link{movebank_store_credentials}()}
}
\concept{movebank-download}
