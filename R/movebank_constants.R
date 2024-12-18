#' @importFrom vroom col_date col_datetime col_double col_factor col_integer col_character col_logical col_big_integer
NULL
# dput(movebank_retrieve("entity_type"='tag_type')  ) # nolint: commented_code_linter
movebank_tag_type_table <- structure(list(description = c(
  NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Localize tracking devices based on approximated distance to nearby GSM cell towers, WiFi or other network connectors.",
  "Localize devices using a global navigation satellite system",
  "Data derived from other sensor data", "Time-Depth Recording",
  "Location Estimate by ATLAS system"
), external_id = c(
  "bird-ring",
  "gps", "radio-transmitter", "argos-doppler-shift", "natural-mark",
  "acceleration", "solar-geolocator", "accessory-measurements",
  "solar-geolocator-raw", "barometer", "magnetometer", "orientation",
  "solar-geolocator-twilight", "acoustic-telemetry", "gyroscope",
  "heart-rate", "sigfox-geolocation", "proximity", "geolocation-api",
  "gnss", "derived", "tdr", "atlas-geolocation"
), id = structure(c(
  1.96144061398975e-321,
  3.22624866734334e-321, 3.32506179651159e-321, 4.09076473443635e-319,
  1.16880220518501e-317, 1.16880269925066e-317, 1.92011745743723e-317,
  3.87493413331319e-317, 4.59550368042471e-317, 3.8408856487366e-316,
  3.84088619220881e-316, 4.04676003659103e-315, 4.51624043736367e-315,
  6.1243104547751e-315, 6.41135836580693e-315, 1.09001844591634e-314,
  1.13629901961024e-314, 1.30684843265253e-314, 1.52677095314155e-314,
  1.52677095610594e-314, 1.52677095660001e-314, 2.10706387617373e-314,
  2.14568681278764e-314
), class = "integer64"), is_location_sensor = c(
  TRUE,
  TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
  FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE,
  FALSE, TRUE
), name = structure(1:23, levels = c(
  "Bird Ring",
  "GPS", "Radio Transmitter", "Argos Doppler Shift", "Natural Mark",
  "Acceleration", "Solar Geolocator", "Accessory Measurements",
  "Solar Geolocator Raw", "Barometer", "Magnetometer", "Orientation",
  "Solar Geolocator Twilight", "Acoustic Telemetry", "Gyroscope",
  "Heart Rate", "Sigfox Geolocation", "Proximity", "Geolocation API",
  "GNSS", "Derived", "TDR", "ATLAS Geolocation"
), class = "factor")), row.names = c(
  NA,
  -23L
), spec = structure(list(cols = list(description = structure(list(), class = c(
  "collector_character",
  "collector"
)), external_id = structure(list(), class = c(
  "collector_character",
  "collector"
)), id = structure(list(), class = c(
  "collector_big_integer",
  "collector"
)), is_location_sensor = structure(list(), class = c(
  "collector_logical",
  "collector"
)), name = structure(list(
  levels = NULL, ordered = FALSE,
  include_na = FALSE
), class = c("collector_factor", "collector"))), default = structure(list(), class = c(
  "collector_guess",
  "collector"
)), delim = ","), class = "col_spec"), class = c(
  "spec_tbl_df",
  "tbl_df", "tbl", "data.frame"
))


movebank_spatial_column_pairs <- list(
  lora_gateway_location = c("lora_gateway_longitude", "lora_gateway_latitude"),
  argos_location_1 = c("argos_lon1", "argos_lat1"),
  argos_location_2 = c("argos_lon2", "argos_lat2"),
  birth_location = c("birth_longitude", "birth_latitude"),
  animal_birth_hatch_location = c("animal_birth_hatch_longitude", "animal_birth_hatch_latitude"),
  animal_mortality_location = c("animal_mortality_longitude", "animal_mortality_latitude"),
  mortality_location = c("mortality_longitude", "mortality_latitude"),
  capture_location = c("capture_longitude", "capture_latitude"),
  location_median = c("long_median", "lat_median"),
  location_upper = c("long_upper", "lat_upper"),
  location_lower = c("long_lower", "lat_lower"),
  main_location = c("main_location_long", "main_location_lat"),
  deploy_on_location = c("deploy_on_longitude", "deploy_on_latitude"),
  deploy_off_location = c("deploy_off_longitude", "deploy_off_latitude"),
  receiver_location = c("receiver_longitude", "receiver_latitude")
)

# Columns from movebank with these names are automatically converted to the correct units
mb_column_units <- c(
  "acceleration-sampling-frequency-per-axis" = "Hz",
  "accelerations" = "m/s^2",
  "acceleration-x" = "m/s^2",
  "acceleration-y" = "m/s^2",
  "acceleration-z" = "m/s^2",
  "activity-count" = "count",
  "angular-velocity-x" = "arc_degree/s",
  "angular-velocity-y" = "arc_degree/s",
  "angular-velocity-z" = "arc_degree/s",
  "animal-mass" = "g",
  "argos:altitude" = "m",
  "argos:best-level" = "dB",
  "argos:calcul-freq" = "MHz",
  "argos:error-radius" = "m",
  "argos:gdop" = "m/Hz",
  "argos:orientation" = "arc_degree",
  "argos:pass-duration" = "s",
  "argos:semi-major" = "m",
  "argos:semi-minor" = "m",
  "atlas:varx" = "m^2",
  "atlas:varxy" = "m^2",
  "atlas:vary" = "m^2",
  "bar:barometric-depth" = "m",
  "bar:barometric-height" = "m",
  "bar:barometric-pressure" = "mbar",
  "barometric-depth" = "m",
  "barometric-height" = "m",
  "barometric-pressure" = "mbar",
  # "bas:compensated-latitude" = , # decimal degrees, WGS84 reference system
  "bas:mid-value-secs" = "s",
  # "bas:stationary-latitude" = , # decimal degrees, WGS84 reference system
  # "bas:transition-1" = , # units GMT
  # "bas:transition-2" = , # units GMT
  "battery-charge-percent" = "%",
  "battery-charging-current" = "mA",
  "battery-charging-voltage" = "mV",
  "beacon:frequency" = "MHz",
  "capture-handling-time" = "s",
  "compass-heading" = "degree_north",
  "cpu-temperature" = "degree_C",
  "ctt:solar-current-since-last-fix" = "mA",
  "deltatime" = "s",
  # "deploy-off-latitude" = , # decimal degrees, WGS84 reference system
  # "deploy-off-longitude" = , # decimal degrees, WGS84 reference system
  # "deploy-on-latitude" = , # decimal degrees, WGS84 reference system
  # "deploy-on-longitude" = , # decimal degrees, WGS84 reference system
  "depth" = "m",
  "dive-duration" = "s",
  "ecg:sampling-frequency" = "Hz",
  "eobs:acceleration-sampling-frequency-per-axis" = "Hz",
  "eobs:battery-voltage" = "mV",
  "eobs:fix-battery-voltage" = "mV",
  "eobs:horizontal-accuracy-estimate" = "m",
  "eobs:speed-accuracy-estimate" = "m/s",
  "eobs:temperature" = "degree_C",
  "eobs:used-time-to-get-fix" = "s",
  "error-count" = "count",
  "external-temperature" = "degree_C",
  "geolocation-station-count" = "count",
  "geolocator-sun-elevation-angle" = "arc_degree",
  "gmatrix:gps-fix-interval" = "s",
  "gnss:dop" = "1",
  "gnss:hdop" = "1",
  "gnss:maximum-signal-strength" = "dBm",
  "gnss:message-count" = "count",
  "gnss:pdop" = "1",
  "gnss:satellite-count" = "count",
  "gnss:time-to-fix" = "s",
  "gnss:vdop" = "1",
  "gps:activity-count" = "count",
  "gps:dop" = "1",
  "gps:gdop" = "1",
  "gps:hdop" = "1",
  "gps:maximum-signal-strength" = "dBm",
  "gps:message-count" = "count",
  "gps:pdop" = "1",
  "gps:satellite-count" = "count",
  "gps:speed-accuracy-estimate" = "m/s",
  "gps:tdop" = "1",
  "gps-time-to-fix" = "s",
  "gps:vdop" = "1",
  "ground-speed" = "m/s",
  # "gsm:gsm-signal-strength" = "ASU", # arbitrary strength units
  "gt:sys-week" = "count",
  "gt:tx-count" = "count",
  "gyroscope-sampling-frequency-per-axis" = "Hz",
  "heading" = "degree_north",
  "heartrate:ecg-sampling-frequency" = "Hz",
  "heartrate:heart-rate" = "1/min", # Units: beats per minute
  "heartrate:rr-interval" = "s",
  "heartrate:voltage-resolution" = "mV",
  "height-above-ellipsoid" = "m",
  "height-above-ground-level" = "m",
  "height-above-mean-sea-level" = "m",
  "height-above-msl" = "m",
  "icarus:solar-cell-current" = "mA",
  "individual-count" = "count",
  "internal-temperature" = "degree_C",
  # "lat-lower" = , # decimal degrees, WGS84 reference system
  # "lat-upper" = , # decimal degrees, WGS84 reference system
  "location-error-3d" = "m",
  "location-error-numerical" = "m",
  "location-error-percentile" = "%",
  # "long-lower" = , # decimal degrees, WGS84 reference system
  # "long-upper" = , # decimal degrees, WGS84 reference system
  "lora:rssi" = "dBm",
  "lora:snr" = "dB",
  "lora:uplink-count" = "count",
  "lotek:deltatime" = "s",
  "lotek-eres" = "1",
  "mag:magnetic-field-sampling-frequency-per-axis" = "Hz",
  "magnetic-field-sampling-frequency-per-axis" = "Hz",
  "magnetic-field-x" = "microtesla",
  "magnetic-field-y" = "microtesla",
  "magnetic-field-z" = "microtesla",
  "mw:activity-count" = "count",
  "number-of-deployed-locations" = "count",
  "number-of-deployments" = "count",
  "number-of-events" = "count",
  "number-of-individuals" = "count",
  "number-of-tags" = "count",
  "odba" = "standard_free_fall",
  "opencollar:cold-retry" = "count",
  "opencollar:hot-retry" = "count",
  "opencollar:msg" = "count",
  "opencollar:uptime" = "weeks",
  "orientation-quaternions-sampling-frequency" = "Hz",
  "pitch" = "arc_degree",
  "quaternions-sampling-frequency" = "Hz",
  "relative-humidity" = "%",
  "roll" = "arc_degree",
  "rr-interval" = "s",
  "sampling-frequency" = "Hz",
  "sequence-number" = "count",
  "sigfox:computed-location-radius" = "m",
  "sigfox:tx-interval" = "min",
  "solar-cell-voltage" = "mV",
  "solar-voltage-percent" = "%",
  "speed-accuracy" = "m/s",
  "tag-backup-voltage" = "mV",
  "tag-beacon-frequency" = "MHz",
  "tag-mass" = "g",
  "tag-mass-total" = "g",
  "tag-voltage" = "mV",
  "tdr:depth" = "m",
  #  "technosmart:signal-quality" = ,
  # MB figure out how to express the carrier to noise density ratio:
  # https://en.wikipedia.org/wiki/Carrier-to-noise_ratio#Carrier-to-noise_density_ratio
  # Units: dB-Hz (decibel-Hz?)(Wikipedia: dB(Hz) – bandwidth relative to one hertz. E.g., 20 dB‑Hz corresponds to a
  # bandwidth of 100 Hz)
  "temperature-max" = "degree_C",
  "temperature-min" = "degree_C",
  "tilt-angle" = "arc_degree", # Units: degrees
  "tilt-x" = "standard_free_fall",
  "tilt-y" = "standard_free_fall",
  "tilt-z" = "standard_free_fall",
  "tinyfox:activity-percent-last-24h" = "%",
  "tinyfox:pressure-min-last-24h" = "mbar",
  "tinyfox:sunny-index-start-voltage" = "mV",
  "tinyfox:sunny-index-voltage-increase" = "mV",
  "tinyfox:temperature-max-last-24h" = "degree_C",
  "tinyfox:temperature-min-last-24h" = "degree_C",
  "tinyfox:total-vedba-count" = "count",
  "tinyfox:total-vedba" = "m/s^2",
  "tinyfox:vedba-00h-ago" = "m/s^2",
  "tinyfox:vedba-01h-ago" = "m/s^2",
  "tinyfox:vedba-02h-ago" = "m/s^2",
  "tinyfox:vedba-03h-ago" = "m/s^2",
  "tinyfox:vedba-04h-ago" = "m/s^2",
  "tinyfox:vedba-05h-ago" = "m/s^2",
  "tinyfox:vedba-06h-ago" = "m/s^2",
  "tinyfox:vedba-07h-ago" = "m/s^2",
  "tinyfox:vedba-08h-ago" = "m/s^2",
  "tinyfox:vedba-09h-ago" = "m/s^2",
  "tinyfox:vedba-10h-ago" = "m/s^2",
  "tinyfox:vedba-11h-ago" = "m/s^2",
  "tinyfox:vedba-12h-ago" = "m/s^2",
  "tinyfox:vedba-13h-ago" = "m/s^2",
  "tinyfox:vedba-14h-ago" = "m/s^2",
  "tinyfox:vedba-15h-ago" = "m/s^2",
  "tinyfox:vedba-16h-ago" = "m/s^2",
  "tinyfox:vedba-17h-ago" = "m/s^2",
  "tinyfox:vedba-18h-ago" = "m/s^2",
  "tinyfox:vedba-19h-ago" = "m/s^2",
  "tinyfox:vedba-20h-ago" = "m/s^2",
  "tinyfox:vedba-21h-ago" = "m/s^2",
  "tinyfox:vedba-22h-ago" = "m/s^2",
  "tinyfox:vedba-23h-ago" = "m/s^2",
  "tinyfox:vedba-burst-sum" = "m/s^2",
  "underwater-count" = "count",
  "underwater-time" = "s",
  "utm-easting" = "m", # Units: meters, WGS84 reference system
  "utm-northing" = "m", # Units: meters, WGS84 reference system
  "vedba" = "standard_free_fall",
  "vertical-error-numerical" = "m",
  "vertical-speed" = "m/s",
  "voltage-resolution" = "mV",
  "wc-residual" = "1",
  "weight" = "g",
  "wet-count" = "count",
  "zero-crossings-steps" = "count",
  NULL
)
movebank_column_types <- list(
  "<html>" = col_character(),
  "acceleration-axes" = col_character(),
  "acceleration-raw-x" = col_double(),
  "acceleration-raw-y" = col_double(),
  "acceleration-raw-z" = col_double(),
  "acceleration-sampling-frequency-per-axis" = col_double(),
  "accelerations" = col_character(),
  "accelerations-raw" = col_character(),
  "acceleration-x" = col_double(),
  "acceleration-y" = col_double(),
  "acceleration-z" = col_double(),
  "acknowledgements" = col_character(),
  "activity-count" = col_integer(),
  "activity-x" = col_double(), # http://vocab.nerc.ac.uk/collection/MVB/current/MVB000334/
  "activity-y" = col_double(), # http://vocab.nerc.ac.uk/collection/MVB/current/MVB000335/
  "activity-z" = col_double(), # http://vocab.nerc.ac.uk/collection/MVB/current/MVB000336/
  "algorithm-marked-outlier" = col_logical(),
  "alt-index-id" = col_character(),
  "alt-project-id" = col_factor(),
  # "ambiguous-detection", # not documented in mb dictionary
  "angular-velocities-raw" = col_character(),
  "angular-velocity-x" = col_double(),
  "angular-velocity-y" = col_double(),
  "angular-velocity-z" = col_double(),
  "animal-birth-hatch-latitude" = col_double(),
  "animal-birth-hatch-longitude" = col_double(),
  "animal-comments" = col_character(),
  "animal-death-comments" = col_character(),
  "animal-earliest-date-born" = col_datetime(),
  "animal-exact-date-of-birth" = col_datetime(),
  "animal-group-id" = col_factor(),
  "animal-id" = col_factor(),
  "animal-latest-date-born" = col_datetime(),
  "animal-life-stage" = col_factor(),
  "animal-marker-id" = col_factor(),
  "animal-mass" = col_double(),
  "animal-mates" = col_factor(),
  "animal-mortality-date" = col_date(),
  "animal-mortality-latitude" = col_double(),
  "animal-mortality-longitude" = col_double(),
  "animal-mortality-type" = col_factor(
    levels = c(
      "bycatch", "capture", "electrocution", "harvest", "disease",
      "natural-death", "other", "parasites", "poison", "predation",
      "starvation", "unknown", "vehicle-collision"
    )
  ),
  "animal-nickname" = col_factor(),
  "animal-offspring" = col_factor(),
  "animal-parents" = col_factor(),
  "animal-reproductive-condition" = col_factor(),
  "animal-ring-id" = col_factor(),
  "animal-sex" = col_factor(levels = c("m", "f", "u")),
  "animal-siblings" = col_factor(),
  "animal-taxon" = col_factor(),
  "animal_taxon_detail" = col_factor(),
  "argos:altitude" = col_double(),
  "argos:best-level" = col_double(),
  "argos:calcul-freq" = col_double(),
  "argos:error-radius" = col_double(),
  "argos:gdop" = col_double(),
  "argos:iq" = col_factor(
    levels = c(apply(expand.grid(0:6, 0:8), 1, paste0, collapse = ""), 0:8, "99")
  ), # MB argos:iq 99 is not in the documentation but does occur in study 1420256397
  "argos:lat1" = col_double(),
  "argos:lat2" = col_double(),
  "argos:lc" = col_factor(levels = c("G", 3:0, "A", "B", "Z"), ordered = TRUE),
  "argos:location-algorithm" = col_factor(levels = c("least-squares", "Kalman")),
  "argos:lon1" = col_double(),
  "argos:lon2" = col_double(),
  "argos:nb-mes-120" = col_integer(),
  "argos:nb-mes" = col_integer(),
  # "argos:nb-mes-identical", # not documented in mb dictionary
  "argos:nopc" = col_factor(ordered = TRUE, levels = as.character(c(0:4, 9))), # MB argos:nopc 9 is not in the documentation but does occur in study 1420256397
  "argos:orientation" = col_double(),
  "argos:pass-duration" = col_double(),
  "argos:sat-id" = col_factor(),
  "argos:semi-major" = col_double(),
  "argos:semi-minor" = col_double(),
  "argos:sensor-1" = col_integer(),
  "argos:sensor-2" = col_integer(),
  "argos:sensor-3" = col_integer(),
  "argos:sensor-4" = col_integer(),
  "argos:transmission-timestamp" = col_datetime(),
  "argos:valid-location-algorithm" = col_factor(levels = as.character(1L:2L)),
  "argos:valid-location-manual" = col_factor(levels = as.character(1L:2L)),
  "atlas:varx" = col_double(),
  "atlas:varxy" = col_double(),
  "atlas:vary" = col_double(),
  "attachment-body-part" = col_factor(),
  "attachment-comments" = col_character(),
  "attachment-type" = col_factor(
    levels = c(
      "backpack-harness", "collar", "ear-tag", "fin mount", "glue",
      "harness", "implant", "leg-band", "leg-loop-harness", "none",
      "other", "subcutaneous-anchor", "suction-cup", "sutures", "tape"
    )
  ),
  "awt:alarms-battery" = col_logical(),
  "awt:alarms-cbit" = col_logical(),
  "awt:alarms-coverage" = col_logical(),
  "awt:alarms-geofence" = col_logical(),
  "awt:alarms-memory" = col_logical(),
  "awt:alarms-movement" = col_logical(),
  "awt:alarms-tamperfoil" = col_logical(),
  "awt:alarms-track-mode" = col_logical(),
  "awt:log-interval" = col_factor(
    levels = c(
      "every 10 minutes", "every 30 minutes", "every 1 hour",
      "every 2 hours", "every 3 hours", "every 4 hours",
      "every 5 hours", "every 6 hours", "every 8 hours",
      "every 12 hours", "every 24 hours", "every 48 hours", "off"
    ), ordered = TRUE
  ),
  "bar:barometric-depth" = col_double(),
  "bar:barometric-height" = col_double(),
  "bar:barometric-pressure" = col_double(),
  "barometric-depth" = col_double(),
  "barometric-height" = col_double(),
  "barometric-pressure" = col_double(),
  "bas:compensated-latitude" = col_double(),
  "bas:confidence" = col_factor(), # MB only integer values allowed?
  "bas:fix-type" = col_factor(levels = c("noon", "midnight")),
  "bas:mid-value-secs" = col_double(),
  "bas:stationary-latitude" = col_double(),
  "bas:transition-1" = col_double(),
  "bas:transition-2" = col_double(),
  "battery-charge-percent" = col_integer(), # integer in study
  "battery-charging-current" = col_double(),
  "battery-charging-voltage" = col_logical(),
  "beacon:frequency" = col_double(),
  "behavior-according-to" = col_character(),
  "behavioural-classification" = col_factor(),
  "breed-stage" = col_character(),
  "canonical-name" = col_factor(),
  "capture-handling-time" = col_double(),
  "capture-latitude" = col_double(),
  "capture-longitude" = col_double(),
  "capture-method" = col_character(),
  "capture-timestamp" = col_datetime(),
  "citation" = col_character(),
  "comments" = col_character(),
  "compass-heading" = col_double(),
  "conductivity" = col_double(),
  "contact-person" = col_character(),
  "contact-person-id" = col_factor(),
  "contact-person-name" = col_factor(),
  "cpu-temperature" = col_double(),
  "ctt:solar-current-since-last-fix" = col_integer(),
  "data-decoding-software" = col_factor(),
  "data-processing-software" = col_factor(),
  "dba-comments" = col_character(),
  "death-comments" = col_character(),
  "deltatime" = col_integer(),
  "deployment-comments" = col_character(),
  "deployment-end-comments" = col_character(),
  "deployment-end-type" = col_factor(
    levels = c(
      "captured", "dead", "dead/fall-off",
      "equipment-failure", "fall-off", "other", "released",
      "removal", "transmission-end", "unknown"
    )
  ),
  "deployment-id" = col_big_integer(), # deployment id now can be a given name defined by de user and, in its absence a large integer created by movebank
  "deployment-image" = col_character(),
  "deploy-off-date" = col_datetime(),
  "deploy-off-latitude" = col_double(),
  "deploy-off-longitude" = col_double(),
  "deploy-off-measurements" = col_character(),
  "deploy-off-person" = col_character(),
  "deploy-off-sampling" = col_character(),
  "deploy-off-timestamp" = col_datetime(),
  "deploy-on-date" = col_datetime(),
  "deploy-on-latitude" = col_double(),
  "deploy-on-longitude" = col_double(),
  "deploy-on-measurements" = col_character(),
  "deploy-on-person" = col_character(),
  "deploy-on-sampling" = col_character(),
  "deploy-on-timestamp" = col_datetime(),
  "depth" = col_double(),
  "dive-duration" = col_integer(),
  "druid:alert" = col_factor(levels = 1),
  "druid:data-source" = col_factor(
    levels = c("[1]", "[2]", "[4]", "[8]", "[10]", "[100]", "[101]")
  ),
  "duty-cycle" = col_character(),
  "earliest-date-born" = col_datetime(), # seems to be undocumented api name
  "ecg:raw" = col_double(),
  "ecg:sampling-frequency" = col_double(),
  "ecgs:raw" = col_character(),
  "end-timestamp" = col_datetime(),
  "eobs:acceleration-axes" = col_factor(),
  "eobs:acceleration-sampling-frequency-per-axis" = col_double(),
  "eobs:accelerations-raw" = col_character(),
  "eobs:activity" = col_integer(),
  "eobs:activity-samples" = col_integer(),
  "eobs:battery-voltage" = col_integer(),
  "eobs:fix-battery-voltage" = col_integer(),
  "eobs:horizontal-accuracy-estimate" = col_double(),
  "eobs:key-bin-checksum" = col_big_integer(),
  "eobs:speed-accuracy-estimate" = col_double(),
  "eobs:start-timestamp" = col_datetime(),
  "eobs:status" = col_factor(ordered = TRUE, levels = LETTERS[1L:4L]),
  "eobs:temperature" = col_integer(),
  "eobs:type-of-fix" = col_factor(), # MB galapagos albatrosses have a type of fix 0 for eobs:type-of-fix I expect either 2 or 3 : http://vocab.nerc.ac.uk/collection/MVB/current/MVB000100/
  "eobs:used-time-to-get-fix" = col_integer(),
  "error-count" = col_integer(),
  "event-group-id" = col_factor(),
  "event-id" = col_big_integer(),
  "exact-date-of-birth" = col_datetime(), # seems to be undocumented api name
  "external-temperature" = col_double(),
  "flt:switch" = col_factor(
    levels = c(as.character(c(0L, 6L, 48L, 49L, 64L, 69L, 72L, 77L, 112L))), ordered = TRUE
  ), # MB check if ordered
  "format-type" = col_character(),
  "geolocation-station-count" = col_integer(),
  "geolocator-calibration" = col_character(),
  "geolocator-fix-type" = col_factor(),
  "geolocator-light-threshold" = col_double(),
  "geolocator-rise" = col_logical(),
  "geolocator-sensor-comments" = col_character(),
  "geolocator-sun-elevation-angle" = col_double(),
  "geolocator-twilight3" = col_datetime(),
  "georeference-protocol" = col_factor(),
  "gls:light-level" = col_double(),
  "gls:twilight" = col_integer(),
  "gmatrix:alert" = col_integer(),
  "gmatrix:alert-message" = col_factor(),
  "gmatrix:device-type" = col_factor(),
  "gmatrix:event" = col_factor(levels = as.character(c(16L, 48L, 49L, 50L, 51L, 52L, 80L))),
  "gmatrix:firmware-version" = col_factor(),
  "gmatrix:gps-event" = col_factor(levels = as.character(c(0L:5L))),
  "gmatrix:gps-event-text" = col_factor(
    levels = c("Unknown location", "Alert", "Panic", "Timed", "Fall", "Heartbeat")
  ),
  "gmatrix:gps-fix-interval" = col_integer(),
  "gmatrix:gps-fix-interval-text" = col_factor(),
  "gmatrix:hardware-version" = col_factor(),
  "gmatrix:is-moving" = col_factor(levels = c("A1", "0")),
  "gmatrix:is-moving-text" = col_factor(levels = c("MOVING", "STATIC")),
  "gmatrix:last-location" = col_factor(levels = c("null", "0", "1")),
  "gmatrix:last-location-text" = col_factor(
    levels = c("not available", "new location", "last known location")
  ),
  "gmatrix:mode" = col_factor(levels = as.character(c(0:6))),
  "gmatrix:mode-text" = col_factor(
    levels = c(
      "reserved", "help me", "press it", "track it",
      "guard it", "monitor it", "don't lose it"
    )
  ),
  "gmatrix:packet-type" = col_factor(), # MB check with Sarah if can be integer
  "gnss:dop" = col_double(),
  "gnss:fix-type" = col_factor(ordered = TRUE, levels = 1L:3L),
  "gnss:fix-type-raw" = col_factor(),
  "gnss:hdop" = col_double(),
  "gnss:maximum-signal-strength" = col_double(),
  "gnss:message-count" = col_integer(),
  "gnss:pdop" = col_double(),
  "gnss:satellite-count" = col_integer(),
  "gnss:time-to-fix" = col_double(),
  "gnss:vdop" = col_double(),
  "go_public_date" = col_datetime(),
  "gps:activity-count" = col_integer(),
  "gps:dop" = col_double(), # http://vocab.nerc.ac.uk/collection/MVB/current/MVB000115/
  "gps:fix-type" = col_factor(
    levels = as.character(0:3), ordered = TRUE
  ), # MB 0 not in vocabulary for fix type but in study 619097045
  "gps:fix-type-raw" = col_factor(),
  "gps:gdop" = col_double(), # http://vocab.nerc.ac.uk/collection/MVB/current/MVB000360/
  "gps:hdop" = col_double(), # http://vocab.nerc.ac.uk/collection/MVB/current/MVB000118/
  "gps:maximum-signal-strength" = col_double(),
  "gps:message-count" = col_integer(),
  "gps:pdop" = col_double(), # http://vocab.nerc.ac.uk/collection/MVB/current/MVB000284/
  "gps:satellite-count" = col_integer(),
  "gps:speed-accuracy-estimate" = col_double(),
  "gps:tdop" = col_double(), # http://vocab.nerc.ac.uk/collection/MVB/current/MVB000361/
  "gps-time-to-fix" = col_double(),
  "gps:twilight" = col_factor(),
  "gps:vdop" = col_double(), # http://vocab.nerc.ac.uk/collection/MVB/current/MVB000122/
  "grants-used" = col_character(),
  "ground-speed" = col_double(),
  "gsat:charge-mode" = col_factor(
    levels = c("Discharge", "ChargeSlow", "ChargeMedium", "ChargeHigh"), ordered = TRUE
  ),
  "gsat:low-power-mode" = col_logical(),
  "gsat:orientation" = col_factor(
    levels = c("XPositive", "XNegative", "YPositive", "YNegative", "ZPositive", "Znegative")
  ),
  "gsm:gsm-signal-strength" = col_integer(),
  "gsm:mcc-mnc" = col_integer(),
  # "gt:activity-count", # not documented in mb dictionary
  "gt:sys-week" = col_integer(),
  "gt:tx-count" = col_integer(),
  "guaternions-raw" = col_character(),
  "gundi-urn" = col_character(),
  "gyroscope-axes" = col_factor(),
  "gyroscope-sampling-frequency-per-axis" = col_double(),
  "habitat-according-to" = col_character(),
  "habitat" = col_factor(),
  "heading" = col_double(),
  "heartrate:ecg-raw" = col_integer(),
  "heartrate:ecg-sampling-frequency" = col_double(),
  "heartrate:ecgs-raw" = col_character(),
  "heartrate:heart-rate" = col_double(),
  "heartrate:rr-interval" = col_double(),
  "heartrate:voltage-resolution" = col_double(),
  "height-above-ellipsoid" = col_double(),
  "height-above-ground-level" = col_double(),
  "height-above-mean-sea-level" = col_double(),
  "height-above-msl" = col_double(),
  "height-raw" = col_character(),
  "i-am-collaborator" = col_logical(),
  "i-am-owner" = col_logical(),
  "i-can-see-data" = col_logical(),
  "icarus:solar-cell-current" = col_double(),
  "id" = col_big_integer(),
  "i-have-download-access" = col_logical(),
  "import-marked-outlier" = col_logical(),
  "individual-count" = col_integer(),
  "individual-id" = col_big_integer(),
  "individual-local-identifier" = col_factor(),
  "individual-taxon-canonical-name" = col_factor(),
  "internal-temperature" = col_double(),
  "is-location-sensor" = col_logical(),
  "is-test" = col_logical(),
  "latest-date-born" = col_datetime(), # seems to be undocumented api name
  "lat-lower" = col_double(),
  "lat-median" = col_double(),
  "lat-sd" = col_double(),
  "lat-upper" = col_double(),
  "license-terms" = col_character(),
  "license-type" = col_factor(levels = c("CC_BY", "CC_BY_NC", "CC_0", "CUSTOM")),
  "light-level" = col_double(),
  "local-identifier" = col_factor(), # needed because api does not append tag_ / individual_ when downloading the specific info
  "locality-according-to" = col_character(),
  "locality" = col_factor(),
  "location-accuracy-comments" = col_character(), # deployment property
  "location-error-3d" = col_double(),
  "location-error-numerical" = col_double(),
  "location-error-percentile" = col_double(),
  "location-error-text" = col_character(),
  "location-lat" = col_double(),
  "location-long" = col_double(),
  "long-lower" = col_double(),
  "long-median" = col_double(),
  "long-sd" = col_double(),
  "long-upper" = col_double(),
  "lora:data-rate" = col_double(),
  "lora:gateway-id" = col_factor(),
  "lora:gateway-latitude" = col_double(),
  "lora:gateway-longitude" = col_double(),
  "lora:payload" = col_character(),
  "lora:port" = col_integer(),
  "lora:rssi" = col_double(),
  "lora:snr" = col_integer(),
  "lora:spreading-factor" = col_double(),
  "lora:uplink-count" = col_integer(),
  "lot:crc-status" = col_factor(levels = c("G", "E", "F")),
  "lot:crc-status-text" = col_factor(levels = c("OK", "Fail", "OK(corrected)")), # might be outdated
  "lotek:deltatime" = col_integer(),
  "lotek-eres" = col_double(),
  # "lotek-fix-status", # not documented in mb dictionary
  "mag:magnetic-field-axes" = col_factor(),
  "mag:magnetic-field-raw-x" = col_double(),
  "mag:magnetic-field-raw-y" = col_double(),
  "mag:magnetic-field-raw-z" = col_double(),
  "mag:magnetic-field-sampling-frequency-per-axis" = col_double(),
  "mag:magnetic-fields-raw" = col_character(),
  "magnetic-field-sampling-frequency-per-axis" = col_double(),
  "magnetic-field-x" = col_double(),
  "magnetic-field-y" = col_double(),
  "magnetic-field-z" = col_double(),
  "main-location-lat" = col_double(),
  "main-location-long" = col_double(),
  "manipulation-comments" = col_character(),
  "manipulation-status" = col_factor(),
  "manipulation-type" = col_factor(
    levels = c(
      "confined", "domesticated", "manipulated-other",
      "none", "reintroduction", "relocated"
    )
  ), # MB see https://github.com/movebank/movebank-api-doc/issues/16 about manipulated-other
  "manually-marked-outlier" = col_logical(),
  "manually-marked-valid" = col_logical(),
  "manufacturer-name" = col_factor(),
  "migration-stage" = col_factor(),
  "migration-stage-standard" = col_factor(
    levels = c(
      "altitudinal-migration", "breeding-grounds", "fall-migration",
      "foraging-grounds", "foraging-migration", "irruptive-migration",
      "latitudinal-migration", "migration-to-molt-site", "molt-site",
      "natal-area", "nomadic-migration", "other-seasonal-migration",
      "removal-migration", "reproductive-migration", "spawning-grounds",
      "spring-migration", "stopover", "summer-non-breeding",
      "vertical-migration-(aquatic)", "wintering-grounds"
    )
  ),
  "model" = col_factor(), # = tag_model
  "modelled" = col_logical(),
  "mortality-status" = col_factor(),
  # "motus:batch-id", # not documented in mb dictionary
  "mw:activity-count" = col_double(), # in study 108391885
  "mw:show-in-kml" = col_logical(),
  "mw:show-in-KML" = col_logical(),
  "name" = col_factor(), # = study name
  "nick-name" = col_factor(),
  "number-of-deployed-locations" = col_integer(),
  "number-of-deployments" = col_integer(),
  "number-of-events" = col_integer(),
  "number-of-individuals" = col_integer(),
  "number-of-tags" = col_integer(),
  "observer" = col_factor(),
  "odba" = col_double(),
  "opencollar:active-tracking" = col_logical(),
  "opencollar:cold-retry" = col_integer(),
  "opencollar:error-acc" = col_logical(),
  "opencollar:error-bat" = col_logical(),
  "opencollar:error-ble" = col_logical(),
  "opencollar:error-flash" = col_logical(),
  "opencollar:error-lr" = col_logical(),
  "opencollar:error-lr-join" = col_logical(),
  "opencollar:error-ublox" = col_logical(),
  "opencollar:error-ublox-fix" = col_logical(),
  "opencollar:hot-retry" = col_integer(),
  "opencollar:locked" = col_logical(),
  "opencollar:msg" = col_integer(),
  "opencollar:payload-type" = col_factor(),
  "opencollar:reset" = col_integer(),
  "opencollar:uptime" = col_double(),
  "opencollar:ver-fw-major" = col_factor(),
  "opencollar:ver-fw-minor" = col_factor(),
  "opencollar:ver-fw-type" = col_factor(),
  "orientation:quaternion-raw-w" = col_double(),
  "orientation:quaternion-raw-x" = col_double(),
  "orientation:quaternion-raw-y" = col_double(),
  "orientation:quaternion-raw-z" = col_double(),
  "orientation-quaternions-raw" = col_character(),
  "orientation-quaternions-sampling-frequency" = col_double(),
  "orn:transmission-protocol" = col_factor(levels = c("GPRS", "SMS")),
  "outlier-comments" = col_character(),
  "pitch" = col_double(),
  "principal-investigator-address" = col_character(),
  "principal-investigator" = col_character(),
  "principal-investigator-email" = col_character(),
  "principal-investigator-name" = col_character(),
  # "processing-type" = col_character(), # currently not used
  "proofed" = col_logical(),
  "provider-update-ts" = col_datetime(),
  "prox-id" = col_factor(),
  "prox-signal-strength" = col_double(),
  "quaternion-raw-w" = col_double(),
  "quaternion-raw-x" = col_double(),
  "quaternion-raw-y" = col_double(),
  "quaternion-raw-z" = col_double(),
  "quaternions-sampling-frequency" = col_double(),
  "raptor-workshop:behaviour" = col_factor(levels = c("Nesting", "Roosting")),
  "raptor-workshop:deployment-special-event" = col_factor(
    levels = c("BiologicallyIrrelecant", "Death", "ReleaseSite", "TagFailure")
  ),
  "raptor-workshop:migration-state" = col_factor(
    levels = c(
      "BreedingGrounds", "FallMigration", "Migration", "NatalArea",
      "NonBreedingGrounds", "SpringMigration", "StopOver", "SummerNonBreeding"
    )
  ),
  "receiver-deployment-id" = col_factor(),
  "receiver-detector-id" = col_factor(),
  "receiver-id" = col_factor(),
  "receiver-latitude" = col_double(),
  "receiver-longitude" = col_double(),
  "relative-humidity" = col_double(),
  "roll" = col_double(),
  "rr-interval" = col_double(),
  "sampling-frequency" = col_double(),
  "savannah:alarm-type" = col_factor(),
  "savannah:record-type" = col_factor(levels = c("P", "A")),
  "scheduled-detachment-date" = col_date(),
  "sensolus:fix-type" = col_character(),
  "sensolus:location-source" = col_character(),
  "sensor-type" = col_factor(levels = movebank_tag_type_table$`external_id`),
  "sensor-type-id" = col_big_integer(),
  "sensor-type-ids" = col_character(),
  "sequence-number-burst" = col_character(),
  "sequence-number" = col_integer(),
  "serial_no" = col_factor(),
  "sigfox:computed-location-radius" = col_double(),
  "sigfox:computed-location-source" = col_factor(levels = as.character(c(1L, 2L, 6L))),
  "sigfox:computed-location-status" = col_factor(levels = c("00", "01", "02", "20"), include_na = T),
  "sigfox:country" = col_factor(),
  "sigfox:device-type" = col_factor(),
  "sigfox:duplicates" = col_character(),
  "sigfox:format-character" = col_factor(levels = c(0:9, LETTERS[1L:6L]), ordered = TRUE),
  "sigfox:link-quality" = col_factor(levels = as.character(c(0L:4L)), ordered = TRUE),
  "sigfox:lqi" = col_factor(
    levels = c("Limit", "Average", "Good", "Excellent", "NA"), ordered = TRUE
  ),
  "sigfox:operator" = col_factor(),
  "sigfox:payload" = col_character(),
  "sigfox:rssi" = col_double(),
  "sigfox:sensor-data-raw" = col_character(), ## its in "hexadecimal format"
  "sigfox:tx-interval" = col_double(), ### MB integer?
  "sigfox:tx-interval-text" = col_character(),
  "solar-cell-voltage" = col_double(),
  "solar-voltage-percent" = col_double(),
  "speed-accuracy" = col_double(),
  "start-timestamp" = col_datetime(),
  "study-id" = col_big_integer(),
  "study-local-timestamp" = col_datetime(),
  "study-name" = col_factor(),
  "study-objective" = col_character(),
  "study-permission" = col_factor(levels = c("collaborator", "data_manager", "na")),
  "study-site" = col_character(),
  "study-specific-measurement" = col_character(),
  "study-summary" = col_character(),
  "study-timezone" = col_factor(),
  "study-type" = col_factor(),
  "suspend-license-terms" = col_logical(),
  "tag-backup-voltage" = col_double(),
  # "tag-battery-voltage",# not documented in mb dictionary
  "tag-beacon-frequency" = col_double(),
  "tag-calibration" = col_character(),
  "tag-comments" = col_character(),
  "tag-failure-comments" = col_character(),
  "tag-firmware" = col_factor(),
  "tag-id" = col_big_integer(),
  "tag-local-identifier" = col_factor(),
  "tag-manufacturer-name" = col_factor(),
  "tag-mass" = col_double(),
  "tag-mass-total" = col_double(),
  "tag-model" = col_factor(),
  # "tag-processing-type" = col_character(), ## currently not used
  "tag-production-date" = col_character(),
  "tag-readout-method" = col_factor(
    levels = c(
      "ISS", "LPWAN", "multiple", "none", "other-wireless",
      "phone-network", "satellite", "tag-retrieval",
      "telemetry-network", "Wi-Fi/Bluetooth"
    )
  ),
  "tag-retrieval-date" = col_date(),
  "tag-serial-no" = col_factor(),
  "tag_serial_no" = col_factor(),
  "tag-settings" = col_character(),
  "tag-tech-spec" = col_character(),
  "tag-voltage" = col_double(),
  "taxon-canonical-name" = col_factor(),
  "taxon_detail" = col_factor(),
  "taxon-ids" = col_character(),
  "tdr:depth" = col_double(),
  "technosmart:activity" = col_factor(levels = c("active", "inactive")),
  "technosmart:signal-quality" = col_double(),
  "telemetry-detection-count" = col_integer(),
  "telemetry-run-id" = col_integer(),
  "temperature-max" = col_double(),
  "temperature-min" = col_double(),
  "there-are-data-which-i-cannot-see" = col_logical(),
  "tilt-angle" = col_double(),
  "tilt-x" = col_double(),
  "tilt-y" = col_double(),
  "tilt-z" = col_double(),
  "timestamp" = col_datetime(),
  "timestamp-end" = col_datetime(),
  "timestamp-first-deployed-location" = col_datetime(),
  "timestamp-last-deployed-location" = col_datetime(),
  "timestamp-of-first-deployed-location" = col_datetime(),
  "timestamp-of-last-deployed-location" = col_datetime(),
  "timestamp-start" = col_datetime(),
  "tinyfox:activity-percent-last-24h" = col_double(),
  "tinyfox:error-happened" = col_logical(),
  "tinyfox:last-error" = col_factor(levels = as.character(c(1L, 2L, 4L, 5L, 6L, 7L))),
  "tinyfox:max-movement-index" = col_factor(levels = as.character(c(0L:3L)), ordered = TRUE),
  "tinyfox:memory-full" = col_logical(),
  "tinyfox:pressure-min-last-24h" = col_double(),
  "tinyfox:software-version" = col_character(),
  "tinyfox:sunny-index-start-voltage" = col_double(),
  "tinyfox:sunny-index-voltage-increase" = col_double(),
  "tinyfox:tag-activation" = col_logical(),
  "tinyfox:temperature-max-last-24h" = col_double(),
  "tinyfox:temperature-min-last-24h" = col_double(),
  "tinyfox:total-vedba" = col_double(),
  "tinyfox:total-vedba-count" = col_integer(),
  "tinyfox:vedba-00h-ago" = col_double(),
  "tinyfox:vedba-01h-ago" = col_double(),
  "tinyfox:vedba-02h-ago" = col_double(),
  "tinyfox:vedba-03h-ago" = col_double(),
  "tinyfox:vedba-04h-ago" = col_double(),
  "tinyfox:vedba-05h-ago" = col_double(),
  "tinyfox:vedba-06h-ago" = col_double(),
  "tinyfox:vedba-07h-ago" = col_double(),
  "tinyfox:vedba-08h-ago" = col_double(),
  "tinyfox:vedba-09h-ago" = col_double(),
  "tinyfox:vedba-10h-ago" = col_double(),
  "tinyfox:vedba-11h-ago" = col_double(),
  "tinyfox:vedba-12h-ago" = col_double(),
  "tinyfox:vedba-13h-ago" = col_double(),
  "tinyfox:vedba-14h-ago" = col_double(),
  "tinyfox:vedba-15h-ago" = col_double(),
  "tinyfox:vedba-16h-ago" = col_double(),
  "tinyfox:vedba-17h-ago" = col_double(),
  "tinyfox:vedba-18h-ago" = col_double(),
  "tinyfox:vedba-19h-ago" = col_double(),
  "tinyfox:vedba-20h-ago" = col_double(),
  "tinyfox:vedba-21h-ago" = col_double(),
  "tinyfox:vedba-22h-ago" = col_double(),
  "tinyfox:vedba-23h-ago" = col_double(),
  "tinyfox:vedba-burst-sum" = col_double(),
  "track-segment-id" = col_factor(),
  "transmission-protocol" = col_factor(),
  "transmission-timestamp" = col_datetime(),
  "tsn" = col_integer(),
  "twilight" = col_factor(),
  "twilight-excluded" = col_logical(),
  "twilight-inserted" = col_logical(),
  "underwater-count" = col_integer(),
  "underwater-time" = col_double(),
  "update-ts" = col_datetime(),
  "utm-easting" = col_double(),
  "utm-northing" = col_double(),
  "utm-zone" = col_factor(),
  "vectronics-activity-mode" = col_factor(levels = as.character(1L:7L)),
  "vedba" = col_double(),
  "vertical-error-numerical" = col_double(),
  "vertical-speed" = col_double(),
  "visible" = col_logical(),
  "voltage-resolution" = col_double(),
  "waterbird-workshop:behaviour" = col_factor(levels = c("Nesting", "Roosting")),
  "waterbird-workshop:deployment-special-event" = col_factor(
    levels = c("BiologicallyIrrelevant", "Death", "ReleaseSite", "TagFailure")
  ),
  "waterbird-workshop:migration-state" = col_factor(
    levels = c(
      "BreedingGrounds", "FallMigration", "MigrationOther",
      "MigrationToMolt", "MoltSite", "SpringMigration",
      "StopOver", "SummerNonBreedingAdult",
      "SummerNonBreedingImmature", "WinterGrounds"
    )
  ),
  "wc-residual" = col_double(),
  "weight" = col_double(),
  "wet" = col_logical(),
  "wet-count" = col_integer(),
  "wildfi-acc-conversion-factor" = col_double(),
  "wildfi-gyro-conversion-factor" = col_double(),
  "wildfi-hall-burst" = col_character(),
  "wildfi-last-error" = col_factor(),
  "wildfi-mag-conversion-factor" = col_double(),
  "wildfi-prox-id-burst" = col_character(),
  "wildfi-prox-rssi-burst" = col_character(),
  "wildfi-record-type" = col_factor(),
  "zero-crossings-amplitude" = col_double(),
  "zero-crossings-steps" = col_integer(),
  NULL
) # add null so it is easy to sort the list (all have comma at end)

movebank_column_types <- movebank_column_types[!unlist(lapply(movebank_column_types, is.null))]
movebank_colon_names <- list(
  "lotek_" = "^lot:", "ornitela_" = "^orn:", "^heartrate:", "^mag:", "gsm" = "^gsm:gsm",
  "gsm_mcc" = "^gsm:mcc",
  "^bar:", "^gls:", "_" = ":"
)
to_download_names <- function(x) {
  for (i in seq_along(movebank_colon_names)) {
    x <- gsub(
      movebank_colon_names[[i]],
      names(movebank_colon_names)[[i]], x
    )
  }
  x <- gsub("-", "_", x, fixed = TRUE)
  return(x)
}

mb_column_units_underscore <- mb_column_units
names(mb_column_units_underscore) <-
  to_download_names(names(mb_column_units_underscore))

mb_column_types_underscore <- movebank_column_types
names(mb_column_types_underscore) <-
  to_download_names(names(mb_column_types_underscore))
toadd <- mb_column_types_underscore[grep(
  "^animal_",
  names(mb_column_types_underscore)
)]
names(toadd) <- sub("^animal_", "", names(toadd))
mb_column_types_underscore <- c(mb_column_types_underscore, toadd)
rm(toadd)


movebank_valid_entity_types <- c(
  "study", "individual", "tag", "sensor", "tag_type", "taxon",
  "deployment", "event", "event_reduced", "study_attribute"
)


movebank_minimal_columns <-
  c(
    "location-long",
    "location-lat",
    "timestamp",
    "individual-local-identifier"
  )

movebank_track_attributes <- c(
  "individual-taxon-canonical-name",
  "study-name",
  "death-comments", "study-id"
) # comments don't seem to be attributes for sensors
movebank_track_attributes <- c(
  movebank_track_attributes,
  gsub("-", "_",
    movebank_track_attributes,
    fixed = TRUE
  ),
  "individual_local_identifier"
) # individual local identifier here because in CSV its used as id column
