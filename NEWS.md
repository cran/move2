# move2 0.4.4

* Add `useragent` to movebank requests (#93)
* Ensure turn angles between -180 & 180 with changes in `units` (#94)

# move2 0.4.2

* Revert changes to deployment id as movebank seem to still return numeric ids (#92)

# move2 0.4.0

* Ensure outliers are removed when attributes are selected (#91, @RonEfrat)
* In test account for timezone EST not being available
* Update stored copy of tag type table to include newest types
* Add citation to published paper
* Update movebank vignette
* Catch condition when no rows are returned from movebank
* Add functionality to download study info with study name (#83)
* Ensure preferred label is used in case of conflict (#84, @peterdesmet)
* Depend on recent version of `bit64` to prevent errors (#21)

# move2 0.3.0

* Fix assertion messaging in `movebank_retrieve`
* Resolve some telemetry import issues (#65)
* Support reading zip files from Env-DATA (#49)
* If `lubridate` is installed improve the printing of the average track duration
* Prevent superfluous printing of join column in `mt_track_lines`
* Improve printing error for multiple studies matching (see #71, @robitalec)
* Make it possible to find unique with tolerances in `mt_unique`
* Assigning in grouped `move2` retains class
* Remove ASU units as they are not recognized by `units` (#67)
* Fix error in matching data when order of tracks differed in `mt_as_track_data` (#73)
* make `setOldClass` inherit `sf` so S4 methods like `mapView` work
* `.keep` in `mt_as_event_attribute` incorrectly did not add column (#74)
* Using `"merge_list"` results in list columns in `track_data` with `mt_stack`, "merge" now returns unique values (#72, Bruno Caneco)
* Update vocabulary (note speed accuracy had incorrect units)
* Update vocabulary to version released in January 2024 (note speed accuracy had incorrect units, #69)
* When `mt_set_track_id` now returns a list column the lists are unnamed
* Fix `mt_has_unique_location_time_records` failing with `int64` `POSIXct` combination (#79)

# move2 0.2.7

* Add "transmission-end" & "dead/fall-off" to deployment end type to read data correctly
* `mt_interpolate` works when column named `x` or `time` is present
* `mt_interpolate` works when timezone of `time` is different

# move2 0.2.6

* `bind_cols` and `st_join` retains class for `tbl` by fixing reconstruct function
* `st_join` works for data.frame based `move2`

# move2 0.2.4

This release was not published on CRAN

* Update vocabulary handling with new movebank vocabulary
* Correct reading of `migration_stage_standard`
* Prevent warning when download does not contain location data (#63)
* Allow dates for `timestamp_start` and `timestamp_end`
* Resolve `group_by_track_data` assumed a fixed track id column name
* Add `track_id_column` and `time_column` to printing
* Optionally set track id with track data column, error if both are present (#59)
* Error when new track identifier contains duplicated rows in track data (#58)
* Add argument `.keep` to `mt_as_track_attribute` and `mt_as_event_attribute` 
* `st_crop` and `st_intersection` from sf now retain attributes reported by @mscacco
* Prevent error with `mt_stack` on single object by forcing units to be double
* Convert integer `track_id` to factor when stacking (#50, partially)
* Make sure factor levels are ordered when converting to move (#40)
* Add `mt_aeqd_crs` to calculated centered crs (#42)
* `mt_segments` now also works for character track id columns (#51)
* `mt_as_event_attribute` now not only removes first column (#45)
* Make it possible to set units in calculation functions (#47)
* In `mt_as_move2` assert time validity on creation (#46)
* Support converting from ctmm `telemetry` and `track_xyt` thanks to @anneks (#53 & #54)
* Implement `rowwise` function (#62)

# move2 0.2.2

* Improve error reporting time ordered
* Improve error reporting when searching `id` with study name
* Ensure right column is made into `sf_column` when data contains multiple spatial columns (potential problems with 
    argos gps data)
* Improve message when no data is available for download
* Leading and trailing white spaces are not trimmed in data downloaded from Movebank (#38)
* Use a underscore as a separator in `mt_read` when pasting tag and individual names (#37)
* `mt_set_track_id` retains class of track data (#37)

# move2 0.2.0

* Initial CRAN release
* Added a `NEWS.md` file to track changes to the package
