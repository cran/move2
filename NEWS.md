# move2 0.2.6

* `bind_cols` and `st_join` retains class for `tbl` by fixing reconstruct function
* `st_join` works for data.frame based `move2`

# move2 0.2.4

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
