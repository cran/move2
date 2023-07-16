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
