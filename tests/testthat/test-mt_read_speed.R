test_that("Files read quick enough", {
  skip_if_offline()
  skip_on_cran()
  skip_on_covr()
  if (file.exists("~/ownCloudUva/Galapagos Albatrosses.csv")) {
    file <- "~/ownCloudUva/Galapagos Albatrosses.csv"
    ref_speed <- 1.5
  } else {
    file <- tempfile(fileext = ".csv")
    ref_speed <- 2
    download.file("https://surfdrive.surf.nl/files/index.php/s/RJp7FcbmSjb6qwZ/download", file, quiet = TRUE)
  }
  expect_lt(system.time(mt_read(file))["elapsed"], ref_speed)
})
