test_that("Download speeds are as expected", {
  skip_if_offline()
  skip_on_cran()
  skip_on_covr()
  skip_if_no_mbpwd()
  withr::with_envvar(
    list("movebank:move2_user" = Sys.getenv("MBPWD")),
    withr::with_options(list(keyring_backend = "env"), {
      show_failure(expect_lt(system.time(
        suppressMessages(aa <- movebank_download_study(1259686571L, timestamp_end = "2023-1-1", attributes = "all"))
      )["elapsed"], 78L))
      expect_true(all(c(
        "taxon_canonical_name", "tag_id", "individual_id",
        "deployment_id", "study_id"
      ) %in% colnames(mt_track_data(aa))))
      expect_false(any(c(
        "individual_taxon_canonical_name", "taxon_canonical_name",
        "tag_id", "individual_id", "study_id", "deployment_id"
      ) %in% colnames(aa)))

      show_failure(expect_lt(
        system.time(
          a <- movebank_download_study(2911040L, attributes = NULL)
        )["elapsed"],
        9.0
      ))
      expect_identical(a |> mt_time() |> lubridate::tz(), "UTC")
      expect_true(all(c(
        "taxon_canonical_name", "tag_id", "individual_id",
        "deployment_id", "study_id"
      ) %in% colnames(mt_track_data(a))))
      expect_false(any(c(
        "individual_taxon_canonical_name",
        "taxon_canonical_name", "tag_id", "individual_id",
        "study_id", "individual_local_identifier"
      ) %in% colnames(a)))
      show_failure(expect_lt(system.time(
        movebank_retrieve(entity_type = "study", license_type = "CC_0")
      )["elapsed"], 2.0))
    })
  )
})
test_that("Download speeds for deployment", {
  skip_if_offline()
  skip_on_cran()
  skip_on_covr()
  skip_if_no_mbpwd()
  withr::with_envvar(
    list("movebank:move2_user" = Sys.getenv("MBPWD")),
    withr::with_options(list(keyring_backend = "env"), {
      show_failure(expect_lt((
        a <- system.time(movebank_download_deployment(1259686571L)))["elapsed"], 4.0))
      expect_lt(summary(a)["system"], 0.5)
      show_failure(expect_lt((
        b <- system.time(movebank_download_deployment(2911040L)))["elapsed"], 4.0))
      expect_lt(summary(b)["system"], 0.5)
    })
  )
})
