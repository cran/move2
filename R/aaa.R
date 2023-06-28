#' @importFrom methods setOldClass
NULL
# nocov start
.onLoad <- function(libname, pkgname) { # nolint
  options(
    move2_movebank_key_name = "movebank",
    move2_movebank_keyring = NULL,
    move2_movebank_api_url =
      "https://www.movebank.org/movebank/service/direct-read"
  )

  invisible()
  register_all_s3_methods()
  # dynamically registers non-imported pkgs (tidyverse)
}
setOldClass("move2")

# nocov end
