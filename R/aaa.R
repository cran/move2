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
  # dynamically registers non-imported pkgs (tidyverse)
  s3_register("dplyr::dplyr_reconstruct", "move2")
  s3_register("dplyr::filter", "move2")
  s3_register("dplyr::arrange", "move2")
  s3_register("dplyr::group_by", "move2")
  s3_register("dplyr::ungroup", "move2")
  s3_register("dplyr::rowwise", "move2")
  s3_register("dplyr::group_split", "move2")
  s3_register("dplyr::mutate", "move2")
  s3_register("dplyr::slice", "move2")
  s3_register("dplyr::select", "move2")
  s3_register("dplyr::left_join", "move2")
  s3_register("sf::st_intersection", "move2")
  s3_register("sf::st_join", "move2")
}
setOldClass(c("move2", "sf"))

# nocov end
