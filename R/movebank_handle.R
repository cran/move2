#' Create a curl handle for accessing movebank
#'
#' @param username Optionally a username as a character string
#'
#' @param password Optionally a password as a character string
#'
#' @details
#'
#' If no credentials are provided the function tries to retrieve the username and password from the system
#' keyring using the keyring package. If a username is provided but no password it is requested using
#' \code{\link[askpass]{askpass}}
#'
#' @return A \code{\link[curl]{handle}} that can be added to a request made with \code{\link[curl]{curl}}
#'
#' @family movebank-download
#' @export
#' @examples
#' movebank_handle("test_user", "test_password")
movebank_handle <- function(username = NULL, password = NULL) {
  if (is.null(username) && is.null(password)) {
    check_installed(c("curl", "keyring"), "to create a movebank handle from the keyring.")
    list_of_keys <- keyring::key_list(
      getOption("move2_movebank_key_name"),
      keyring = getOption("move2_movebank_keyring")
    )
    if (nrow(list_of_keys) > 1) {
      cli_abort(
        class = "move2_error_multiple_keys",
        c("Multiple possible keys for movebank found",
          x = 'There are {nrow(list_of_keys)} key{?s} found with the service name
          "{getOption("move2_movebank_key_name")}".',
          i = "Delete the extra keys so only one correct one remains. This can be done using either
          {.code keyring::key_delete()}, {.code movebank_remove_credentials()} or in the password manager of your
          operating system."
        )
      )
    }
    if (nrow(list_of_keys) < 1) {
      cli_abort(
        class = "move2_error_no_key",
        c("No login for movebank found",
          i = "Your password can be stored with the keyring package with `movebank_store_credentials`",
          i = "Replace your username and enter your password when prompted, keys are stored in the system keyring."
        )
      )
    }
    userpwd <- paste0(
      list_of_keys$username, ":",
      keyring::key_get(list_of_keys$service, list_of_keys$username,
        keyring = getOption("move2_movebank_keyring")
      )
    )
  } else {
    check_installed("curl", "to create a movebank handle.")

    assert_that(!is.null(username))
    if (is.null(password)) {
      password <- mt_internal_get_password()
    }
    assert_that(is.string(password))
    userpwd <- paste0(username, ":", password)
  }
  curl_handle <- curl::new_handle()

  curl::handle_setopt(
    handle = curl_handle,
    httpauth = 1,
    userpwd = userpwd
  )
  return(curl_handle)
}
