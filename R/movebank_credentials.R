mt_internal_get_password <- function(
    msg = "Please provide your movebank password") {
  check_installed("askpass", "to enable inputting a password.")
  askpass::askpass(msg)
}


#' Store or remove credentials in the system keyring
#'
#' @param username A string with the movebank username
#'
#' @param password Either a string or missing, if missing then the password is asked for using
#' \code{\link[askpass]{askpass}}
#' @param key_name The name of the key in the keyring. By default this is stored in
#' `getOption("move2_movebank_key_name")`, this might be useful if you have multiple accounts
#' @param force If TRUE, when accessing movebank fails the key is stored anyway.
#'
#' @description
#' The function stores the credentials for accessing movebank, by default it checks if accessing movebank is possible,
#' and fails when either the credentials are invalid or movebank cannot be reached. The force option can be used to
#' override this. Once credentials are stored, these functions are not needed again as all call to movebank can use the
#' credentials from the keyring.
#'
#' For more details on the usage of the keyring, how passwords are handled and handling multiple accounts see
#' `vignette("movebank", package="move2")`
#'
#' @return `TRUE` invisible if successful
#' @family movebank-download
#' @export
#' @examples
#' \dontrun{
#' movebank_store_credentials("bart")
#' }
movebank_store_credentials <- function(
    username, password,
    key_name = getOption("move2_movebank_key_name"),
    force = FALSE) {
  check_installed(c("keyring"), "to store credentials in the keyring.")
  if (missing(username)) {
    cli_abort(
      class = "move2_error_no_username",
      c("No username specified. ",
        i = "Please call the {.fun movebank_store_credentials} function with the {.arg username} argument specified"
      )
    )
  }
  assert_that(is.string(username))
  if (missing(password)) {
    password <- mt_internal_get_password()
  }
  assert_that(is.string(password))
  assert_that(is.string(key_name))

  if (!force) {
    curl_handle <- movebank_handle(username, password = password)
    res <- catch_cnd(movebank_retrieve(
      entity_type = "tag_type",
      handle = curl_handle
    ))
    if (!is.null(res)) {
      cli_abort(
        "The provided {.arg username} and {.arg password} combination fail.",
        class = "move2_error_multiple_keys",
        body = format_error_bullets(c(
          x = "`mt_store_credential` was not able to succesfully conduct a test query to movebank with the `username`
          and `password`. This might be due to problems with the credentials but can also be the result of a failure
          to contact movebank.",
          i = "Please check the `username` and `password`, alternatively you can force storing the credentials using
          force=TRUE"
        ))
      )
    }
  }
  keyring::key_set_with_value(
    username = username, password = password,
    service = key_name, keyring = getOption("move2_movebank_keyring")
  )
  return(invisible(TRUE))
}
#' @export
#' @rdname movebank_store_credentials
movebank_remove_credentials <- function(key_name = getOption("move2_movebank_key_name")) {
  check_installed("keyring", format_message("for {.fun movebank_remove_credentials}."))
  list_of_keys <- keyring::key_list(service = key_name)
  if (nrow(list_of_keys) == 0L) {
    cli_warn("There are no keys to remove from the keyring.", class = "move2_warn_no_keys_to_remove")
    return(invisible(TRUE))
  }
  mapply(keyring::key_delete,
    service = list_of_keys$service,
    username = list_of_keys$username,
    MoreArgs = list(keyring = getOption("move2_movebank_keyring"))
  )
  cli_inform("There {?is/are} {nrow(list_of_keys)} key{?s} removed from the keyring.",
    class = "move2_inform_keys_removed"
  )
  return(invisible(TRUE))
}
