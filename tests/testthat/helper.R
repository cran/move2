skip_if_no_mbpwd <- function() {
  if (!identical(Sys.getenv("MBPWD"), "")) {
    return(invisible(TRUE))
  }

  skip("No env variable MBPWD")
}
