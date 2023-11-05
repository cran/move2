st_intersection.move2 <- function(x, y, ...) {
  x.new <- NextMethod()
  dplyr_reconstruct.move2(x.new, x)
}

st_join.move2 <- function(x, y, join, ...) {
  if (inherits(x, "tbl")) {
    return(NextMethod())
  }
  res <- NextMethod()
  res <- dplyr_reconstruct.move2(res, x)
  return(res)
}
