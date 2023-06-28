#' @export
plot.move2 <- function(x, y, ...) {
  class(x) <- setdiff(class(x), "move2")
  NextMethod()
}
