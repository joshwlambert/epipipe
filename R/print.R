#' @export
print.epi_offspring_dist <- function(x, ...) {
  x <- unclass(x)
  NextMethod()
}
