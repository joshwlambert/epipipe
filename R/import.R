#' @export
import <- function(x, ...) {
  UseMethod("import")
}

#' @export
import.character <- function(x, ...) {
  .data <- rio::import(file = x, ...)
  class(.data) <- c("epi_linelist", class(.data))
  return(.data)
}

#' @export
import.function <- function(x, ...) {

  closure <- as.character(substitute(x))
  stopifnot(
    "function required in namespace::function format" =
      length(closure) == 3 && closure[1] == "::"
  )
  pkg_name <- closure[2]
  func_name <- closure[3]
  func <- get(func_name, asNamespace(pkg_name))

  if (pkg_name != "simulist") {
    stop(
      "Currently only the {simulist} package is accepted to simulate data",
      call. = FALSE
    )
  }

  .data <- do.call(func, list(...))

  subclass <- switch(func_name,
    sim_linelist = "epi_linelist",
    sim_contacts = "epi_contacts",
    sim_outbreak = "epi_outbreak"
  )
  class(.data) <- c(subclass, class(.data))

  return(.data)
}
