#' @export
is_epi_linelist <- function(x, ...) {
  inherits(x, what = "epi_linelist", ...)
}

#' @export
is_epi_contacts <- function(x, ...) {
  inherits(x, what = "epi_contacts", ...)
}

#' @export
is_epi_outbreak <- function(x, ...) {
  inherits(x, what = "epi_outbreak", ...)
}

#' @export
is_epi_incidence <- function(x, ...) {
  inherits(x, what = "epi_incidence", ...)
}

#' @export
is_epi_secondary_contacts <- function(x, ...) {
  inherits(x, what = "epi_secondary_contacts", ...)
}

#' @export
is_epi_offspring_dist <- function(x, ...) {
  inherits(x, what = "epi_offspring_dist", ...)
}
