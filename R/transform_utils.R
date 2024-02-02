#' @export
as_epi_incidence <- function(x, ...) {
  UseMethod("as_epi_incidence")
}

#' @export
as_epi_secondary_contacts <- function(x, ...) {
  UseMethod("as_epi_secondary_contacts")
}

#' @export
as_epi_incidence.incidence2 <- function(x, ...) {
  class(x) <- c("epi_incidence", class(x))
  return(x)
}

#' @export
as_epi_incidence.data.frame <- function(x, ...) {
  message(
    "as_epi_linelist() does not check if the <data.frame> is a ",
    "valid line list. \n We recommend the {linelist} package to tag ",
    "and validate case data"
  )
  class(x) <- c("epi_linelist", class(x))
  return(x)
}

#' @export
as_epi_linelist.list <- function(x, ...) {
  stopifnot(
    "list must be a single element containing a line list <data.frame>" =
      length(x) == 1 && is.data.frame(x[[1]])
  )
  class(x) <- c("epi_linelist", class(x))
  return(x)
}

#' @export
as_epi_contacts.list <- function(x, ...) {
  stopifnot(
    "list must be a single element containing a contacts <data.frame>" =
      length(x) == 1 && is.data.frame(x[[1]])
  )
  class(x) <- c("epi_contacts", class(x))
  return(x)
}

#' @export
as_epi_outbreak.list <- function(x, ...) {
  stopifnot(
    "list must be two elements each containing a <data.frame>" =
      length(x) == 2 && all(vapply(x, is.data.frame, FUN.VALUE = logical(1)))
  )
  class(x) <- c("epi_outbreak", class(x))
  return(x)
}

#' @export
as_epi_linelist.linelist <- function(x, ...) {
  linelist::validate_linelist(x, ...)
  class(x) <- c("epi_linelist", class(x))
  return(x)
}

#' @export
as_epi_contacts.linelist <- function(x, ...) {
  stop()
}

#' @export
as_epi_incidence.default <- function(x, ...) {
  stop(
    paste("as_epi_incidence() not implemented for class", class(x)),
    call. = FALSE
  )
}

#' @export
as_epi_secondary_contacts.default <- function(x, ...) {
  stop(
    paste("as_epi_secondary_contacts() not implemented for class", class(x)),
    call. = FALSE
  )
}
