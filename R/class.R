#' `<epi_linelist>` constructor
#'
#' @slot linelist A line list `<data.frame>`.
#'
#' @return An `<epi_linelist>` object.
#' @export
epi_linelist <- new_class("epi_linelist",
  properties = list(
    linelist = class_data.frame
  ),
  validator = function(self) {
    if (1 == 2) {
      "@linelist must contain at least one ID column"
    }
  },
  package = "epipipe"
)

epi_outbreak <- new_class(
  "epi_outbreak",
  properties = list(
    linelist = class_data.frame,
    contacts = class_data.frame
  ),
  validator = function(self) {
    if (1 == 2) {
      "@linelist must contain at least one ID column"
    }
  },
  package = "epipipe"
)

#' transform generic function
#'
#' @param linelist A line list `<data.frame>`.
#'
#' @return An `<epi_linelist>` object.
#' @export
transform <- new_generic(name = "transform", "x")

# ll <- epi_linelist(linelist = data.frame(a = 1))
#
# ll@linelist
# ll@linelist <- data.frame(a = 1, b = 2)
# ll
#
# class(ll)
# S7_class(ll)
#
# outbreak <- epi_outbreak(linelist = data.frame(a = 1), contacts = data.frame(b = 2))
# outbreak
#
# outbreak@linelist
# outbreak@contacts
# class(outbreak)
# S7_class(outbreak)
#
#
#

#
# method(transform, epi_linelist) <- function(x, ...) {
#   as.matrix(x@linelist)
# }
#
# method(transform, epi_outbreak) <- function(x, ...) {
#   x@contacts <- data.frame(contacts = 10)
#   x
# }
#
# transform
#
# transform(outbreak)
#
#
# method(convert, list(foo1, class_integer)) <- function(from, to) {
#   from@x
# }
# convert()
