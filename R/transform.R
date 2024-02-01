#' Transform line list data
#'
#' @details
#' The use of non-syntactic variable name for the first argument is because we
#' use the [transform()] generic from base R.
#'
#' @param `_data` A `<linelist>` object (sub class of `<data.frame>`)
#' output from [import()].
#' @param ... [dots] Extra arguments to be passed to specific methods.
#'
#' @return An \R object
#' @export
transform.linelist <- function(`_data`, transformation, ...) {

  if (transformation == "aggregate") {
    args <- list(
      date_index = c("date_onset", "date_death"),
      interval = "daily"
    )

    args <- modifyList(args, list(...))

    incidence <- incidence2::incidence(
      x = `_data`,
      date_index = args$date_index,
      interval = args$interval,
      ...
    )

    return(incidence)
  }

  if (transformation == "secondary_contacts") {
    second_contacts <- table(`_data`$contacts$from, `_data`$contacts$was_case)
    second_contacts_infect <- second_contacts[, "Y"]
    class(second_contacts_infect) <- c(
      "secondary_contacts", class(second_contacts_infect)
    )
    return(second_contacts_infect)
  }

  return(0)
}
