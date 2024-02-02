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
transform.epi_linelist <- function(`_data`, transformation = c("aggregate"), ...) {

  transformation <- match.arg(transformation)

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

    class(incidence) <- c("epi_incidence", class(incidence))

    return(incidence)
  }

  if (transformation == "secondary_contacts") {
    second_contacts <- table(`_data`$contacts$from, `_data`$contacts$was_case)
    second_contacts_infect <- second_contacts[, "Y"]
    class(second_contacts_infect) <- c(
      "epi_secondary_contacts", class(second_contacts_infect)
    )
    return(second_contacts_infect)
  }

  return(0)
}

#' @export
transform.epi_contacts <- function(`_data`, transformation = c("secondary_contacts"), ...) {

  transformation <- match.arg(transformation)

  if (transformation == "secondary_contacts") {
    second_contacts <- table(`_data`$contacts$from, `_data`$contacts$was_case)
    second_contacts_infect <- second_contacts[, "Y"]
    class(second_contacts_infect) <- c(
      "ep_secondary_contacts", class(second_contacts_infect)
    )
    return(second_contacts_infect)
  }

  return(0)
}

#' @export
transform.epi_outbreak <- function(`_data`,
                                   transformation = c("aggregate",
                                                      "secondary_contacts"),
                                   ...) {

  # TODO: maybe add ep_ prefix to class names to not confuse with other classes with same name

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

    class(incidence) <- c("epi_incidence", class(incidence))

    return(incidence)
  }

  if (transformation == "secondary_contacts") {
    second_contacts <- table(`_data`$contacts$from, `_data`$contacts$was_case)
    second_contacts_infect <- second_contacts[, "Y"]
    class(second_contacts_infect) <- c(
      "epi_secondary_contacts", class(second_contacts_infect)
    )
    return(second_contacts_infect)
  }

  return(0)
}
