#' @export
analyse <- function(x, ...) {
  UseMethod("analyse")
}

#' @export
analyse.incidence2 <- function(x, analysis, ...) {

  # TODO: S4 multiple dispatch to solve if ladder

  if (analysis == "severity") {
    cfr_tbl <- cfr::prepare_data(
      data = x,
      cases_variable = "date_onset",
      deaths_variable = "date_death"
    )
    severity <- cfr::cfr_static(data = cfr_tbl)
    return(severity)
  }

  return(0)
}

#' @export
analyse.secondary_contacts <- function(x, analysis, ...) {
  if (analysis == "offspring_dist") {
    offspring <- fitdistrplus::fitdist(data = unclass(x), distr = "nbinom")
    names(offspring$estimate) <- c("k", "R")
    offspring <- offspring$estimate[c("R", "k")]
    class(offspring) <- c("offspring_dist", class(offspring))
    return(offspring)
  }

  return(0)

}

#' @export
analyse.offspring_dist <- function(x, analysis, ...) {
  if (analysis == "probability_epidemic") {
    prob_epidemic <- superspreading::probability_epidemic(
      R = x[["R"]], k = x[["k"]], ...
    )
    return(prob_epidemic)
  } else if (analysis == "probability_extinct") {
    prob_extinct <- superspreading::probability_extinct(
      R = x[["R"]], k = x[["k"]], ...
    )
    return(prob_extinct)
  } else if (analysis == "final_size") {
    args <- list(
      contact_matrix = matrix(1.0) / 6.7e7,
      demography_vector = 6.7e7,
      susceptibility = matrix(1.0),
      p_susceptibility = matrix(1.0)
    )

    args <- modifyList(args, list(...))

    final_size <- finalsize::final_size(
      r0 = x[["R"]],
      contact_matrix = args$contact_matrix,
      demography_vector = args$demography_vector,
      susceptibility = args$susceptibility,
      p_susceptibility = args$p_susceptibility
    )
    return(final_size)
  }


  return(0)
}

#' @export
analyse.chain <- function(x, analysis, ...) {

}
