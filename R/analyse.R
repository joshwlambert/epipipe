#' @export
analyse <- function(x, ...) {
  UseMethod("analyse")
}

#' @export
analyse.epi_incidence <- function(x,
                                  analysis = c("severity",
                                               "growth_rate",
                                               "rolling_average"),
                                  ...) {
  analysis <- match.arg(analysis)

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

  if (analysis == "growth_rate") {
    fit <- i2extras::fit_curve(
      x,
      model = "negbin",
      alpha = 0.05
    )
    growth_rate <- i2extras::growth_rate(fit)
    return(growth_rate)
  }

  if (analysis == "rolling_average") {
    rolling_average <- i2extras::add_rolling_average(x, n = 7)
    return(rolling_average)
  }

  return(0)
}

#' @export
analyse.epi_secondary_contacts <- function(x, analysis = c("offspring_dist"), ...) {

  analysis <- match.arg(analysis)

  if (analysis == "offspring_dist") {
    offspring <- fitdistrplus::fitdist(data = unclass(x), distr = "nbinom")
    names(offspring$estimate) <- c("k", "R")
    offspring <- offspring$estimate[c("R", "k")]
    class(offspring) <- c("epi_offspring_dist", class(offspring))
    return(offspring)
  }

  return(0)

}

#' @export
analyse.epi_offspring_dist <- function(x, analysis, ...) {

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
analyse.epi_growth_rate <- function(x,
                                analysis = c("doubling_time", "reproduction_number"),
                                ...) {

  if (analysis == "doubling_time") {
    # $$ T = \frac{\log(2)}{r},$$
  }

  if (analysis == "reproduction_number") {
    R_mean <- epitrix::r2R0(x$r, density(serial_interval, 1:1000))
    R_lower <- epitrix::r2R0(x$r_lower, density(serial_interval, 1:1000))
    R_upper <- epitrix::r2R0(x$r_upper, density(serial_interval, 1:1000))
  }
}

#' @export
analyse.chain <- function(x, analysis, ...) {

}


