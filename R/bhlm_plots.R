#' @include bhlm_classes.R
#'
#' @importFrom groupdata2 group
#' @import polspline

NULL

# Trace plots function --------------------------------------------------------

#' Show trace plots for outcomes
#'
#' @description Plot outcomes with object from \code{bhlm}.
#' @author Hugh Benjamin Zachariae
#'
#'
#' @param bhlm_object Object returned from \code{bhlm}, of class \code{bhlm_object}.
#' @param outcome_options Character vector specifying which outcomes should be plotted.
#'   Defaults to all outcome options from \code{bhlm_object@outcome_options}.
#' @param return_plots Return ggplot objects in \code{list}.
#'
#' @export
bhlm.traceplot <- function(bhlm_object, outcome_options = NULL, return_plots = FALSE) {

  if (class(bhlm_object) != "bhlm_object") {
    stop(paste("Not bhlm_object. Found ", class(bhlm_object), ".", sep = ""), call. = FALSE)
  }

  if (is.null(outcome_options)) {
    outcome_options <- bhlm_object@outcome_options
  }

  bugs_output <- bhlm_object@jags_samples$BUGSoutput

  iterations <- seq((bugs_output$n.burnin +
                       bugs_output$n.thin),
                    bugs_output$n.iter,
                    bugs_output$n.thin)

  if (length(iterations) < (length(bugs_output$sims.list$deviance)/bugs_output$n.chains)) {
    warning("Added iteration n.burnin.", call. = FALSE)
    iterations <- c(bugs_output$n.burnin, iterations)
  }

  outcome_sims <-
    as.data.frame(bugs_output$sims.matrix[,outcome_options]) %>%
    cbind(iterations) %>%
    group(., bugs_output$n.chains, col_name = "chains")

  plots <- lapply(outcome_options, plot.outcome.trace,
                  chains = bugs_output$n.chains,
                  plotdata = outcome_sims,
                  thin = bugs_output$n.thin,
                  summary = bugs_output$summary)

  lapply(plots, print)

  if (return_plots) {
    return(plots)
  }

}

# Posterior plots function ----------------------------------------------------

#' Savage-Dickey plots for outcomes
#'
#' @description Plot outcomes posterior and prior distributions with object from \code{bhlm}.
#'
#' @author Hugh Benjamin Zachariae
#'
#'
#' @param bhlm_object Object returned from \code{bhlm}, of class \code{bhlm_object}.
#' @param null_hypothesis \code{int}, point at which to check the Bayes Factor.
#' @param outcome_priors_data \code{data.frame} with variables for each outcome (\strong{Important}:
#' variable names need to be the same as chosen outcome names.). Sample from the prior distribution with ex. \code{rnorm(10000, 0, 1)}.
#'
#' If not set, automatically samples from the prior distributions (\code{bhlm_object@outcome_priors_c} or \code{bhlm_object@outcome_priors_m}).
#' Automatic sampling is not yet implemented for priors defined with data vectors.
#' @param outcome_options Choose which outcomes should be plotted. Defaults to \code{bhlm_object@outcome_options}.
#' @param return_plots Return ggplot objects in \code{list}.
#' @param log_estimation Log estimate the posterior and prior distributions for the plot.
#'
#' Non-log estimated plots are not yet fully implemented.
#' @param cum_prob Print cummulated probability (from log estimated posterior distribution) at chosen point along \code{x}
#'
#' (Not yet implemented).
#' @param iter Number of iterations for sampling from prior distribution in automatic sampling.
#'
#' @return \code{list} of outcomes with each element containing a plot and log estimation of the posterior distribution. T
#' he simulation data is also listed as "data".
#'
#' Get plot:  \code{list$outcome$plot}
#'
#' Get Log estimate:  \code{list$outcome$log_est}
#'
#' @export
bhlm.SDplots <- function(bhlm_object,
                         null_hypothesis,
                         outcome_priors_data = NULL,
                         outcome_options = NULL,
                         return_plots = FALSE,
                         log_estimation = TRUE,
                         cum_prob = NULL,
                         iter = 10000) {

  if (class(bhlm_object) != "bhlm_object") {
    stop(paste("No bhlm_object. Found ", class(bhlm_object), ".", sep = ""), call. = FALSE)
  }

  if (is.null(outcome_options)) {
    outcome_options <- bhlm_object@outcome_options
  }

  bugs_output <- bhlm_object@jags_samples$BUGSoutput

  # Data ----------------------------------------------------------------------

  postprior_data <- as.data.frame(bugs_output$sims.matrix[,outcome_options]) %>%
    gather("outcome", "sim", factor_key = TRUE) %>%
    mutate("postprior" = as.factor("Posterior"))

  ## Automated prior sampling -------------------------------------------------

  if (is.null(outcome_priors_data)) {
    if (bhlm_object@outcome_priors_c[1] == "NULL") {
      for(i in 1:length(outcome_options)) {
        postprior_data <- rbind(postprior_data,
                                sample.prior.dist.m(bhlm_object@outcome_priors_m[i,], iter))
      }
    } else {
      for(i in 1:length(outcome_options)) {
        postprior_data <- rbind(postprior_data,
                                sample.prior.dist.c(bhlm_object@outcome_priors_c[i], iter))
      }
    }

  ## Manual prior data.frame incorporation ------------------------------------

  } else {
    if (length(outcome_priors_data) != length(levels(postprior_data$outcome))) {
      stop(paste("Incorrect number of priors defined. Found ",
                 length(outcome_priors_data), ", requires ",
                 length(levels(postprior_data$outcome)),
                 sep = ""),
           call. = FALSE)
    }
    if (!all(colnames(outcome_priors_data) == outcome_options)) {
      stop(paste("Incorrect naming of one or more priors. Found \n  ",
                 colnames(outcome_priors_data),
                 ", requires ", outcome_options, sep = ""),
           call. = FALSE)
    }

    postprior_data <- rbind(postprior_data,
                            outcome_priors_data %>%
                              gather("outcome", "sim", factor_key = TRUE) %>%
                              mutate("postprior" = as.factor("Prior"))
                            )
  }

  # Returns ------------------------------------------------------------------

  if (log_estimation) {
    plots <- lapply(outcome_options, plot.outcome.sd.log,
                    plotdata = postprior_data,
                    null_hypothesis = null_hypothesis,
                    estimate = bhlm_object@estimate_name)
    names(plots) <- outcome_options

    plots$data <- postprior_data
  } else {
    warning("Plot without log estimation is not yet implemented and simply plots the two sampling distributions.")
    plots <- lapply(outcome_options, plot.outcome.sd,
                    plotdata = postprior_data)
  }

  lapply(outcome_options, function(x) print(plots[[x]][["plot"]]))

  if (return_plots) {
    return(plots)
  }

}
