#' @include bhlm_classes.R
#'
#' @importFrom groupdata2 group

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

  bugs_output = bhlm_object@jags_samples$BUGSoutput

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
                  thin = bugs_output$n.thin)

  lapply(plots, print)

  if (return_plots) {
    return(plots)
  }

}

# Posterior plots function ----------------------------------------------------

#' Show posterior distribution plots for outcomes
#'
#' @description Plot outcomes with object from \code{bhlm}.
#' @author Hugh Benjamin Zachariae
#'
#'
#' @param bhlm_object Object returned from \code{bhlm}, of class \code{bhlm_object}.
#' @param return_plots Return ggplot objects in \code{list}.
#'
#' @export
bhlm.posteriorplot <- function(bhlm_object,
                               return_plots = FALSE,
                               sample = FALSE,
                               log.estimation = FALSE,
                               bayes.factors = TRUE) {

  if (class(bhlm_object) != "bhlm_object") {
    stop(paste("Not bhlm_object. Found ", class(bhlm_object), ".", sep = ""), call. = FALSE)
  }

  if (sample) {
    posteriors <- jags.samples(bhlm_object@jags_samples$model)
  }

  bugs_output = bhlm_object@jags_samples$BUGSoutput



  if (return_plots) {
    return(plots)
  }

}
