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

#' Posterior distribution plots for outcomes
#'
#' @description Plot outcomes with object from \code{bhlm}.
#' @author Hugh Benjamin Zachariae
#'
#'
#' @param bhlm_object Object returned from \code{bhlm}, of class \code{bhlm_object}.
#' @param outcome_priors_data \code{data.frame} with variables for each outcome (\strong{Important}:
#' variable names need to be the same as chosen outcome names.). Sample from the prior distribution with ex. \code{rnorm(10000, 0, 1)}.
#'
#' If not set, automatically samples from the prior distributions (\code{bhlm_object@outcome_priors_c} or \code{bhlm_object@outcome_priors_m}).
#' Automatic sampling is not yet implemented for priors defined with data vectors.
#' @param outcome_options Choose which outcomes should be plotted. Defaults to \code{bhlm_object@outcome_options}.
#' @param return_plots Return ggplot objects in \code{list}.
#' @param log_estimation Log estimate the posterior and prior distributions for the plot (Not yet implemented).
#' @param bayes_factors Print Bayes factors (Not yet implemented).
#' @param cum_prob Print cummulated probability (from log estimated posterior distribution) at chosen point along \code{x} (Not yet implemented).
#' @param iter Number of iterations for sampling from prior distribution in automatic sampling.
#'
#' @export
bhlm.SDplots <- function(bhlm_object,
                               outcome_priors_data = NULL,
                               outcome_options = NULL,
                               return_plots = FALSE,
                               log_estimation = FALSE,
                               bayes_factors = TRUE,
                               cum_prob = NULL,
                               iter = 10000) {

  if (class(bhlm_object) != "bhlm_object") {
    stop(paste("No bhlm_object. Found ", class(bhlm_object), ".", sep = ""), call. = FALSE)
  }

  if (is.null(outcome_options)) {
    outcome_options <- bhlm_object@outcome_options
  }

  bugs_output <- bhlm_object@jags_samples$BUGSoutput

  ## Data ---------------------------------------------------------------------

  postprior_data <- as.data.frame(bugs_output$sims.matrix[,outcome_options]) %>%
    gather("outcome", "sim", factor_key = TRUE) %>%
    mutate("postprior" = as.factor("post"))

  if (is.null(outcome_priors_data)) {
    if (bhlm_object@outcome_priors_c[1] == "NULL") {
      for(i in 1:length(outcome_options)) {
        postprior_data <- rbind(postprior_data,
                                sample.prior.dist(bhlm_object@outcome_priors_m[i,], iter))
      }
    } else {
      stop(paste("Automatic prior sampling from character vector is not yet implemented.",
                 "\n  Create a data.frame with priors for each outcome (columns).",
                 "\n  Example:",
                 "\n    data.frame('outcome1' = rnorm(10000, ...), ...)",
                 "\n  Make sure that outcome names are correct.", sep = ""),
           call. = FALSE)
    }
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
                              mutate("postprior" = as.factor("prior"))
                            )
  }

  ## Returns ------------------------------------------------------------------

  if (return_plots) {

    return(lapply(outcome_options, plot.outcome.post, plotdata = postprior_data))

  } else {

    print(ggplot(postprior_data, aes(x = sim, fill = postprior))+
      geom_density(alpha = 0.5) +
      labs(y = "Density", x = "Estimate", fill = "Posterior/Prior") +
      facet_wrap(~outcome, scales = "free_x", ncol=2) +
      theme_bw())

  }

}
