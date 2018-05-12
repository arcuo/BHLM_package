#' @include bhlm_classes.R
#'
#' @importFrom groupdata2 group

NULL

#' Show trace plots
#'
#' @description Plot outcomes with object from \code{bhlm}.
#' @author Hugh Benjamin Zachariae
#'
#'
#' @param bhlm_object Object returned from \code{bhlm}, of class \code{bhlm_object}.
#' @param return_plots Return ggplot objects in \code{list}.
#'
#' @export
bhlm.traceplot <- function(bhlm_object, return_plots = FALSE) {

  if (class(bhlm_object) != "bhlm_object") {
    stop(paste("Not bhlm_object. Found ", class(bhlm_object), ".", sep = ""), call. = FALSE)
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
    as.data.frame(bugs_output$sims.matrix[,bhlm_object@outcome_options]) %>%
    cbind(iterations) %>%
    group(., bugs_output$n.chains, col_name = "chains")

  plots <- lapply(bhlm_object@outcome_options, plot.outcome.trace, chains = bugs_output$n.chains, plotdata = outcome_sims)

  lapply(plots, print)

  if (return_plots) {
    return(plots)
  }

}
