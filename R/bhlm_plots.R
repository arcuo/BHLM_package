#' @include bhlm_classes.R
#'
#'

#' Show trace plots
#'
bhlm.traceplot <- function(bhlm_object, save_plots_to_folder = NULL) {

  # Create outcome sim objects ------------------------------------------------

  bugs_output = bhlm_object@jags_samples$BUGSoutput

  iterations <- seq((bugs_output$n.burnin +
                       bugs_output$n.thin),
                    bugs_output$n.iter,
                    bugs_output$n.thin)

  outcome_sims = lapply(bhlm_object@outcome_options, function(out) {
    as.data.frame(bugs_output$sims.array[, , out])

  })

  return(outcome_sims)


  # ---------------------------------------------------------------------------
  iterations <- seq((bhlm_object@jags_samples$BUGSoutput$n.burnin +
                       bhlm_object@jags_samples$BUGSoutput$n.thin),
                    bhlm_object@jags_samples$BUGSoutput$n.iter,
                    by = bhlm_object@jags_samples$BUGSoutput$n.thin)


  psidata = as.data.frame(list("iter" = iter,
                               "chain1" = ar[, 1, "psi"],
                               "chain2" = ar[, 2, "psi"],
                               "chain3" = ar[, 3, "psi"])) %>%
    gather("chain", "sim", chain1, chain2, chain3)

  ggplot(psidata, aes(x = iter, y = sim, color = chain)) + geom_line(alpha = 0.9) +
    scale_color_brewer(palette = "Set1") +
    theme_bw() +
    ggtitle("Psi")




}
