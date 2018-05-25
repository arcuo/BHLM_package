#' @include bhlm_classes.R
#'
#' @import tidyverse
NULL

# Latent mixture preprocessing ------------------------------------------------

bhlm.preprocessing <- function (dataframe,
                                grouping_factors_cols,
                                meta_outcome_col,
                                outcome_options_col,
                                outcome_options = c(),
                                identifier_col = NULL) {

  if(purrr::is_empty(outcome_options)) {
    stop("missing outcome options", call. = FALSE)
  } else if(length(grouping_factors_cols) != 2) {
    stop(paste("missing grouping factors (Studies*Outcomes) or too many factors",
         "(currently limited to only 2)"), call. = FALSE)
  } else {

    if (is.null(identifier_col)) {
      useful <- dataframe %>%
      dplyr::select(one_of(grouping_factors_cols),
                    outcomes = !!meta_outcome_col,
                    outcomes_names = !!outcome_options_col) %>%
      .[.[["outcomes_names"]] %in% outcome_options,] %>%
      dplyr::mutate_at(vars(outcomes_names),
                       funs(outcomes_numeric = as.numeric(.)))
    } else {
      useful <- dataframe %>%
        dplyr::select(id = !!identifier_col,
                      one_of(grouping_factors_cols),
                      outcomes = !!meta_outcome_col,
                      outcomes_names = !!outcome_options_col) %>%
        .[.[["outcomes_names"]] %in% outcome_options,] %>%
        dplyr::mutate_at(vars(outcomes_names),
                         funs(outcomes_numeric = as.numeric(.)))
    }

    # Create start bounds (index) for studies loop.

    start_bounds <- useful[grouping_factors_cols[1]] %>%
      duplicated() %>%
      which(x = (. == FALSE)) %>%
      append(., nrow(useful) + 1)

    return(new("bhlm_data",
               used_data = useful,
               start_bounds = start_bounds))
  }
}

# Latent mixture model file helpers -------------------------------------------

# Prior distributions creater -------------------------------------------------

define.prior.dist <- function (dist) {

  if (length(dist) > 1) {
    string <-
      paste(paste("~", dist[1], "(", sep=""),
            paste(tail(dist, length(dist) - 1), collapse = ", "),
            ")", sep = "")
    return(string)
  } else {
    return(paste("~", dist, sep = ""))
  }

}

# Automated sampling of prior distributions (matrix)

sample.prior.dist.m <- function(dist, iter) {

  if (dist[2] == "dnorm") {

    d <- data.frame(
      "temp" = c(iter,
                 as.numeric(dist[3]),
                 1/sqrt(as.numeric(dist[4]))) %>%
        as.list() %>%
        do.call("rnorm", .)
      )

    names(d) <- dist[1]

    d <- d %>%
      gather("outcome", "sim") %>%
      mutate("postprior" = as.factor("Prior"))

    return(d)

  } else {
    stop(paste("outcome_priors_m: Automatic prior distribution plotting is",
               " only implemented for gaussian priors (dnorm)."), call. = FALSE)
  }

}

# Automated sampling of prior distributions (character vector)

sample.prior.dist.c <- function(dist, iter) {

  if (grepl("dnorm", a@outcome_priors_c[1])) {

    args <- stringr::str_extract_all(dist, "\\d(\\.\\d+)?", simplify = T) %>%
      as.numeric()

    d <- data.frame(
      "temp" = c(iter, args[1], 1/sqrt(args[2])) %>%
        as.list() %>%
        do.call("rnorm", .)
    )

    names(d) <- stringr::str_extract(dist, "^.*(?=#)")

    d <- d %>%
      gather("outcome", "sim") %>%
      mutate("postprior" = as.factor("Prior"))

    return(d)

  } else {
    stop(paste("outcome_priors_m: Automatic prior distribution plotting is",
               " only implemented for gaussian priors (dnorm)."), call. = FALSE)
  }

}

# Write outcome priors and references -----------------------------------------

bhlm.make.outcomes <- function (outcomes_list, outcome_priors){

  if (class(outcome_priors) == "matrix") {
    priors <- unlist(lapply(seq(1, length(outcomes_list), 1),
                            function(x) paste("\t", outcomes_list[x],
                                              define.prior.dist(outcome_priors[x,]),
                                              sep="")
                            )
                     )
  } else if (class(outcome_priors) == "character" || class(outcome_priors) == "string" ) {
    priors <- unlist(lapply(seq(1, length(outcomes_list), 1),
                            function(x) paste("\t", outcomes_list[x],
                                              define.prior.dist(outcome_priors[x]),
                                              sep="")
                            )
                     )
  } else {
    stop("Wrong outcome_priors input")
  }

  outcomes_map <- unlist(lapply(seq(1, length(outcomes_list), 1),
                            function(x) paste("\toutcome_options[", x, "] <- ",
                                              outcomes_list[x],
                                              sep="")
                            )
                     )

  return(c(priors, "\t", outcomes_map))
}

# Create the model file list ----------------------------------------------------

bhlm.create.model.list <- function (outcomes_list,
                                    theta_prior,
                                    lambda_prior,
                                    outcome_priors,
                                    field_theta){

  if (is.null(field_theta)) {

    return(c("model{",
             "\t",
             paste("\tlambda", define.prior.dist(lambda_prior), sep = ""),
             "\t",
             bhlm.make.outcomes(outcomes_list, outcome_priors),
             "\t",
             "\tfor (s in 1:upper_group) {",
             "\t\t",
             paste("\t\ttheta[s]", define.prior.dist(theta_prior), sep = ""),
             "\t\t",
             "\t\tfor (o in start_bounds[s]:(start_bounds[s+1]-1)) {",
             "\t\t\t",
             "\t\t\toutcome[s,o] <-  outcome_options[outcomes_numeric[o]]",
             "\t\t\teta[s,o] <- theta[s] + outcome[s,o]",
             "\t\t\toutcomes[o] ~ dnorm(eta[s,o],lambda)",
             "\t\t\t",
             "\t\t}",
             "\t}",
             "}"
    )
    )

  } else

    return(c("model{",
             "\t",
             paste("\tlambda", define.prior.dist(lambda_prior), sep = ""),
             paste("\ttheta_field", define.prior.dist(theta_prior), sep = ""),
             "\t",
             bhlm.make.outcomes(outcomes_list, outcome_priors),
             "\t",
             "\tfor (s in 1:upper_group) {",
             "\t\t",
             paste("\t\ttheta[s]", define.prior.dist(c(field_theta[1], "theta_field", field_theta[2])), sep = ""),
             "\t\t",
             "\t\tfor (o in start_bounds[s]:(start_bounds[s+1]-1)) {",
             "\t\t\t",
             "\t\t\toutcome[s,o] <-  outcome_options[outcomes_numeric[o]]",
             "\t\t\teta[s,o] <- theta[s] + outcome[s,o]",
             "\t\t\toutcomes[o] ~ dnorm(eta[s,o],lambda)",
             "\t\t\t",
             "\t\t}",
             "\t}",
             "}"
            )
           )

  }

# Write to textfile -----------------------------------------------------------

bhlm.write.model <- function (model_file_list, path) {

  if (is.null(path)) {

    connection <- file(paste(getwd(), "model_file.txt", sep = "/"))
    writeLines(model_file_list, connection)
    close(connection)

    message(paste("Model file saved to: ", getwd(), " as 'model_file.txt'", sep=""))

    return(paste(getwd(), "model_file.txt", sep = "/"))

  } else if (is.character(path)) {

    connection <- file(path)
    writeLines(model_file_list, connection)
    close(connection)

    return(path)

  } else if (!path) {

    tmpFile <- tempfile(pattern = "modelFile", tmpdir = tempdir(),  fileext = ".txt")
    connection <- file(tmpFile)
    writeLines(model_file_list, connection)
    close(connection)

    return(tmpFile)

  } else {

    stop(paste("save_model is not valid. Set to either",
               "\n\tFALSE to not save the model file,",
               "\n\tTRUE to save as 'model_file.txt' i working directory,",
               "\n\tor set a manual path string with file name at the",
               "end and '.txt' (with '\\' or '/' separators).",
               sep=" "), call. = FALSE)

  }

}

# Plot helpers ----------------------------------------------------------------

plot.outcome.trace <- function (plotdata, outcome, chains, thin, summary) {

  ggplot(plotdata, aes_string(x = "iterations", y = outcome, color = "chains")) + geom_line(alpha = 0.9) +
    scale_color_brewer(palette = "Set1") +
    labs(title = paste(outcome, " trace plot",
                       "\n\nMean: ", round(summary[outcome, "mean"], 3),
                       "\n Chains: ", chains,
                       "   Thinning rate: ", thin,
                       "\nn.eff: ", summary[outcome, "n.eff"],
                       "   Rhat: ", round(summary[outcome, "Rhat"], 3), sep =""),
         x = "Iterations",
         y = "Simulation estimate",
         color = "Chains") +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))

}

plot.outcome.sd.geom <- function (plotdata, outcome_name) {

  filter(plotdata, outcome == !!outcome_name) %>%
    ggplot(aes(x = sim, fill = postprior)) +
      geom_density(alpha = 0.5) +
      labs(y = "Density", x = outcome_name, fill = "Posterior/Prior") +
      theme_bw()

}

plot.outcome.sd.log <- function (plotdata, outcome_name,
                                 null_hypothesis,
                                 estimate) {

  filtered_data <- filter(plotdata, outcome == !!outcome_name)

  post_log <- filtered_data %>%
    filter(postprior == "Posterior") %>%
    select(sim) %>%
    logspline()

  prior_log <- filtered_data %>%
    filter(postprior == "Prior") %>%
    select(sim) %>%
    logspline()

  u1 <- min(qlogspline(0.01, post_log), qlogspline(0.01, prior_log))
  u2 <- max(qlogspline(0.99, post_log), qlogspline(0.99, prior_log))
  u3 <- 1.1 * u1 - 0.1 * u2
  u4 <- 1.1 * u2 - 0.1 * u1

  plotdat2 <- data.frame(x = (0:(1000 - 1))/(1000 - 1) * (u4 - u3) + u3) %>%
    mutate(Posterior = dlogspline(x, post_log), Prior = dlogspline(x, prior_log)) %>%
    gather("postprior", "estimation", c(Posterior, Prior))

  post_hyp <- dlogspline(null_hypothesis, post_log)
  prior_hyp <- dlogspline(null_hypothesis, prior_log)

  bf01 <- round(prior_hyp/post_hyp, 3)
  bf10 <- round(post_hyp/prior_hyp, 3)

  plot <- ggplot(plotdat2, aes(x, estimation, color = postprior)) + geom_line() +
    scale_color_manual(values = c("chocolate4", "chocolate")) +
    geom_segment(aes(x = null_hypothesis,
                     y = prior_hyp,
                     yend = post_hyp,
                     xend = null_hypothesis),
                 linetype = "dashed",
                 color = "grey10") +
    geom_point(aes(x = null_hypothesis,
                   y = prior_hyp),
               color = "black") +
    geom_point(aes(x = null_hypothesis,
                   y = post_hyp),
               color = "black") +
    labs(title = paste(outcome_name, " Savage-Dickey plot",
                       "\n\nBayes Factor: ", bf01, " / ", bf10,
                       sep =""),
         x = estimate,
         y = "Log estimated density",
         color = "Posterior/Prior") +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))

  return_obj <- vector(mode="list", length = 0)
  return_obj$plot <-  plot
  return_obj$log_est <- post_log

  return(return_obj)

}

plot.outcome.sd.base <- function (plotdata, outcome_name,
                                 null_hypothesis,
                                 estimate) {

  filtered_data <- filter(plotdata, outcome == !!outcome_name)

  post_dens <- filtered_data %>%
    filter(postprior == "Posterior")
  post_dens <- density(post_dens$sim)

  prior_dens <- filtered_data %>%
    filter(postprior == "Prior")
  prior_dens <- density(prior_dens$sim)


  plotdat2 <- data.frame(x = post_dens$x, estimation = post_dens$y, postprior = "Posterior") %>%
    rbind(data.frame(x = prior_dens$x, estimation = prior_dens$y, postprior = "Prior"))

  post_hyp = approx(post_dens$x, post_dens$y, xout = null_hypothesis)$y
  prior_hyp = approx(prior_dens$x, prior_dens$y, xout = null_hypothesis)$y

  bf01 <- round(prior_hyp/post_hyp, 3)
  bf10 <- round(post_hyp/prior_hyp, 3)

  plot <- ggplot(plotdat2, aes(x, estimation, color = postprior)) + geom_line() +
    scale_color_manual(values = c("chocolate4", "chocolate")) +
    geom_segment(aes(x = null_hypothesis,
                     y = prior_hyp,
                     yend = post_hyp,
                     xend = null_hypothesis),
                 linetype = "dashed",
                 color = "grey10") +
    geom_point(aes(x = null_hypothesis,
                   y = prior_hyp),
               color = "black") +
    geom_point(aes(x = null_hypothesis,
                   y = post_hyp),
               color = "black") +
    labs(title = paste(outcome_name, " Savage-Dickey plot",
                       "\n\nBayes Factor: ", bf01, " / ", bf10,
                       sep =""),
         x = estimate,
         y = "Estimated density (base::density)",
         color = "Posterior/Prior") +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))

  return_obj <- vector(mode="list", length = 0)
  return_obj$plot <-  plot
  return_obj$dens_est <- post_dens

  return(return_obj)

}
