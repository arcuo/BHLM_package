#' @include lm_classes.R
#'
#' @import tidyverse
NULL

# Latent mixture preprocessing ------------------------------------------------

bhlm_preprocessing <- function(dataframe,
                                         grouping_factors_cols,
                                         meta_outcome_col,
                                         outcome_options_col,
                                         outcome_options = c(),
                                         identifier_col = "") {

  if(purrr::is_empty(outcome_options)) {
    stop("missing outcome options")
  } else if(purrr::is_empty(grouping_factors_cols) || length(grouping_factors_cols) > 2) {
    stop("missing grouping factors (Studies*Outcomes) or too many factors " +
         "(currently limited to only 2)")
  } else {

    if (identifier_col == "") {
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

define_prior_dist <- function(dist){

  if (length(dist) > 1) {
    string <- paste("~", dist[1], "(", sep="")
    i = 2
    while(i < length(dist)){
      string <- paste(string, dist[i], ",", sep="")
      i = i+1
    }
    return(paste(string, dist[length(dist)], ")", sep=""))
  } else {
    return(paste("~", dist, sep = ""))
  }
}

# Write outcome priors and references -----------------------------------------

bhlm_make_outcomes <- function(outcomes_list, outcome_priors){

  if (class(outcome_priors) == "matrix") {
    priors <- unlist(lapply(seq(1, length(outcomes_list), 1),
                            function(x) paste("\t", outcomes_list[x],
                                              define_prior_dist(outcome_priors[x,]),
                                              sep="")
                            )
                     )
  } else if (class(outcome_priors) == "character" || class(outcome_priors) == "string" ) {
    priors <- unlist(lapply(seq(1, length(outcomes_list), 1),
                            function(x) paste("\t", outcomes_list[x],
                                              define_prior_dist(outcome_priors[x]),
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

bhlm_create_model_list <- function(outcomes_list,
                                             theta_prior,
                                             lambda_prior,
                                             outcome_priors){

  return(c("model{",
           "\t",
           paste("\tlambda", define_prior_dist(lambda_prior), sep = ""),
           "\t",
           bhlm_make_outcomes(outcomes_list, outcome_priors),
           "\t",
           "\tfor (s in 1:upper_group) {",
           "\t\t",
           paste("\t\ttheta[s]", define_prior_dist(theta_prior), sep = ""),
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

bhlm_write_model <- function(modelFileList, save = "") {

  if (save == "") {

    tmpFile <- tempfile(pattern = "modelFile", tmpdir = tempdir(),  fileext = ".txt")
    connection <- file(tmpFile)
    writeLines(modelFileList, connection)
    close(connection)

    return(tmpFile)

  } else {

    connection <- file(save)
    writeLines(modelFileList, connection)
    close(connection)

    return(save)

  }

}
