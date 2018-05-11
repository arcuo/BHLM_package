#' @include bhlm_classes.R
#'
#' @import tidyverse
NULL

# Latent mixture preprocessing ------------------------------------------------

bhlm.preprocessing <- function(dataframe,
                                         grouping_factors_cols,
                                         meta_outcome_col,
                                         outcome_options_col,
                                         outcome_options = c(),
                                         identifier_col = NULL) {

  if(purrr::is_empty(outcome_options)) {
    stop("missing outcome options", call. = FALSE)
  } else if(purrr::is_empty(grouping_factors_cols) || length(grouping_factors_cols) > 2) {
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

define.prior.dist <- function(dist){

  if (length(dist) > 1) {
    string <-
      paste(paste("~", dist[1], "(", sep=""),
            +       paste(tail(dist, length(dist)-1), collapse = ", "),
            +       ")", sep = "")
    return(string)
  } else {
    return(paste("~", dist, sep = ""))
  }
}

# Write outcome priors and references -----------------------------------------

bhlm.make.outcomes <- function(outcomes_list, outcome_priors){

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

bhlm.create.model.list <- function(outcomes_list,
                                             theta_prior,
                                             lambda_prior,
                                             outcome_priors){

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

  }

# Write to textfile -----------------------------------------------------------

bhlm.write.model <- function(model_file_list, path = NULL) {

  if (is.null(path)) {

    tmpFile <- tempfile(pattern = "modelFile", tmpdir = tempdir(),  fileext = ".txt")
    connection <- file(tmpFile)
    writeLines(model_file_list, connection)
    close(connection)

    return(tmpFile)

  } else {

    connection <- file(path)
    writeLines(model_file_list, connection)
    close(connection)

    return(path)

  }

}
