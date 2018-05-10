#' @include lm_classes.R
#' @include lm_helpers.R
#'
#' @import tidyverse
#' @import R2jags
NULL

#'@title Bayesian Hierarchical Latent Mixture Model
#'@description Run a BHLM model with custom number of latent variables according to specified number of outcome options.
#'The model sets priors for each outcome, group mean (theta) and estimate variance.
#'
#'Go to LINK TO MODEL GRAPHICAL NOTATION for model specification.
#' @author Hugh Benjamin Zachariae
#' @export
#' @param dataframe A data frame with studies, two grouping factor cols (e.g. studies*outcomes),
#' estimates, and optional ID column.
#' @param grouping_factor_cols A data vector of two grouping factor columns of the data frame
#'  (e.g. \code{c("Study", "Outcome")}).
#' @param estimate_col Column of estimates in the data frame.
#' @param outcome_options_col Column containing the different outcome names
#'
#'  \code{Outcomes : Factor w/ 3 levels "Physical","Psychological",..: 2 2 2 2 2 2 2 2 1 1 ...}
#'
#' @param outcome_options Chosen outcomes. You can choose a subset of outcomes.
#' @param outcome_priors Priors for outcomes. Takes data vector, \code{character}, or \code{matrix}.
#'
#'  Define all outcome priors to one set prior (throws Warning) as \code{character}
#'
#'  \code{"dnorm(0, 1)"}
#'
#'  priors defined as character strings are checked with regex:
#'
#'  \code{grepl("^[a-zA-Z]+\\(((\\d*\\.)*\\d+,\\s?)*(\\d*\\.)*\\d+\\)", .)}
#'
#'  or as data vector
#'
#'  \code{c("dnorm", 0, 1)}
#'
#'  Define all outcome priors manually with \code{matrix}. \strong{Notice}: byrow = TRUE)
#'
#'  \code{matrix(c("dnorm", 0, 1, "dnorm", 0, 1.1),
#'  nrow = 'number of outcomes',
#'  ncol = 'maximum amount of dist parameters',
#'  byrow = TRUE)}
#'
#'  or as data vector of \code{characters}
#'
#'  \code{c("dnorm(0,1)", "dnorm(0,1.1)")}
#'
#' @param lambda_prior Prior for Lambda as \code{character} or data vector.
#' @param theta_prior Prior for Theta as \code{character} or data vector.
#' @param identifier_col ID column. Chosen column is added to the used data for easier overview.
#' @param bayes_method Use JAGS or STAN (currently only JAGS is implemented).
#' @param jags Parameters for \code{R2jags::\link[R2jags]{jags}}:
#'
#'   \code{init} \code{NULL} default, \code{chains},
#'  \code{iter} iterations per chain, \code{burning} length of burn, \code{thin} thinning rate, and
#'  \code{DIC} compute deviance, pD and DIC.
#' @param save_file Input filepath location and name to save the model file as .txt
#'
#' @examples
#'
#' # INCOMING
#'
#'
bhlm <- function(dataframe,
                 grouping_factor_cols,
                 estimate_col,
                 outcome_options_col,
                 outcome_options,
                 outcome_priors,
                 lambda_prior,
                 theta_prior,
                 identifier_col = "",
                 bayes_method = "jags",
                 jags_init = NULL, jags_chains=3, jags_iter = 10000,
                 jags_burning = 1000, jags_thin = 1, jags_DIC = TRUE,
                 save_file = "") {

  if(bayes_method == "jags") {

    tryCatch({

    data <- bhlm_preprocessing(dataframe,
                               grouping_factor_cols,
                               estimate_col,
                               outcome_options_col,
                               outcome_options,
                               identifier_col = identifier_col)

    }, error = function(e) {

      stop(paste("Preprocessing:", e))

    })

# Create objects needed for model and jags ----------------------------------------------

    outcomes = as.vector(data@used_data$outcomes)
    outcomes_numeric = as.vector(data@used_data$outcomes_numeric)
    start_bounds = data@start_bounds
    upper_group = length(start_bounds) - 1

    jags_data <- c("outcomes", "outcomes_numeric", "start_bounds", "upper_group")
    result_parameters <- c(c("lambda", "theta"), outcome_options)

# Convert outcome priors ------------------------------------------------------

    dist_check <- "^[a-zA-Z]+\\(((\\d*\\.)*\\d+,\\s?)*(\\d*\\.)*\\d+\\)"

    # Matrix (all outcome prior defined by list c("dist", n, n))

    if (class(outcome_priors) == "matrix") {
      if (length(outcome_priors[,1]) != length(outcome_options)) {
        stop(paste("Wrong number of priors, found ",
                   length(outcome_priors[,1]),
                   ", requires ", length(outcome_options),
                   " priors defined.\n  For full outcome prior defined, use: \n",
                   "  matrix(outcome_priors, nrow = length(outcome_options), ",
                   "ncol = 'distribution + max(parameters)', byrow = TRUE)",
                   sep = "")
             )
      } else {
        outcome_priors_new <- outcome_priors
      }
    # c(...) or single string/character
    } else if (class(outcome_priors) == "character" || class(outcome_priors) == "string" ) {
      # If c(...)
      if (length(outcome_priors) > 1) {
        # If c("dist", n, n)
        if (!grepl("[^0-9]", outcome_priors[2])) {
          warning(paste("Only one outcome prior defined as vector, c(",
                        paste(outcome_priors, collapse = ", "),
                        "). All outcomes are set to have this prior.",
                        sep = "")
                  )
          outcome_priors_new <- matrix(outcome_priors,
                                       length(outcome_options),
                                       ncol = 3, byrow = TRUE)
        # Else c("dnorm(0, 1), ...) (all outcome priors defined)
        } else {
          # Check that all prior distributions are well defined
          if (!all(grepl(dist_check, outcome_priors))) {
            stop(paste("Error in one of the defined outcome prior distributions.\n  ",
                 paste(outcome_priors, collapse = "\n"),
                 sep = ""))
          } else {
            outcome_priors_new <- outcome_priors
          }
        }
        # Else only one string (all outcomes get same prior.)
      } else {
        if (!grepl(dist_check, outcome_priors)) {
          stop(paste("Error in the defined outcome prior distribution.\n",
                     outcome_priors,
                     sep = ""))
        } else {
          warning(paste("Only one outcome prior defined as string, '",
                        outcome_priors,
                        "'. All outcomes are set to have this prior.",
                        sep = ""))
          outcome_priors_new <- rep(outcome_priors, length(outcome_options))
        }
      }
    } else {
      stop(paste("Unknown outcome priors setup, found ",
                 class(outcome_priors),
                 ".\n  oucome_priors has following setups: \n  ",
                 "One prior for all",
                 sep = ""))
    }

# Create model file -----------------------------------------------------------

    tryCatch({

      modelfilepath = bhlm_write_model(
        bhlm_create_model_list(outcomes_list = outcome_options,
                               theta_prior = theta_prior,
                               lambda_prior = lambda_prior,
                               outcome_priors = outcome_priors_new),
        save = save_file)

    }, error = function(e) {

      stop(paste("Model creation:", e))

    })

    tryCatch({

      samples = R2jags::jags(jags_data, inits = jags_init, result_parameters,
                             model.file = modelfilepath, n.chains=jags_chains, n.iter=jags_iter,
                             n.burnin=jags_burning, n.thin=jags_thin, DIC=jags_DIC)

    }, error = function(e) {

      stop(paste("JAGS:", e))

    })
  }

  return(new("bhlm_object",
             data = data,
             jags_samples = samples))

}






