#' @import R2jags
#' @import dirsetr
#' @import polspline
#' @import ggplot2

library(R2jags)
library(dirsetr)
library(polspline)
library(ggplot2)

setClass(Class="metaCategorize_object",
         representation(
           data = "ValuableData",
           jags.samples = "rjags"
         )
)

metaCategorize <- function(dataframe,
                           groupingFactorsCols,
                           metaOutcomeCol,
                           outcomeOptionCol,
                           outcomeOptions,
                           BayesMethod = "jags",
                           create.outcomePriors = matrix(c("dnorm", 0, 1), length(outcomeOptions), 3, byrow = TRUE),
                           create.lambdaPrior = c("dgamma", 0.001, 0.001), create.thetaPrior = c("dnorm", 0, 1),
                           jags.inits = NULL, jags.chains=3, jags.iter = 10000,
                           jags.burning = 1000, jags.thin = 1, jags.DIC = TRUE,
                           saveFile = "") {

  if(BayesMethod == "jags") {

    tryCatch({

    data <- getValuableData(dataframe,
                    groupingFactorsCols,
                    metaOutcomeCol,
                    outcomeOptionCol,
                    outcomeOptions)

    }, error = function(e) {
      print("Error at getValuableData")
    })

    # Create needed objects ()

    outcomes = as.vector(useful@usedData$outcomes)
    outcomes_numeric = as.vector(useful@usedData$outcomes_numeric)
    start_bounds = useful@start_bounds
    upper_group = length(start_bounds) - 1

    jags.data <- c("outcomes", "outcomes_numeric", "start_bounds", "upper_group")
    resultParameters <- c(c("lambda", "theta"), outcomeOptions)

    # Create model file

    modelfilepath = writeModelFile(createModelFileList(nUpper = upper_group,
                                       outcomesList = outcomeOptions,
                                       thetaPrior = create.thetaPrior,
                                       lambdaPrior = create.lambdaPrior,
                                       outcomePriors = create.outcomePriors
                                       ), save = saveFile)

    samples = jags(jags.data, inits = jags.inits, resultParameters,
                   model.file = modelfilepath, n.chains=jags.chains, n.iter=jags.iter,
                   n.burnin=jags.burning, n.thin=jags.thin, DIC=jags.DIC)

  }

  return(new("metaCategorize_object",
             data = data,
             jags.samples = samples))

}






