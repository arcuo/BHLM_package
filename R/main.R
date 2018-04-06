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
                           jags.burning = 1000, jags.thin = 1, jags.DIC = TRUE) {

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

    outcomesMatrix <- as.matrix(data@outcomeValues)
    outcomeOptionsMatrix <- as.matrix(data@outcomeOptionsValuesNumeric)
    upper.group <- nrow(outcomesMatrix)
    lower.group <- data@nLowerGroup

    jags.data <- c("outcomesMatrix", "outcomeOptionsMatrix", "upper.group", "lower.group")
    resultParameters <- c(c("lambda", "theta"), outcomeOptions)

    # Create model file

    writeModelFile(createModelFileList(nUpper = upper.group,
                                       outcomesList = outcomeOptions,
                                       thetaPrior = create.thetaPrior,
                                       lambdaPrior = create.lambdaPrior,
                                       outcomePriors = create.outcomePriors))

    samples = jags(jags.data, inits = jags.inits, resultParameters,
                   model.file ="modelFile.txt", n.chains=jags.chains, n.iter=jags.iter,
                   n.burnin=jags.burning, n.thin=jags.thin, DIC=jags.DIC)

  }

  return(new("metaCategorize_object",
             data = data,
             jags.samples = samples))

}






