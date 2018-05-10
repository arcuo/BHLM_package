#Testing

dat = EWI

#Preprocessing test

outcomeOptions <- c("Psychological", "Physical")

useful <- getValuableData(dat,
                        groupingFactorsCols = c("StudNo", "OutcomeNo"),
                        metaOutcomeCol = "Hedges.s.g",
                        outcomeOptionCol = "Outcome2",
                        outcomeOptions = outcomeOptions)

# Write model

outcomesMatrix <- as.matrix(useful@outcomeValues)
outcomeOptionsMatrix <- as.matrix(useful@outcomeOptionsValuesNumeric)
upper.group <- nrow(outcomesMatrix)
lower.group <- data@nLowerGroup

create.outcomePriors = matrix(c("dnorm", 0, 1), length(outcomeOptions), 3, byrow = TRUE)
create.lambdaPrior = c("dgamma", 0.001, 0.001)
create.thetaPrior = c("dnorm", 0, 1)
writeModelFile(createModelFileList(nUpper = upper.group,
                                   outcomesList = outcomeOptions,
                                   thetaPrior = create.thetaPrior,
                                   lambdaPrior = create.lambdaPrior,
                                   outcomePriors = create.outcomePriors))

#JAGS

outcomes = as.vector(useful@usedData$outcomes)
outcomes_numeric = as.vector(useful@usedData$outcomes_numeric)
start_bounds = useful@start_bounds
upper.group = length(start_bounds) - 1

jags.data <- c("outcomes", "outcomes_numeric", "start_bounds", "upper.group")
resultParameters <- c(c("lambda", "theta"), outcomeOptions)

samples = jags(jags.data, inits = NULL, resultParameters,
               model.file ="modelFile.txt", n.chains=3, n.iter=10000,
               n.burnin=1000, n.thin=1, DIC=TRUE)

#Main test

a = bhlm(dataframe = dat,
         grouping_factor_cols = c("StudNo", "OutcomeNo"),
         estimate_col = "Hedges.s.g",
         outcome_options_col =  "Outcome2",
         outcome_options = c("Physical", "Psychological"),
         outcome_priors = "dnorm(0, 1)",
         lambda_prior = "dgamma(0.001, 0.001)",
         theta_prior = "dnorm(0,1)",
         bayes_method = "jags",
         identifier_col = "Study"
        )

BF <- dlogspline(0, logspline(a@jags_samples$BUGSoutput$sims.list$Physical))/dnorm(0,0,1)
