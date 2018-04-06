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

#Main test

a = metaCategorize(dataframe = dat,
                   groupingFactorsCols = c("StudNo", "OutcomeNo"),
                   metaOutcomeCol = "Hedges.s.g",
                   outcomeOptionCol = "Outcome2",
                   outcomeOptions = c("Physical", "Psychological"),
                   BayesMethod = "jags"
               )

dat2 = CBT

dat2$Outcome2 = as.factor(dat2$Outcome)
levels(dat2$Outcome2) = c("outcome1", "outcome2", "outcome3", "outcome4", "outcome5")

outcomeOptions2 <- c("outcome1", "outcome2", "outcome3")
outcomeOptionsPriors2 <- matrix(c("dnorm", 0, 1), length(outcomeOptions2), 3, byrow = TRUE)

a = metaCategorize(dat2, c("StudNo", "OutcomeNo"),
                   "Hedges.s.g",
                   "Outcome2", outcomeOptions2,
                   BayesMethod = "jags",
                   create.outcomePriors = outcomeOptionsPriors2
)

a = getValuableData(dat2, c("StudNo", "OutcomeNo"),
                    "Hedges.s.g",
                    "Outcome2", outcomeOptions2)

clean = function(dat = dat2,
                 groupingFactorsCols = c("StudNo", "OutcomeNo"),
                 metaOutcomeCol="Hedges.s.g", outCol = "Outcome2",
                 outcomeOptions = c("outcome1", "outcome2")) {

  useful <- dat %>%
    select_(.dots = groupingFactorsCols, metaOutcomeCol, outCol) %>%
    filter_(paste(outCol, " == quote(", outcomeOptions, ")", sep = "", collapse=" || "))

  return(useful)
}

clean()
