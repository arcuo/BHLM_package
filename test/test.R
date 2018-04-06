#Testing

dat = EWI

a = metaCategorize(dataframe = dat,
                   groupingFactorCols = c("StudNo", "OutcomeNo"),
                   metaOutcomeCol = "Hedges.s.g", "Outcome2",
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
                 GroupingFactorsCols = c("StudNo", "OutcomeNo"),
                 metaOutcomeCol="Hedges.s.g", outCol = "Outcome2",
                 outcomeOptions = c("outcome1", "outcome2")) {

  useful <- dat %>%
    select_(.dots = GroupingFactorsCols, metaOutcomeCol, outCol) %>%
    filter_(paste(outCol, " == quote(", outcomeOptions, ")", sep = "", collapse=" || "))

  return(useful)
}

clean()
