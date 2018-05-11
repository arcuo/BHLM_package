#Testing

dat1 = EWI
dat2 = CBT
dat2 = mutate(dat2, Outcome2 = as.factor(Outcome)) %>% mutate(Outcome2 = fct_recode(Outcome2, "phi" = "1", "rho" = "2", "psi" = "3", "the" = "4", "ce" = "5"))

#Preprocessing test

outcomeOptions <- c("phi", "rho", "psi", "the")

useful <- bhlm_preprocessing(dat2,
                        grouping_factors_cols = c("StudNo", "OutcomeNo"),
                        meta_outcome_col = "Hedges.s.g",
                        outcome_options_col = "Outcome2",
                        outcome_options = outcomeOptions)

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

## EWI

a = bhlm(dataframe = dat1,
         grouping_factor_cols = c("StudNo", "OutcomeNo"),
         estimate_col = "Hedges.s.g",
         outcome_options_col =  "Outcome2",
         outcome_options = c("Physical", "Psychological", "QoL"),
         outcome_priors = c("dnorm(0, 1)", "dnorm(0, 1.1)"),
         lambda_prior = "dgamma(0.001, 0.001)",
         theta_prior = "dnorm(0,1)",
         bayes_method = "jags",
         identifier_col = "Study",
         save_model = "D:\\Desktop\\bachelors_meta\\model_file.txt"
        )

BF <- dlogspline(0, logspline(a@jags_samples$BUGSoutput$sims.list$Physical))/dnorm(0,0,1)

## CBT

b = bhlm(dataframe = dat2,
         grouping_factor_cols = c("StudNo", "OutcomeNo"),
         estimate_col = "Hedges.s.g",
         outcome_options_col =  "Outcome2",
         outcome_options = c("phi", "rho", "psi"),
         outcome_priors = c("dnorm(0, 1)"),
         lambda_prior = "dgamma(0.001, 0.001)",
         theta_prior = "dnorm(0,1)",
         bayes_method = "jags",
         identifier_col = "Study",
         jags_thin = 19,
         save_model = "D:\\Desktop\\bachelors_meta\\model_file2.txt"
)

# Plots

ar <-  b@jags_samples$BUGSoutput$sims.array

iter <- seq((b@jags_samples$BUGSoutput$n.burnin+ b@jags_samples$BUGSoutput$n.thin),b@jags_samples$BUGSoutput$n.iter,by=b@jags_samples$BUGSoutput$n.thin)

psidata = as.data.frame(list("iter" = iter,
                        "chain1" = ar[, 1, "psi"],
                        "chain2" = ar[, 2, "psi"],
                        "chain3" = ar[, 3, "psi"])) %>%
  gather("chain", "sim", chain1, chain2, chain3)

ggplot(psidata, aes(x = iter, y = sim, color = chain)) + geom_line(alpha = 0.9) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  ggtitle("Psi")

c <- bhlm.traceplot(b)

library(jagsplot)

jags.trace(b@jags_samples, which.param = c("psi", "phi", "rho", "the"))

dlogspline(0, logspline(b@jags_samples$BUGSoutput$sims.list$psi))/dnorm(0,0,1)
dlogspline(0, logspline(b@jags_samples$BUGSoutput$sims.list$phi))/dnorm(0,0,1)
dlogspline(0, logspline(b@jags_samples$BUGSoutput$sims.list$rho))/dnorm(0,0,1)
dlogspline(0, logspline(b@jags_samples$BUGSoutput$sims.list$the))/dnorm(0,0,1)

