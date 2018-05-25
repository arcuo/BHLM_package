# Data ------------------------------------------------------------------------

dat1 = EWI
dat2 = CBT
dat2 = mutate(dat2, Outcome2 = as.factor(Outcome)) %>% mutate(Outcome2 = fct_recode(Outcome2, "phi" = "1", "rho" = "2", "psi" = "3", "the" = "4", "ce" = "5"))

#Preprocessing test -----------------------------------------------------------

outcomeOptions <- c("phi", "rho", "psi", "the")

useful <- bhlm_preprocessing(dat2,
                        grouping_factors_cols = c("StudNo", "OutcomeNo"),
                        meta_outcome_col = "Hedges.s.g",
                        outcome_options_col = "Outcome2",
                        outcome_options = outcomeOptions)

# Write model -----------------------------------------------------------------

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

#JAGS -------------------------------------------------------------------------

outcomes = as.vector(useful@usedData$outcomes)
outcomes_numeric = as.vector(useful@usedData$outcomes_numeric)
start_bounds = useful@start_bounds
upper.group = length(start_bounds) - 1

jags.data <- c("outcomes", "outcomes_numeric", "start_bounds", "upper.group")
resultParameters <- c(c("lambda", "theta"), outcomeOptions)

samples = jags(jags.data, inits = NULL, resultParameters,
               model.file ="modelFile.txt", n.chains=3, n.iter=10000,
               n.burnin=1000, n.thin=1, DIC=TRUE)

#Main test --------------------------------------------------------------------

## EWI 1 ----------------------------------------------------------------------

dat1 = EWI

set.seed(1)
a = bhlm(dataframe = dat1,
         grouping_factor_cols = c("StudNo", "OutcomeNo"),
         estimate_col = "Hedges.s.g",
         outcome_options_col =  "Outcome2",
         outcome_options = c("Physical", "Psychological", "QoL"),
         outcome_priors = c("dnorm(0, 1)"),
         lambda_prior = "dgamma(0.001, 0.001)",
         theta_prior = "dnorm(0,1)",
         bayes_method = "jags",
         identifier_col = "Study",
         jags_iter = 3000,
         jags_burnin = 500
        )

a@jags_samples
traceplots <- bhlm.traceplot(a, return_plots = TRUE)
postplots <- bhlm.SDplots(a, 0, return_plots = T)
bhlm.MAP(a)

## EWI 2 ----------------------------------------------------------------------

dat1 = EWI

set.seed(1)
a2 = bhlm(dataframe = dat1,
         grouping_factor_cols = c("StudNo", "OutcomeNo"),
         estimate_col = "Hedges.s.g",
         outcome_options_col =  "Outcome2",
         outcome_options = c("Physical", "Psychological", "QoL"),
         outcome_priors = c("dnorm(0, 0.1)"),
         lambda_prior = "dgamma(0.001, 0.001)",
         theta_prior = "dnorm(0,1)",
         bayes_method = "jags",
         identifier_col = "Study",
         jags_iter = 3000,
         jags_burnin = 500
)

a2@jags_samples
traceplots <- bhlm.traceplot(a2, return_plots = TRUE)
postplots <- bhlm.SDplots(a2, 0, return_plots = T)
bhlm.MAP(a2)



## CBT ------------------------------------------------------------------------

dat2 = CBT
dat2 = mutate(dat2, Outcome2 = as.factor(Outcome)) %>%
  mutate(Outcome2 = fct_recode(Outcome2,
                               "Psychological" = "1",
                               "Interpersonal" = "2",
                               "Physical" = "3",
                               "Mastery" = "4",
                               "QoL" = "5"))
set.seed(1)
b = bhlm(dataframe = dat2,
         grouping_factor_cols = c("StudNo", "OutcomeNo"),
         estimate_col = "Hedges.s.g",
         outcome_options_col =  "Outcome2",
         outcome_options = c("Psychological","Interpersonal","Physical","Mastery","QoL"),
         outcome_priors = c("dnorm", 0, 1),
         lambda_prior = "dgamma(0.001, 0.001)",
         theta_prior = "dnorm(0,1)",
         bayes_method = "jags",
         identifier_col = "Study",
         jags_iter = 3000,
         jags_burnin = 500
)

b@jags_samples
traceplots <- bhlm.traceplot(b, return_plots = TRUE)
postplots <- bhlm.SDplots(b, 0, return_plots = T)
bhlm.MAP(b)

### WITH MATRIX PRIORS --------------------------------------------------------

outcome_priors_new <- matrix(c("dnorm", 0, 0.1),
                             5,
                             ncol = 3, byrow = TRUE)
set.seed(1)
b2 = bhlm(dataframe = dat2,
         grouping_factor_cols = c("StudNo", "OutcomeNo"),
         estimate_col = "Hedges.s.g",
         outcome_options_col =  "Outcome2",
         outcome_options = c("Psychological","Interpersonal","Physical","Mastery","QoL"),
         outcome_priors = outcome_priors_new,
         lambda_prior = "dgamma(0.001, 0.001)",
         theta_prior = "dnorm(0,1)",
         bayes_method = "jags",
         identifier_col = "Study",
         jags_iter = 3000,
         jags_burnin = 500
)

b2@jags_samples
traceplots <- bhlm.traceplot(b2, return_plots = TRUE)
postplots <- bhlm.SDplots(b2, 0, return_plots = T)
bhlm.MAP(b2)

# Plots -----------------------------------------------------------------------

traceplots <- bhlm.traceplot(b2, return_plots = TRUE)

postplots <- bhlm.SDplots(b2, 0, return_plots = F)

c <- jags.samples(b@jags_samples$model, c("psi", "phi", "rho"), n.iter = 1000)


library(jagsplot)

jags.trace(b@jags_samples, which.param = c("psi", "phi", "rho", "the"))

c <- as.data.frame(KernSmooth::bkde(as.vector(b@jags_samples$BUGSoutput$sims.list$psi)))

c2 <- as.data.frame(b@jags_samples$BUGSoutput$sims.list$psi) %>% rename(x2 = V1)

c3 <- data.frame(x3 = rlogspline(1000000, logspline(b@jags_samples$BUGSoutput$sims.list$psi)))

ggplot(c, aes(x, y)) + geom_line()

ggplot(c3, aes(x3)) + geom_density()

dlogspline(0, logspline(b@jags_samples$BUGSoutput$sims.list$psi))/dnorm(0,0,1)
plogspline(0.4, logspline(b@jags_samples$BUGSoutput$sims.list$psi))
dlogspline(0, logspline(b@jags_samples$BUGSoutput$sims.list$phi))/dnorm(0,0,1)
dlogspline(0, logspline(b@jags_samples$BUGSoutput$sims.list$rho))/dnorm(0,0,1)
dlogspline(0, logspline(b@jags_samples$BUGSoutput$sims.list$the))/dnorm(0,0,1)

# Standard deviation ----------------------------------------------------------

useful <- bhlm.preprocessing(dat2,
                             grouping_factors_cols = c("StudNo", "OutcomeNo"),
                             meta_outcome_col = "Hedges.s.g",
                             outcome_options_col = "Outcome2",
                             outcome_options = c("phi", "rho", "psi"),
                             identifier_col = "SE")
useful@used_data


modelstring <- "model{

  theta_field~dnorm(0, 1)
  lambda~dgamma(0.001, 0.001)

  phi~dnorm(0, 1)
  rho~dnorm(0, 1)
  psi~dnorm(0, 1)

  outcome_options[1] <- phi
  outcome_options[2] <- rho
  outcome_options[3] <- psi

  for (s in 1:upper_group) {

    theta[s]~dnorm(theta_field, 1)

    for (o in start_bounds[s]:(start_bounds[s+1]-1)) {

      outcome[s,o] <-  outcome_options[outcomes_numeric[o]]
      eta[s,o] <- theta[s] + outcome[s,o]
      outcomes[o] ~ dnorm(eta[s,o],lambda)

    }
  }
}"

outcomes = as.vector(useful@used_data$outcomes)
outcomes_numeric = as.vector(useful@used_data$outcomes_numeric)
start_bounds = useful@start_bounds
upper_group = length(start_bounds) - 1

jags_data <- c("outcomes", "outcomes_numeric", "start_bounds", "upper_group")
result_parameters <- c(c("lambda", "theta", "theta_field"), c("phi", "rho", "psi"))

samples = R2jags::jags(jags_data, inits = NULL, result_parameters,
                       model.file = textConnection(modelstring), n.chains=3, n.iter=10000,
                       n.burnin=2000, n.thin=1, DIC=TRUE)
