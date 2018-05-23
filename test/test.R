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

## EWI ------------------------------------------------------------------------

a = bhlm(dataframe = dat1,
         grouping_factor_cols = c("StudNo", "OutcomeNo"),
         estimate_col = "Hedges.s.g",
         outcome_options_col =  "Outcome2",
         outcome_options = c("Physical", "Psychological", "QoL"),
         outcome_priors = c("dnorm(0, 1)"),
         lambda_prior = "dgamma(0.001, 0.001)",
         theta_prior = "dnorm(0,1)",
         bayes_method = "jags",
         identifier_col = "Study"
        )

phys <- logspline(a@jags_samples$BUGSoutput$sims.list$Physical)
prior <-logspline(rnorm(10000, 0,1))

u1 <- min(qlogspline(0.01, phys), qlogspline(0.01, prior))
u2 <- max(qlogspline(0.99, phys), qlogspline(0.99, prior))
u3 <- 1.1 * u1 - 0.1 * u2
u4 <- 1.1 * u2 - 0.1 * u1

physdat <- data.frame(x = (0:(100 - 1))/(100 - 1) * (u4 - u3) + u3) %>%
  mutate(post = dlogspline(x, phys), prior = dlogspline(x, prior)) %>%
  gather("postprior", "estimation", c(post, prior))

ggplot(physdat, aes(x, estimation, color = postprior)) + geom_line() +
  geom_segment(aes(x = 0, y = dlogspline(0, prior), yend = dlogspline(0, phys), xend = 0), linetype = "dashed", color = "black") +
  theme_bw()

dlogspline(0, logspline(a@jags_samples$BUGSoutput$sims.list$Physical))/dnorm(0,0,1)
dlogspline(0, logspline(a@jags_samples$BUGSoutput$sims.list$Psychological))/dnorm(0,0,1)

traceplots <- bhlm.traceplot(a, return_plots = TRUE)

priors <-  data.frame(Physical = rnorm(10000, 0, 1), Psychological = rnorm(10000, 0, 1), QoL = rnorm(10000, 0, 1))
postplots <- bhlm.posteriorplot(a, outcome_priors_data = priors)

phys = logspline(a@jags_samples$BUGSoutput$sims.list$Physical)
plot(phys)

suplot(density(a@jags_samples$BUGSoutput$sims.list$Physical))

## CBT ------------------------------------------------------------------------

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
         jags_thin = 27,
         jags_chains = 3,
         save_model = "D:\\Desktop\\bachelors_meta\\model_file2.txt"
)

### WITH MATRIX PRIORS --------------------------------------------------------

outcome_priors_new <- matrix(c("dnorm", 0, 1),
                             3,
                             ncol = 3, byrow = TRUE)

b2 = bhlm(dataframe = dat2,
         grouping_factor_cols = c("StudNo", "OutcomeNo"),
         estimate_col = "Hedges.s.g",
         outcome_options_col =  "Outcome2",
         outcome_options = c("phi", "rho", "psi"),
         outcome_priors = outcome_priors_new,
         lambda_prior = "dgamma(0.001, 0.001)",
         theta_prior = "dnorm(0,1)",
         bayes_method = "jags",
         identifier_col = "Study",
         jags_thin = 27,
         jags_chains = 3,
         field_theta_precision = NULL,
         save_model = "D:\\Desktop\\bachelors_meta\\model_file2.txt"
)

# Plots -----------------------------------------------------------------------

traceplots <- bhlm.traceplot(b2, return_plots = TRUE)

postplots <- bhlm.posteriorplot(b2, return_plots = F)

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
