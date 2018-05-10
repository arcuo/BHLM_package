unseen_studies_model_string = "model {

#Loop through studies

for(i in 1:length(studies_sig)) {

studies_sig[i] ~ dbern(theta) # Bernoulli

}

#Priors

# alpha and beta specified outside the model.

theta ~ dbeta(alpha, beta)

}"

library(purrr)

studies_sig <- as.numeric(rbernoulli(50, 0.75))
alpha <-  5
beta <- 5

data <- c("studies_sig", "alpha", "beta")

library(R2jags)

samples <- jags(data, NULL, c("theta"), model.file = textConnection(unseen_studies_model_string), 3, 10000, 1000)
d <- as.data.frame(samples$BUGSoutput$sims.list$theta[,1])
names(d) <- "x"

d$y <- rbeta(3000, 5,5)

library(ggplot2)

ggplot(d, aes(x)) + geom_density() + geom_density(aes(y))

plot(samples)
dev.off()
