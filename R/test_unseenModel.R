# Unseen studies bayesian model


# Multibinomial model
# k = observed studies ~ dbinom(q, N)
# N = Total studies ~ multinom(n, A)
# n = conjugate prior to N ~ dirichlet()
# q = probability for publication <- R + p(1-R)
# R = probability for significant results ~ dbeta(.5, .5)
# rho = rate of unsignificant publications ~ dunif(0,1) (maybe set 0.1, 0.5 and 0.9)


model.string <- "

model {

  r_lambda = dlambda(0.001, 0.001)

  for (i in 1:3) {

    #observed analysis

    r_observed = sig_studies/total_studies

    r_otheta = dnorm(r_observed, r_lambda)

    q_observed = r_otheta + p[i]*(1-r_otheta)

    n ~ dnbinom(k_observed, q_observed)



    r ~ dbeta(0.5, 0.5)


    n_total = n + k_observed

    k ~ dbinom(n_total, q)

  }

}

"



