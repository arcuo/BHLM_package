#' @import tidyverse

# Data

# Subfunctions

##make prior string
definePriorDist <- function(dist = c("dnorm", 0, 1)){

  String = paste("~", dist[1], "(", sep="")

  i = 2
  while(i < length(dist)){
          String = paste(String, dist[i], ",", sep="")
          i = i+1
  }

  String = paste(String, dist[length(dist)], ")", sep="")

  return(String)

}


##write outcome priors and references
makeOutcomes <- function(outcomesList, outcomePriors){

  priors = unlist(lapply(seq(1, length(outcomesList), 1), function(x) paste("\t", outcomesList[x], definePriorDist(outcomePriors[x,]), sep="")))
  outcomes = unlist(lapply(seq(1, length(outcomesList), 1), function(x) paste("\toutcome_options[", x, "] <- ", outcomesList[x], sep="")))

  return(c(priors, "\t", outcomes))
}


# Main functions

##create the modelfilelist
createModelFileList <- function(nUpper,
                                outcomesList,
                                thetaPrior = c("dnorm", 0, 1),
                                lambdaPrior = c("dgamma", 0.001, 0.001),
                                outcomePriors = matrix(c("dnorm", 0, 1), length(outcomesList), 3, byrow = TRUE)){

  return(c("model{",
           "\t",
           paste("\tlambda", definePriorDist(lambdaPrior), sep = ""),
           "\t",
           makeOutcomes(outcomesList, outcomePriors),
           "\t",
           "\t# Loop through studies (upper_group)",
           "\tfor (s in 1:upper_group) {",
           "\t\t",
           paste("\t\ttheta[s]", definePriorDist(thetaPrior), sep = ""),
           "\t\t",
           "\t\t# Loop through observations",
           "\t\tfor (o in start_bounds[s]:(start_bounds[s+1]-1)) {",
           "\t\t\t",
           "\t\t\toutcome[s,o] <-  outcome_options[outcomes_numeric[o]]",
           "\t\t\teta[s,o] <- theta[s] + outcome[s,o]",
           "\t\t\toutcomes[o] ~ dnorm(eta[s,o],lambda)",
           "\t\t\t",
           "\t\t}",
           "\t}",
           "}"
            )
  )


}

##write to textfile
writeModelFile <- function(modelFileList, save = "") {

  if (save == "") {

    tmpFile = tempfile(pattern = "modelFile", tmpdir = tempdir(),  fileext = ".txt")
    connection = file(tmpFile)
    writeLines(modelFileList, connection)
    close(connection)

    return(tmpFile)

  } else {

    connection = file(save)
    writeLines(modelFileList, connection)
    close(connection)

    return(save)

  }

}

