#' An S4 class to contain the used data for \code{latent_mixture} from the preprocessing.
#'
#' @slot used_data The updated dataframe of the data, after preprocessing.
#' @slot start_bounds Starting bounds between studies for the looping through estimates.
setClass(Class="bhlm_data",
         representation(
           used_data="data.frame",
           start_bounds = "numeric"
         )
)

#' An S4 class to contain the results and used data of \code{latent_mixture}.
#'
#' @slot data latent_mixture_data object, including the data used in the latent mixture model.
#' @slot jags_samples rjags object from R2Jags.
setClass(Class="bhlm_object",
         representation(
           data = "bhlm_data",
           jags_samples = "rjags"
         )
)
