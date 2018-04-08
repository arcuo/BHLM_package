# preprocessing

library(dirsetr)
library(tidyverse)
library(lazyeval)
library(purrrlyr)

# You need: Study (factor), observation number (factor), (allaround grouping factors) Meta-analysis outcome (e.g. Hedges G) and outcome options.

setClass(Class="ValuableData",
         representation(
           usedData="data.frame",
           start_bounds = "numeric"
         )
)

getValuableData <- function(dataframe,
                            groupingFactorsCols,
                            metaOutcomeCol,
                            outcomeOptionCol,
                            outcomeOptions = c()) {

  if(is_empty(outcomeOptions)) {
    stop("missing outcome options")
  } else if(is_empty(groupingFactorsCols) || length(groupingFactorsCols) > 2) {
    stop("missing grouping factors (Studies*Outcomes) or too many factors (currently limited to only 2)")
  } else {

    useful <- dataframe %>%
      select(one_of(groupingFactorsCols), outcomes = !!metaOutcomeCol, outcomes_names = !!outcomeOptionCol) %>%
      .[.[["outcomes_names"]] %in% outcomeOptions,] %>%
      mutate_at(vars(outcomes_names), funs(outcomes_numeric = as.numeric(.)))

    start_bounds <- useful[groupingFactorsCols[1]] %>% duplicated() %>% which(x = (. == FALSE)) %>% append(., nrow(useful) + 1)

    return(new("ValuableData",
               usedData=useful,
               start_bounds = start_bounds))

  }
}
