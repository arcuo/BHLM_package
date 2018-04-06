# preprocessing

library(dirsetr)
library(tidyverse)
library(lazyeval)
library(purrrlyr)

# You need: Study (factor), observation number (factor), (allaround grouping factors) Meta-analysis outcome (e.g. Hedges G) and outcome options.

setClass(Class="ValuableData",
         representation(
           usedData="data.frame",
           outcomeValues="data.frame",
           outcomeOptionsValues="data.frame",
           outcomeOptionsValuesNumeric="data.frame",
           nLowerGroup = "integer"
         )
)

getValuableData <- function(dataframe,
                            groupingFactorsCols,
                            metaOutcomeCol,
                            outcomeOptionCol,
                            outcomeOptions) {

  useful <- dataframe %>%
    select(one_of(groupingFactorsCols), !!metaOutcomeCol, !!outcomeOptionCol)

  useful[[outcomeOptionCol]] = as.character(useful[[outcomeOptionCol]])

  useful <- useful[useful[[outcomeOptionCol]] %in% outcomeOptions,]

  #outcome values in matrix
  outcomeValues <- useful %>%
    select_(.dots = groupingFactorsCols, metaOutcomeCol) %>%
    spread_(groupingFactorsCols[2], metaOutcomeCol) %>%
    select_(paste("-", GroupingFactorsCols[1], sep = ""))
  #outcome options in matrix
  outcomeOptionsValues <- useful %>%
    select_(.dots = groupingFactorsCols, outcomeOptionCol) %>%
    spread_(groupingFactorsCols[2], outcomeOptionCol) %>%
    select_(paste("-", GroupingFactorsCols[1], sep = ""))
  #outcome option numeric in matrix
  outcomeOptionsValuesNumeric <- outcomeOptionsValues %>%
    mutate_all(funs(as.numeric), vars = colnames(.))
  #n for lower grouping GroupingFactorsCols[2]
  nLowerGroup <- outcomeOptionsValuesNumeric %>%
    by_row(function(x) sum(!is.na(x)), .to="n") %>%
    select_("n") %>%
    unlist(use.names = FALSE)



  return(new("ValuableData",
             usedData=useful,
             outcomeValues=outcomeValues,
             outcomeOptionsValues=outcomeOptionsValues,
             outcomeOptionsValuesNumeric=outcomeOptionsValuesNumeric,
             nLowerGroup = nLowerGroup))

}
