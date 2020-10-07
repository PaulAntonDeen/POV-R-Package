# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Partition of Variation
#'
#' @param Formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
#' @param Data a data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model.
#' @param Complete (Default False) an optional boolean to change the result output. True will give you a table including between, within and total summary values. False will only give you the variance components themselves.
#'
#' @return POV returns a table of variance components.
#'
#' @details Models for pov are specified symbolically. A typical model has the form response ~ terms where response is the (numeric) response vector and terms is a series of terms which specifies a linear predictor for response. A terms specification of the form first + second indicates all the terms in first together with all the terms in second with duplicates removed. A specification of the form first:second indicates the set of terms obtained by taking the interactions of all terms in first with all terms in second. The specification first*second indicates the cross of first and second. This is the same as first + second + first:second.
#'
#' Variables on the right hand side of the model should be converted to factors before running.
#'
#' Between variance is the variance due to change in Mean. Within variance is the variance due to the change in StdDev. Common variance is the minimum variance common to all categories.
#'
#' @examples
#' POV(Response ~ Machine * Metrology, Data = dt, Complete = TRUE)
#' @export POV
POV <- function(Formula, Data, Complete = FALSE) {
  model <- stats::lm(Formula, data = Data)
  modelterms <- all.vars(Formula, Data)
  SampleVar <- stats::var(Data[modelterms[1]])
  N <- stats::nobs(model)
  popVar <- (N - 1) / N * SampleVar


  # Get RSS components
  RSSTotal <- stats::anova(model)
  RSSBetween <- sum(RSSTotal$`Sum Sq`) - RSSTotal$`Sum Sq`[length(RSSTotal$`Sum Sq`)]
  RSSWithin <- RSSTotal$`Sum Sq`[length(RSSTotal$`Sum Sq`)]
  ComponentNames <- broom::tidy(RSSTotal)$term
  ComponentNames <- ComponentNames[-length(ComponentNames)]

  # Get total variances
  VarWithinTotal <- RSSWithin / sum(RSSTotal$`Sum Sq`) * popVar
  VarBetweenTotal <- popVar - VarWithinTotal

  # Get between variance components
  BetweenVarComponents <- as.vector(VarBetweenTotal) * RSSTotal$`Sum Sq` / RSSBetween
  BetweenVarComponents <- BetweenVarComponents[-length(BetweenVarComponents)]

  # Get variance table
  VarTable <- stats::aggregate(Formula, data = Data, FUN = stats::var, drop = FALSE, simplify = FALSE)
  names(VarTable)[ncol(VarTable)] <- "rowVariance"
  VarTableN <- stats::aggregate(Formula, data = Data, FUN = NROW, drop = FALSE, simplify = FALSE)
  names(VarTableN)[ncol(VarTableN)] <- "rowN"
  VarTable$rowN <- VarTableN$rowN
  VarTable <- VarTable[!(VarTable$rowN == "NULL"), ]
  VarTable[is.na(VarTable)] <- 0
  VarTable$rowVariance <- as.numeric(VarTable$rowVariance)
  VarTable$rowN <- as.numeric(VarTable$rowN)
  VarTable$popVar <- with(VarTable, rowVariance * (rowN - 1) / rowN)
  CommonVar <- min(VarTable$popVar)

  # Within variance components
  formwithin <- Formula
  formula.tools::lhs(formwithin) <- quote(popVar)
  modelwithin <- stats::lm(formwithin, data = VarTable)
  WithinComponentsRSS <- suppressWarnings(stats::anova(modelwithin)$`Sum Sq`)
  if (sum(WithinComponentsRSS) == 0) {
    # Make all 0
    WithinVarComponents <- as.vector(VarWithinTotal - CommonVar) * WithinComponentsRSS[-length(WithinComponentsRSS)]
  } else {
    WithinVarComponents <- as.vector(VarWithinTotal - CommonVar) * WithinComponentsRSS[-length(WithinComponentsRSS)] / sum(WithinComponentsRSS)
  }

  if (Complete) {
    # Compute POV table with group sums
    FullVarianceComponents <- c(VarBetweenTotal, BetweenVarComponents, VarWithinTotal, WithinVarComponents, CommonVar, popVar)
    FullSDComponents <- sqrt(FullVarianceComponents)
    FullPercentComponents <- 100 * FullVarianceComponents / as.vector(popVar)
    FullComponents <- c("Between Total", paste("  Between ", ComponentNames, sep = ""), "Within Total", paste("  Within ", ComponentNames, sep = ""), "  Common", "Total")
    FullPOV <- data.frame(FullVarianceComponents, FullSDComponents, FullPercentComponents, row.names = FullComponents)
    colnames(FullPOV) <- c("Variance", "StdDev", "% of total")
    return(FullPOV)
  } else {
    # Compute POV table without group sums
    VarianceComponents <- c(BetweenVarComponents, WithinVarComponents, CommonVar)
    SDComponents <- sqrt(VarianceComponents)
    PercentComponents <- 100 * VarianceComponents / sum(VarianceComponents)
    Components <- c(paste("Between ", ComponentNames, sep = ""), paste("Within ", ComponentNames, sep = ""), "Common")
    povComponents <- data.frame(VarianceComponents, SDComponents, PercentComponents, row.names = Components)
    colnames(povComponents) <- c("Variance", "StdDev", "% of total")
    return(povComponents)
  }
}
