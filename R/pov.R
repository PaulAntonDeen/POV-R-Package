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
#'
#' @return POV returns a table of variance components.
#'
#' @details Models for pov are specified symbolically. A typical model has the form response ~ terms where response is the (numeric) response vector and terms is a series of terms which specifies a linear predictor for response. A terms specification of the form first + second indicates all the terms in first together with all the terms in second with duplicates removed. A specification of the form first:second indicates the set of terms obtained by taking the interactions of all terms in first with all terms in second. The specification first*second indicates the cross of first and second. This is the same as first + second + first:second.
#'
#' @examples POV(Etch.Depth ~ Lot + Wafer %in% Lot, dt)
#' @export POV
POV <- function(Formula, Data) {
  model <- lm(Formula, data = Data)
  modelterms <- all.vars(Formula, Data)
  Var <- 	var(Data[modelterms[1]])
  N <- 	nobs(model)
  popVar <- (N-1)/N * Var


  #Get RSS components
  RSSTotal <- anova(model)
  RSSBetween <- sum(RSSTotal$`Sum Sq`)-RSSTotal$`Sum Sq`[length(RSSTotal$`Sum Sq`)]
  RSSWithin <- RSSTotal$`Sum Sq`[length(RSSTotal$`Sum Sq`)]
  ComponentNames <- broom::tidy(RSSTotal)$term
  ComponentNames <- ComponentNames[-length(ComponentNames)]

  #Get total variances
  VarWithinTotal <- RSSWithin/sum(RSSTotal$`Sum Sq`) * popVar
  VarBetweenTotal <- popVar - VarWithinTotal

  #Get between variance components
  BetweenVarComponents <- as.vector(VarBetweenTotal) * RSSTotal$`Sum Sq`/RSSBetween
  BetweenVarComponents <- BetweenVarComponents[-length(BetweenVarComponents)]

  #Get variance table
  VarTable <- aggregate( Formula, data=Data, FUN=var, drop=FALSE, simplify=FALSE)
  names(VarTable)[ncol(VarTable)] <- "rowVariance"
  VarTableN <- aggregate( Formula, data=Data, FUN=NROW, drop=FALSE, simplify=FALSE)
  names(VarTableN)[ncol(VarTableN)] <- "rowN"
  VarTable$rowN <- VarTableN$rowN
  VarTable <- VarTable[!(VarTable$rowN == "NULL"), ]
  VarTable[is.na(VarTable)] <- 0
  VarTable$rowVariance <- as.numeric(VarTable$rowVariance)
  VarTable$rowN <- as.numeric(VarTable$rowN)
  VarTable$popVar <- with(VarTable, rowVariance*(rowN-1)/rowN )
  CommonVar <- min(VarTable$popVar)

  #Within variance components
  formwithin <- Formula
  formula.tools::lhs(formwithin) <- quote(popVar)
  modelwithin <- lm(formwithin, data = VarTable)
  WithinComponentsRSS <- suppressWarnings(anova(modelwithin)$`Sum Sq`)
  if(sum(WithinComponentsRSS)==0){
    #Make all 0
    WithinVarComponents <- as.vector(VarWithinTotal-CommonVar) * WithinComponentsRSS[-length(WithinComponentsRSS)]
  } else {
    WithinVarComponents <- as.vector(VarWithinTotal-CommonVar) * WithinComponentsRSS[-length(WithinComponentsRSS)]/sum(WithinComponentsRSS)
  }

  #Compute POV table without group sums
  VarianceComponents <- c(BetweenVarComponents,WithinVarComponents,CommonVar)
  SDComponents <- sqrt(VarianceComponents)
  PercentComponents <- 100*VarianceComponents/sum(VarianceComponents)
  Components <- c(paste("Between ", ComponentNames, sep = ""),paste("Within ", ComponentNames, sep = ""),"Common")
  povComponents <- data.frame(Components, VarianceComponents, SDComponents, PercentComponents)
  colnames(povComponents) <- c("Component", "Variance", "StdDev", "% of total")

  #Compute POV table with group sums
  FullVarianceComponents <- c(VarBetweenTotal,BetweenVarComponents,VarWithinTotal,WithinVarComponents,CommonVar,popVar)
  FullSDComponents <- sqrt(FullVarianceComponents)
  FullPercentComponents <- 100*FullVarianceComponents/as.vector(popVar)
  FullComponents <- c("Between Total", paste("  Between ", ComponentNames, sep = ""),"Within Total", paste("  Within ", ComponentNames, sep = ""),"  Common", "Total")
  FullPOV <- data.frame(FullComponents, FullVarianceComponents, FullSDComponents, FullPercentComponents)
  colnames(FullPOV) <- c("Component", "Variance", "StdDev", "% of total")
  return(FullPOV)
  #return(povComponents)
}