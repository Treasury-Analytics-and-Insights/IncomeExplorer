# Helper functions for EMTR function

#' @title Abatement
#' @author Christopher Ball, \email{Christopher.Ball@@treasury.govt.nz}
#'
#' @description Works out the abatement on a given amount under a given abatement regime
#' against a certain income.  Can be applied only to certain sample elements using
#' the Cond argument, and is fully vectorised by design.
#'
#' @param Cond Boolean vector of which elements are abated.  Can use
#' \code{T} to apply to all elements.
#' @param Amount The amount which is abated.
#' @param Scale An abatement scale (set of rates and thresholds) which is applied to
#' the amount.
#' @param Income An income measure used for determining abatement
#' @return Vector of abated incomes.  Assumes that abated amount is non-negative.
#' @family Utilities
#' @export
Abate <- function(Cond, Amount, Scale, Income){
  Scale_Rows <- nrow(Scale)
  for (Row in 2:Scale_Rows){
    Amount <- Amount - Cond * pmax(0, pmin(Scale[Row, 1], Income) - Scale[Row - 1, 1]) * Scale[Row - 1, 2]
  }
  Amount <- Amount - Cond * pmax(0, Income - Scale[Scale_Rows, 1]) * Scale[Scale_Rows, 2]
  return(pmax(0, Amount))
}
attr(Abate, "utility") <- T

#' @title Abatement Vanishing Point
#' @author Christopher Ball, \email{Christopher.Ball@@treasury.govt.nz}
#'
#' @description Works out the minimum income amount under a given scale at which
#' the abated amount is $0.  Assumes that such a point exists.  Not vectorised,
#' but it doesn't in general need to be as a vector can be made through recycling.
#'
#' @inheritParams Abate
#' @return Abatement vanishing point.
#' @family Utilities
#' @export
Abatement_Vanishing_Point <- function(Scale, Amount){
  # Not vectorised - but it doesn't need to be!
  n <- nrow(Scale)
  for (Th in 1:(n-1)){
    if (Scale[Th, 2] == 0) next
    if (Scale[Th, 1] + Amount/Scale[Th, 2] > Scale[Th + 1, 1]){
      Amount = Amount - Scale[Th, 2]*(Scale[Th + 1, 1] - Scale[Th, 1])
    } else {
      return(Scale[Th, 1] + Amount/Scale[Th, 2])
    }
  }
  return(Scale[n, 1] + Amount/Scale[n, 2])
}
attr(Abatement_Vanishing_Point, "utility") <- T

#' @title Applies a given tax system to an income amount.
#' @author Christopher Ball, \email{Christopher.Ball@@treasury.govt.nz}
#'
#' @description This function calculates the tax amount due given the pre-tax
#' amount and the relevant tax scale.  Vectorised for efficiency.
#'
#' @inheritParams Gross_From_Net
#' @return Vector of tax amounts.
#' @family Utilities
#' @export
Apply <- function(Amount, Tax_Scale){
  Tax <- 0
  n <- nrow(Tax_Scale)
  for (Th in 1:(n-1)){
    Tax <- Tax + (pmin(pmax(Amount, Tax_Scale[Th, 1]), Tax_Scale[Th + 1, 1]) - Tax_Scale[Th, 1])*Tax_Scale[Th, 2]
  }
  return(Tax + pmax(0, Amount - Tax_Scale[n, 1])*Tax_Scale[n, 2])
}
attr(Apply, "utility") <- T



Gross_From_Net <- function(Amount, Net_Thresholds, Tax_Scale){
  Levels <- length(Net_Thresholds)
  Old <- 0
  Gross <- 0*Amount
  for (Th in 2:Levels){
    Gross <- Gross + (Amount < Net_Thresholds[Th] & Amount >= Net_Thresholds[Th - 1])*
      (Tax_Scale[Th-1, 1] + (Amount - Old) / (1 - Tax_Scale[Th - 1, 2]))
    #print(Tax_Scale[Th-1, 1])
    #print((Amount - Old) / (1 - Tax_Scale[Th - 1, 2]))
    Old <- Net_Thresholds[Th]
  }
  return(Gross + (Amount > Net_Thresholds[Levels]) * 
           (Tax_Scale[Levels, 1] + (Amount - Old) / (1 - Tax_Scale[Levels, 2])))
}
attr(Gross_From_Net, "utility") <- T


#' @title Calculates the net income given gross income and a tax scale.
#' @author Christopher Ball, \email{Christopher.Ball@@treasury.govt.nz}
#'
#' @description This function calculates the net of tax income given the pre-tax
#' amount and the relevant tax scale.  Vectorised for efficiency.
#'
#' @inheritParams Gross_From_Net
#' @return Vector of net of tax income amounts.
#' @seealso Apply
#' @family Utilities
#' @export
Net_From_Gross <- function(Amount, Tax_Scale){
  return(Amount - Apply(Amount, Tax_Scale))
}
attr(Net_From_Gross, "utility") <- T




