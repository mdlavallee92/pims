#' @title
#' creates a LookBackOption class with type "any", NULL fixed lookback,
#' and FALSE require observe
#' @param anyLookBack The Analysis object to used for generation
#' @param days The number of days used for any look back
#' @param requiredObserved Require lookback time be all observed
#'
#' @return A list containing a tibble for categorical and continuous results
#'
#' @export

anyLookBack <- function(requiredObserved = FALSE) {


  anyLookBack <- PimsAnalysis$lookBackType(
      type = "any",
      requiredObserved = requiredObserved
    )


}

#' @title
#' creates a LookBackOption class with type "fixed", N
#' @param fixedLookBack The Analysis object to used for generation
#' @param days The number of days used for fixed look back
#' @param requiredObserved Require lookback time be all observed
#'
#' @return A list containing a tibble for categorical and continuous results
#'
#' @export


fixedLookBack <- function (days = 365, requiredObserved = TRUE) {

  if (!is.numeric(days)) stop("Days must be numeric")

  fixedLookBack <- PimsAnalysis$lookBackType(
    type = "fixed",
    days = days,
    requiredObserved = requiredObserved
  )

}

#' @title
#' creates a pointPrevalence specification
#' @param denom Denominator type: either "sufficient","Day1", or "complete"
#' @param n Number of days for prevalence window (required for "sufficient" denominator)
#' @param reportMult Multiplier for reporting (default: 100,000)
#'
#' @return A point prevalence configuration object
#'
#' @export

pointPrevalence <- function(denom = c("sufficient", "complete", "Day1"), n = NULL, reportMult = 100000) {

  denom <- match.arg(denom)

  if(denom == "sufficient") {
    if (is.null(n)) n <- 30
    stopifnot(is.numeric(n), length(n) == 1, n > 0)
  }

  if(denom == "Day1") {
    if (!is.null(n)) stop("When denom = 'Day1', n must be NULL")
  }

  if(denom == "complete") {
    if (!is.null(n)) stop("When denom = 'complete', n must be NULL")
  }

  stopifnot(is.numeric(reportMult), length(reportMult) == 1, reportMult > 0)

  pointPrevalence <- PimsAnalysis$pointPrevalenceType(
    denom = denom,
    n = n,
    reportMult = reportMult
  )

}

#' Point prevalence with "sufficient" denominator
#' @export
pointPrevalenceSufficient <- function(n=30, reportMult = 1e5) {
  pointPrevalence(denom = "sufficient",
                  n = n,
                  reportMult = reportMult)
}


#' Point prevalence with "Day1" denominator
#' @export
pointPrevalenceDay1 <- function(n = NULL, reportMult = 1e5) {
  pointPrevalence(denom = "Day1",
                  n = n,
                  reportMult = reportMult)
}

