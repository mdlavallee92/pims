#' Create a PimsAnalysis object
#'
#' Constructs and returns a `PimsAnalysis` object
#'
#' @param analysisCohorts A list of `CohortInfo` objects (required).
#' @param prevalence A `PrevalenceOptions` object (optional).
#' @param incidence An `IncidenceOptions` object (optional).
#' @param mortality A `MortalityOptions` object (optional).
#' @param strata A `DemographicStrata` object (optional).
#'
#' @return A `PimsAnalysis` R6 object.
#' @export
createPimsAnalysis <- function(
    analysisCohorts,
    prevalence = NULL,
    incidence = NULL,
    mortality = NULL,
    strata = NULL
) {
  PimsAnalysis$new(
    analysisCohorts = analysisCohorts,
    prevalence = prevalence,
    incidence = incidence,
    mortality = mortality,
    strata = strata
    )
}

#' Create a CohortInfo object
#'
#' Constructs a `CohortInfo` object from a cohort ID and name.
#'
#' @param id Integer. The cohort definition ID.
#' @param name Character. The name of the cohort.
#'
#' @return A `CohortInfo` R6 object.
#' @export
createCohortInfo <- function(id, name) {
  CohortInfo$new(id = id, name = name)
}

#' Create a DemographicStrata object
#'
#' @param strataOptions Character vector subset of c("age", "gender", "race", "ethnicity", "location")
#'
#' @return A `DemographicStrata` R6 object
#' @export
createStrataOptions <- function(strataOptions = c("age", "gender")) {
  DemographicStrata$new(strataOptions = strataOptions)
}

#' Create an IncidenceAnalysis object
#'
#' Constructs an `IncidenceAnalysis` object with the specified settings.
#'
#' @param cohortOfInterest `CohortInfo` object of interest (required).
#' @param periodOfInterest Numeric vector specifying the period of interest (required).
#' @param lookbackOptions A `LookBackOption` object (required).
#' @param incidenceType Character string: one of `"proportion"`, `"rate"`, or `"both"` (required).
#' @param reportMultiplier Numeric value used for scaling the incidence report (optional).
#' @param strata A `DemographicStrata` object (optional).
#' @param populationStandardization A `PopulationStandardization` object (optional).
#'
#' @return An `IncidenceAnalysis` R6 object.
#' @export
createIncidenceAnalysis <- function(
    periodOfInterest,
    incidenceType = "rate",
    reportMultiplier = 100000,
    populationStandardization = NULL
) {
  IncidenceAnalysis$new(
    periodOfInterest = periodOfInterest,
    incidenceType = incidenceType,
    reportMultiplier = reportMultiplier,
    populationStandardization = populationStandardization
  )
}

#' Create an AnnualPrevalenceAnalysis object
#'
#' Constructs an `AnnualPrevalenceAnalysis` object with the specified settings.
#'
#' @param cohortOfInterest `CohortInfo` object of interest (required).
#' @param periodOfInterest Numeric vector specifying the period of interest (required).
#' @param lookbackOptions A `LookBackOption` object (required).
#' @param incidenceType Character string: one of `"proportion"`, `"rate"`, or `"both"` (required).
#' @param reportMultiplier Numeric value used for scaling the incidence report (optional).
#' @param strata A `DemographicStrata` object (optional).
#' @param populationStandardization A `PopulationStandardization` object (optional).
#'
#' @return An `AnnualPrevalenceAnalysis` R6 object.
#' @export
createAnnualPrevalenceAnalysis <- function(
    periodOfInterest,
    reportMultiplier = 100000,
    populationStandardization = NULL
) {
  AnnualPrevalenceAnalysis$new(
    periodOfInterest = periodOfInterest,
    reportMultiplier = reportMultiplier,
    populationStandardization = populationStandardization
  )
}

#' Create a PeriodOfInterest object
#'
#' @param startYear Integer start year.
#' @param endYear Integer end year.
#'
#' @return A `PeriodOfInterest` R6 object.
#' @export
createPeriodOfInterest <- function(startYear, endYear) {
  PeriodOfInterest$new(startYear = startYear, endYear = endYear)
}
