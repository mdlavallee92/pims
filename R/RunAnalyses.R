#' Generate SQL to extract observation periods for a target cohort
#'
#' Renders and translates the SQL to retrieve observation period data.
#'
#' @param connectionDetails A `connectionDetails` object created by
#'   `DatabaseConnector::createConnectionDetails()`
#' @param cdmDatabaseSchema The schema name where the data resides.
#' @param cohortDatabaseSchema The schema name where the cohort table is located.
#' @param cohortTable The name of the cohort table containing the target cohort.
#' @param startYear An integer specifying the start year for filtering observation periods.
#' @param endYear An integer specifying the end year for filtering observation periods.
#' @param required_op_length An integer indicating the minimum required length (in days)
#'   of observation periods to include (default is 0, meaning no minimum length).
#' @param targetCohortId An integer specifying the cohort ID of interest
#' @param tempEmulationSchema Optional; a schema used to emulate temporary tables if the
#'   database platform does not support them natively. Default is obtained from the
#'   R option `"sqlRenderTempEmulationSchema"`.
#'
#' @export
#'

renderObservationPeriod <- function(connectionDetails,
                                    cdmDatabaseSchema,
                                    cohortDatabaseSchema,
                                    cohortTable,
                                    startYear,
                                    endYear,
                                    required_op_length = 0,
                                    targetCohortId,
                                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")){

  sql <- SqlRender::loadRenderTranslateSql(
    "observationPeriod.sql",
    "pims",
    dbms = connectionDetails$dbms,
    cdm_database_schema = "main",
    cohort_database_schema = "main",
    cohort_table = "cohort",
    start_year = startYear,
    end_year = endYear,
    required_op_length = required_op_length,
    target_cohort_id = targetCohortId,
    tempEmulationSchema = tempEmulationSchema,
    warnOnMissingParameters = TRUE
  )
  return(sql)
}

#' Run incidence analysis for a target cohort
#'
#' Executes an incidence analysis on a specified cohort.
#'
#' @param connectionDetails A `connectionDetails` object created by
#'   `DatabaseConnector::createConnectionDetails()`
#' @param cdmDatabaseSchema The schema name where the data resides.
#' @param cohortDatabaseSchema The schema name where the cohort table is located.
#' @param cohortTable The name of the cohort table containing the target cohort.
#' @param targetCohort An integer specifying the cohort ID of interest.
#' @param incidenceAnalysis An `IncidenceOptions` object specifying incidence analysis settings.
#' @param strata A `DemographicStrata` object specifying the columns to stratify results by.
#' @param tempEmulationSchema Optional; schema used to emulate temporary tables if needed.
#'   Defaults to the value of `getOption("sqlRenderTempEmulationSchema")`.
#' @export

runIncidenceAnalysis <- function(connectionDetails,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 cohortTable,
                                 targetCohort,
                                 incidenceAnalysis,
                                 strata,
                                 tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")){

  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- renderObservationPeriod(connectionDetails = connectionDetails,
                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                 cohortTable = cohortTable,
                                 startYear = incidenceAnalysis$periodOfInterest$startYear,
                                 endYear = incidenceAnalysis$periodOfInterest$endYear,
                                 targetCohortId = targetCohort$id())

  DatabaseConnector::executeSql(connection, sql)

  sql <- SqlRender::loadRenderTranslateSql(
    "incidenceRate.sql",
    "pims",
    dbms = connectionDetails$dbms,
    multiplier = incidenceAnalysis$reportMultiplier,
    strata = strata
  )
  DatabaseConnector::executeSql(connection, sql)

  results <- DatabaseConnector::dbGetQuery(connection, "SELECT * FROM incidence_rate;") |> tibble::as_tibble()

  DatabaseConnector::disconnect(connection)

  return(results)

}

#' Run Annual Prevalence Analysis for a Target Cohort
#'
#' Executes an annual prevalence analysis on a specified cohort within an OMOP CDM database.
#'
#' @param connectionDetails A `connectionDetails` object created by
#'   `DatabaseConnector::createConnectionDetails()`
#' @param cdmDatabaseSchema The schema name where the data resides.
#' @param cohortDatabaseSchema The schema name where the cohort table is located.
#' @param cohortTable The name of the cohort table containing the target cohort.
#' @param targetCohort An integer specifying the cohort ID of interest.
#' @param prevalenceAnalysis An `AnnualPrevalenceAnalysis` object specifying incidence analysis settings.
#' @param strata A `DemographicStrata` object specifying the columns to stratify results by.
#' @param tempEmulationSchema Optional; schema used to emulate temporary tables if needed.
#'   Defaults to the value of `getOption("sqlRenderTempEmulationSchema")`.
#' @export

runAnnualPrevalenceAnalysis <- function(connectionDetails,
                                        cdmDatabaseSchema,
                                        cohortDatabaseSchema,
                                        cohortTable,
                                        targetCohort,
                                        prevalenceAnalysis,
                                        strata,
                                        tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")){

  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- renderObservationPeriod(connectionDetails = connectionDetails,
                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                 cohortTable = cohortTable,
                                 startYear = prevalenceAnalysis$periodOfInterest$startYear,
                                 endYear = prevalenceAnalysis$periodOfInterest$endYear,
                                 targetCohortId = targetCohort$id())

  DatabaseConnector::executeSql(connection, sql)

  sql <- SqlRender::loadRenderTranslateSql(
    "pointPrevalence.sql",
    "pims",
    dbms = connectionDetails$dbms,
    strata = strata
  )
  DatabaseConnector::executeSql(connection, sql)

  results <- DatabaseConnector::dbGetQuery(connection, "SELECT * FROM point_prevalence;") |> tibble::as_tibble()

  DatabaseConnector::disconnect(connection)

  return(results)

}

#' Run PIMS Analysis
#'
#' Executes the PIMS analysis using the specified options.
#'
#' @param connectionDetails A `connectionDetails` object created by
#'   `DatabaseConnector::createConnectionDetails()`
#' @param cdmDatabaseSchema The schema name where the data resides.
#' @param cohortDatabaseSchema The schema name where the cohort table is located.
#' @param cohortTable The name of the cohort table containing the target cohort.
#' @param pimsAnalysis A `PimsAnalysis` object specifying analysis settings.
#' @param tempEmulationSchema Optional; schema used to emulate temporary tables if needed.
#'   Defaults to the value of `getOption("sqlRenderTempEmulationSchema")`.
#' @export

runPimsAnalysis <- function(connectionDetails,
                            cdmDatabaseSchema,
                            cohortDatabaseSchema,
                            cohortTable,
                            pimsAnalysis,
                            tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")){

  # Clone so the original stays unmodified
  pimsResult <- pimsAnalysis$clone(deep = TRUE)

  if(!is.null(pimsResult$incidence)){
    targetCohort <- pimsResult$analysisCohorts[[1]] #TODO: Match on target cohort id or name in sep argument
    incidenceResult <- runIncidenceAnalysis(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      targetCohort = targetCohort,
      incidenceAnalysis = pimsResult$incidence,
      tempEmulationSchema = tempEmulationSchema,
      strata = pimsAnalysis$strata$strataOptions)

    pimsResult$incidence$results <- incidenceResult

  }

  if(!is.null(pimsResult$prevalence)){
    targetCohort <- pimsResult$analysisCohorts[[1]] #TODO: Match on target cohort id or name in sep argument
    prevalenceResult <- runAnnualPrevalenceAnalysis(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      targetCohort = targetCohort,
      prevalenceAnalysis = pimsResult$prevalence,
      tempEmulationSchema = tempEmulationSchema,
      strata = pimsAnalysis$strata$strataOptions)

    pimsResult$prevalence$results <- prevalenceResult
    }

  return(pimsResult)

}
