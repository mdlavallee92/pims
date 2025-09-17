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
                                    required_op_length,
                                    targetCohortId,
                                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                    outputFolder){

  sqlFile <- file.path(outputFolder, "observationPeriod.sql")

  sql <- SqlRender::loadRenderTranslateSql(
    "observationPeriod.sql",
    "pims",
    dbms = connectionDetails$dbms,
    cdm_database_schema = cdmDatabaseSchema,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    start_year = startYear,
    end_year = endYear,
    required_op_length = required_op_length,
    target_cohort_id = targetCohortId,
    tempEmulationSchema = tempEmulationSchema,
    warnOnMissingParameters = TRUE
  )

  SqlRender::writeSql(sql, sqlFile)
  return(sql)
}

#' Generate SQL to extract observation periods for a target cohort for multi-year analyses
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

renderObservationPeriodSpan <- function(connectionDetails,
                                        cdmDatabaseSchema,
                                        cohortDatabaseSchema,
                                        cohortTable,
                                        required_op_length,
                                        targetCohortId,
                                        tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                        outputFolder){

  connection <- connect(connectionDetails)

  sqlFile <- file.path(outputFolder, "observationPeriodSpan.sql")

  sql <- SqlRender::loadRenderTranslateSql(
    "observationPeriodSpan.sql",
    "pims",
    dbms = connectionDetails$dbms,
    cdm_database_schema = cdmDatabaseSchema,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    required_op_length = required_op_length,
    target_cohort_id = targetCohortId,
    tempEmulationSchema = tempEmulationSchema,
    warnOnMissingParameters = TRUE
  )

  SqlRender::writeSql(sql, sqlFile)
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
                                 strata = NULL,
                                 required_op_length = 0,
                                 tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                 outputFolder){

  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- renderObservationPeriod(connectionDetails = connectionDetails,
                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                 cohortTable = cohortTable,
                                 required_op_length = required_op_length,
                                 startYear = incidenceAnalysis$periodOfInterest$startYear,
                                 endYear = incidenceAnalysis$periodOfInterest$endYear,
                                 targetCohortId = targetCohort$id(),
                                 outputFolder = outputFolder)

  DatabaseConnector::executeSql(connection, sql)

  sqlFile <- file.path(outputFolder, "incidence.sql")

  sql <- SqlRender::loadRenderTranslateSql(
    "incidenceRate.sql",
    "pims",
    strata = strata,
    dbms = connectionDetails$dbms,
    multiplier = incidenceAnalysis$reportMultiplier
  )
  SqlRender::writeSql(sql, sqlFile)

  DatabaseConnector::executeSql(connection, sql)

  results <- DatabaseConnector::dbGetQuery(connection, "SELECT * FROM #incidence_rate;") |> tibble::as_tibble()

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
                                        required_op_length = 0,
                                        strata = NULL,
                                        tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                        outputFolder){

  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- renderObservationPeriod(connectionDetails = connectionDetails,
                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                 cohortTable = cohortTable,
                                 required_op_length = required_op_length,
                                 startYear = prevalenceAnalysis$periodOfInterest$startYear,
                                 endYear = prevalenceAnalysis$periodOfInterest$endYear,
                                 targetCohortId = targetCohort$id(),
                                 outputFolder = outputFolder)

  DatabaseConnector::executeSql(connection, sql)

  sqlFile <- file.path(outputFolder, "annual_prevalence.sql")

  sql <- SqlRender::loadRenderTranslateSql(
    "pointPrevalence.sql",
    "pims",
    dbms = connectionDetails$dbms,
    strata = strata
  )
  SqlRender::writeSql(sql, sqlFile)

  DatabaseConnector::executeSql(connection, sql)

  results <- DatabaseConnector::dbGetQuery(connection, "SELECT * FROM point_prevalence;") |> tibble::as_tibble()

  DatabaseConnector::disconnect(connection)

  return(results)

}

#' Run Span Prevalence Analysis for a Target Cohort
#'
#' Executes a Span prevalence analysis on a specified cohort within an OMOP CDM database.
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

runSpanPrevalenceAnalysis <- function(connectionDetails,
                                      cdmDatabaseSchema,
                                      cohortDatabaseSchema,
                                      cohortTable,
                                      targetCohort,
                                      prevalenceAnalysis,
                                      required_op_length = 0,
                                      spanYears = data.frame(start_year = 2005,
                                                           end_year = 2024),
                                      strata = NULL,
                                      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                      outputFolder){

  connection <- DatabaseConnector::connect(connectionDetails)

  sql <- renderObservationPeriodSpan(connectionDetails = connectionDetails,
                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                     cohortDatabaseSchema = cohortDatabaseSchema,
                                     cohortTable = cohortTable,
                                     required_op_length = required_op_length,
                                     targetCohortId = targetCohort$id(),
                                     outputFolder = outputFolder)

  names(spanYears) <- SqlRender::camelCaseToSnakeCase(names(spanYears))

  DatabaseConnector::dbWriteTable(
    connection,
    "spans",
    value = spanYears,
    temporary = TRUE,
    overwrite = TRUE
  )

  DatabaseConnector::executeSql(connection, sql)

  sqlFile <- file.path(outputFolder, "span_prevalence.sql")

  sql <- SqlRender::loadRenderTranslateSql(
    "spanPrevalence.sql",
    "pims",
    dbms = connectionDetails$dbms,
    strata = strata
    )
  SqlRender::writeSql(sql, sqlFile)

  DatabaseConnector::executeSql(connection, sql)

  results <- DatabaseConnector::dbGetQuery(connection, "SELECT * FROM #span_prevalence;") |> tibble::as_tibble()

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
                            tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                            outputFolder = NULL){

  # Clone so the original stays unmodified
  pimsResult <- pimsAnalysis$clone(deep = TRUE)

  # Create output folder if does not exist
  if(is.null(outputFolder)){
    outputFolder <- file.path(".", paste0("PimsResult", Sys.Date()))
  }

  if (!dir.exists(outputFolder)) {
    # Create the directory if it doesn't exist
    dir.create(outputFolder, recursive = TRUE)
  }

  # Run analyses

  if(pimsResult$strata$strataOptions != ""){
    strata <- paste0(pimsResult$strata$strataOptions, ",")
  } else {
    strata <- pimsResult$strata$strataOptions
  }

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
      strata = strata,
      outputFolder = outputFolder)

    if(pimsResult$strata$strataOptions != ""){
      incidenceResult <- incidenceResult |>
        dplyr::arrange(across(all_of(pimsResult$strata$strataOptions)))
    }

    exportFile <- file.path(outputFolder, "incidenceResult.csv")

    pimsResult$incidence$results <- incidenceResult |>
      computeCiInc()
      exportResult(pimsResult$incidence$results, filePath = exportFile)

  }

  if(!is.null(pimsResult$annualPrevalence)){
    targetCohort <- pimsResult$analysisCohorts[[1]] #TODO: Match on target cohort id or name in sep argument
    annualPrevalenceResult <- runAnnualPrevalenceAnalysis(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      targetCohort = targetCohort,
      prevalenceAnalysis = pimsResult$annualPrevalence,
      tempEmulationSchema = tempEmulationSchema,
      strata = strata,
      outputFolder = outputFolder)

    if(pimsResult$strata$strataOptions != ""){
      annualPrevalenceResult <- annualPrevalenceResult |>
        dplyr::arrange(across(all_of(pimsResult$strata$strataOptions)))
    }

    exportFile <- file.path(outputFolder, "annualPrevalenceResult.csv")

    pimsResult$annualPrevalence$results <- annualPrevalenceResult |>
      computeCiPrev()
      exportResult(pimsResult$annualPrevalence$results, filePath = exportFile)
  }

  if(!is.null(pimsResult$spanPrevalence)){
    targetCohort <- pimsResult$analysisCohorts[[1]] #TODO: Match on target cohort id or name in sep argument
    spanPrevalenceResult <- runSpanPrevalenceAnalysis(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      targetCohort = targetCohort,
      prevalenceAnalysis = pimsResult$spanPrevalence,
      spanYears = pimsResult$spanPrevalence$spanOfInterest$spanYears,
      strata = strata,
      tempEmulationSchema = tempEmulationSchema,
      outputFolder = outputFolder)

    if(pimsResult$strata$strataOptions != ""){
      spanPrevalenceResult <- spanPrevalenceResult |>
        dplyr::arrange(across(all_of(pimsResult$strata$strataOptions)))
    }

    exportFile <- file.path(outputFolder, "spanPrevalenceResult.csv")

    pimsResult$spanPrevalence$results <- spanPrevalenceResult |>
      computeCiPrev()

    exportResult(pimsResult$spanPrevalence$results, filePath = exportFile)

  }

  return(pimsResult)

}
