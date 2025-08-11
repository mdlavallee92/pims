renderObservationPeriod <- function(connectionDetails,
                                    cdmDatabaseSchema,
                                    cohortDatabaseSchema,
                                    cohortTable,
                                    startYear,
                                    endYear,
                                    required_op_length = 0,
                                    targetCohortId,
                                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")){

  sql <- loadRenderTranslateSql(
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

runIncidenceAnalysis <- function(connectionDetails,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 cohortTable,
                                 targetCohort,
                                 incidenceAnalysis,
                                 strata,
                                 tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")){

  connection <- connect(connectionDetails)
  sql <- renderObservationPeriod(connectionDetails = connectionDetails,
                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                 cohortTable = cohortTable,
                                 startYear = incidenceAnalysis$periodOfInterest$startYear,
                                 endYear = incidenceAnalysis$periodOfInterest$endYear,
                                 targetCohortId = targetCohort$id())

  executeSql(connection, sql)

  sql <- loadRenderTranslateSql(
    "incidenceRate.sql",
    "pims",
    dbms = connectionDetails$dbms,
    multiplier = incidenceAnalysis$reportMultiplier,
    strata = strata
  )
  executeSql(connection, sql)

  results <- dbGetQuery(connection, "SELECT * FROM incidence_rate;") |> tibble::as_tibble()

  disconnect(connection)

  return(results)

}

runAnnualPrevalenceAnalysis <- function(connectionDetails,
                                        cdmDatabaseSchema,
                                        cohortDatabaseSchema,
                                        cohortTable,
                                        targetCohort,
                                        incidenceAnalysis,
                                        strata,
                                        tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")){

  connection <- connect(connectionDetails)
  sql <- renderObservationPeriod(connectionDetails = connectionDetails,
                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                 cohortTable = cohortTable,
                                 startYear = incidenceAnalysis$periodOfInterest$startYear,
                                 endYear = incidenceAnalysis$periodOfInterest$endYear,
                                 targetCohortId = targetCohort$id())

  executeSql(connection, sql)

  sql <- loadRenderTranslateSql(
    "pointPrevalence.sql",
    "pims",
    dbms = connectionDetails$dbms,
    strata = strata
  )
  executeSql(connection, sql)

  results <- dbGetQuery(connection, "SELECT * FROM point_prevalence;") |> tibble::as_tibble()

  disconnect(connection)

  return(results)

}

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
      incidenceAnalysis = pimsResult$incidence,
      tempEmulationSchema = tempEmulationSchema,
      strata = pimsAnalysis$strata$strataOptions)

    pimsResult$prevalence$results <- prevalenceResult
    }

  return(pimsResult)

}
