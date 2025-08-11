library(Eunomia)
library(DatabaseConnector)
library(SqlRender)

# options(sqlRenderTempEmulationSchema = "main")

connectionDetails <- getEunomiaConnectionDetails()
connection <- connect(connectionDetails)

createCohorts(
  connectionDetails,
  cdmDatabaseSchema = "main",
  cohortDatabaseSchema = "main",
  cohortTable = "cohort"
)

#Step 1: Cohort options
cohortInfo <- createCohortInfo(id = 1,
                               name = "Celecoxib")
cohortInfoList <- list(cohortInfo)

# Step 1: Incidence analysis options
strataOptions <- createStrataOptions(c("age", "gender", "race")) #options: age, gender, race, location, ethnicity
periodOfInterest <- createPeriodOfInterest(1950, 2025)

# Step 2: Create incidence/prevalence options
incidenceAnalysis <- createIncidenceAnalysis(periodOfInterest = periodOfInterest,
                                             incidenceType = "rate",
                                             reportMultiplier = 1e+05)
prevalenceAnalysis <- createAnnualPrevalenceAnalysis(periodOfInterest = periodOfInterest,
                                                     reportMultiplier = 1e+05)

pimsAnalysis <- createPimsAnalysis(analysisCohorts = cohortInfoList,
                                   incidence = incidenceAnalysis,
                                   prevalence = prevalenceAnalysis,
                                   strata = strataOptions)

# Result
pimsResult <- runPimsAnalysis(connectionDetails = connectionDetails,
                              cdmDatabaseSchema = "main",
                              cohortDatabaseSchema =  "main",
                              cohortTable = "cohort",
                              pimsAnalysis = pimsAnalysis)

pimsResult$incidence$results
pimsResult$prevalence$results

# Test prevalence

