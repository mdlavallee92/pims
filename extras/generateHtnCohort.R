library(Capr)
library(tidyverse)

# Write JSON ----------------------------------
sitagliptin <- cs(
  descendants(1580747),
  name = "sitagliptin"
) %>%
  getConceptSetDetails(connection, cdmDatabaseSchema)

newUserCohort <- cohort(
  entry = entry(drugExposure(sitagliptin, firstOccurrence())),

  exit = exit(endStrategy = drugExit(sitagliptin, persistenceWindow = 30, surveillanceWindow = 0))
)

json <- as.json(newUserCohort)
cat(json, file = "inst/cohorts/sitagliptin.json")

hypertensiveDisorder <- cs(
  descendants(316866),
  name = "Hypertensive disorder"
)
hypertensiveDisorder <- getConceptSetDetails(hypertensiveDisorder, connection, cdmDatabaseSchema)

cohort <- cohort(
  entry = entry(
    conditionOccurrence(conceptSet = hypertensiveDisorder),
    primaryCriteriaLimit = "All"
  ),
  attrition = attrition(
    'sitagliptin' = withAll(exactly(1,
                                    drugExposure(sitagliptin, firstOccurrence())
                                    )
                            ),
    expressionLimit = "All"),
  exit = exit(
    endStrategy = observationExit()
  )
)

json <- as.json(cohort)
cat(json, file = "inst/cohorts/hypertension.json")


# Build cohorts (Requires connectionDetails) ------------------------------
json <- readChar("inst/cohorts/sitagliptin.json", file.info("inst/cohorts/sitagliptin.json")$size)
sql <- CirceR::buildCohortQuery(
  expression = CirceR::cohortExpressionFromJson(json),
  options = CirceR::createGenerateOptions(generateStats = FALSE)
)
cohortDefinitionSet <- tibble(
  cohortId = 1580747,
  cohortName = "Sitagliptin",
  json = json,
  sql = sql
)

json <- readChar("inst/cohorts/hypertension.json", file.info("inst/cohorts/hypertension.json")$size)
sql <- CirceR::buildCohortQuery(
  expression = CirceR::cohortExpressionFromJson(json),
  options = CirceR::createGenerateOptions(generateStats = FALSE)
)
cohortDefinitionSet[2, ] <- tibble(
  cohortId = 316866,
  cohortName = "hypertension",
  json = json,
  sql = sql
)


cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
CohortGenerator::createCohortTables(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTableNames = cohortTableNames
)
CohortGenerator::generateCohortSet(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTableNames = cohortTableNames,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDefinitionSet = cohortDefinitionSet
)

# Check counts ----------------------------------
cohortCounts <- CohortGenerator::getCohortCounts(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTableNames$cohortTable
)
cohortCounts
