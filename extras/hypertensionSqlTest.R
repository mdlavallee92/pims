library(DatabaseConnector)
library(SqlRender)

# Connection details -----------------------------------------------------------
Sys.setenv(DATABASECONNECTOR_JAR_FOLDER = keyring::key_get("driver_path"))

options(sqlRenderTempEmulationSchema = keyring::key_get("writableSchema"))

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "spark",
  user = keyring::key_get("user"),
  password = keyring::key_get("databricks"),
  connectionString = paste("jdbc:databricks://",
                           keyring::key_get("databricks_host"),
                           keyring::key_get("databricks_path"),
                           keyring::key_get("databricks_http_path"),
                           ";EnableArrow=1;", sep = "")
)

# database containing the OMOP CDM data
cdmDatabaseSchema <- keyring::key_get("databaseSchema")
# database with read/write access
cohortDatabaseSchema <- keyring::key_get("writableSchema")

# table name where the cohorts will be generated
cohortTable <- 'pims_cohort'

connection <- connect(connectionDetails)


# Test sql ---------------------------------------------------------------------

sql <- loadRenderTranslateSql(
  "observationPeriod.sql",
  "pims",
  dbms = connectionDetails$dbms,
  cdm_database_schema = cdmDatabaseSchema,
  cohort_database_schema = cohortDatabaseSchema,
  cohort_table = cohortTable,
  start_year = 2002,
  end_year = 2023,
  required_op_length = 0,
  target_cohort_id = 316866,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
  warnOnMissingParameters = TRUE
)

executeSql(connection, sql)

sql <- loadRenderTranslateSql(
  "incidenceRate.sql",
  "pims",
  dbms = connectionDetails$dbms,
  multiplier = 100000
)
executeSql(connection, sql)

dbGetQuery(connection, "SELECT * FROM #incidence_rate;")

sql <- loadRenderTranslateSql(
  "pointPrevalence.sql",
  "pims",
  dbms = connectionDetails$dbms)
executeSql(connection, sql)

dbGetQuery(connection, "SELECT * FROM #point_prevalence;")
