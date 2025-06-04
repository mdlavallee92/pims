# Pims analysis class ----------------
PimsAnalysis <- R6::R6Class(
  classname = "PimsAnalysis",
  public = list(

    initialize = function(
      analysisCohorts,
      periodOfInterest,
      lookbackOptions,
      prevalence = NULL,
      incidence = NULL,
      mortality = NULL,
      strata = NULL,
      populationStandarization = NULL
    ) {

      # set analysis Cohort
      checkmate::assert_list(x = analysisCohort, types = "CohortInfo", null.ok = FALSE, min.len = 1)
      private[['analysisCohorts']] <- analysisCohorts

      # set period of interest
      checkmate::assert_numeric(x = periodOfInterest)
      private[['periodOfInterest']] <- periodOfInterest

      # set lookback options
      checkmate::assert_class(x = lookbackOptions, classes = "LookBackOption")
      private[['lookbackOptions']] <- lookbackOptions

      # set prevalence
      checkmate::assert_class(x = prevalence, classes = "Prevalence", null.ok = TRUE)
      private[['prevalence']] <- prevalence

      # set incidence
      checkmate::assert_class(x = incidence, classes = "Incidence", null.ok = TRUE)
      private[['incidence']] <- incidence

      # set mortality
      checkmate::assert_class(x = mortality, classes = "Mortality", null.ok = TRUE)
      private[['mortality']] <- mortality

      # set strata
      checkmate::assert_class(x = strata, classes = "DemographicStrata", null.ok = TRUE)
      private[['strata']] <- strata

      # set population standardization
      checkmate::assert_class(x = populationStandarization, classes = "PopulationStandarization", null.ok = TRUE)
      private[['populationStandarization']] <- populationStandarization

    }
  ),
  private = list(
    analysisCohorts = NULL,
    periodOfInterest = NULL,
    lookbackOptions = NULL,
    prevalence = NULL,
    incidence = NULL,
    mortality = NULL,
    strata = NULL,
    populationStandarization = NULL
  )
)

# Cohort Info -------------------
CohortInfo <- R6::R6Class(
  classname = "CohortInfo",
  public = list(
    #' @param id the cohort definition id
    #' @param name the name of the cohort definition
    initialize = function(id, name) {
      .setNumber(private = private, key = "id", value = id)
      .setString(private = private, key = "name", value = name)
    },
    #' @description get the cohort id
    getId = function() {
      cId <- private$id
      return(cId)
    },
    #' @description get the cohort name
    getName = function() {
      cName <- private$name
      return(cName)
    },
    #' @description print the cohort details
    cohortDetails = function(){
      id <- self$getId()
      name <- self$getName()
      info <- glue::glue_col( "\t- Cohort Id: {green {id}}; Cohort Name: {green {name}}")
      return(info)
      }
     ),
   private = list(
    id = NULL,
    name = NULL
   )
)

# LookBackOption -------------------------
# R6 Class for lookback option
LookBackOption <- R6::R6Class(
  classname = "LookBackOption",
  public = list(
    initialize = function(
    lookbackType,
    fixedLookBackTime,
    requireObservedLookBack) {

      # set option of lookback type
      checkmate::assert_choice(x = lookbackType, choices = c("any", "fixed"))
      private[['.lookbackType']] <- lookbackType

      # set time of fixed lookback
      checkmate::assert_numeric(x = lookbackType, null.ok = TRUE)
      private[['.fixedLookBackTime']] <- fixedLookBackTime

      # set option to require that lookback time be all observed
      checkmate::assert_logical(x = requireObservedLookBack, null.ok = TRUE)
      private[['.requireObservedLookBack']] <- requireObservedLookBack
    }
  ),

  private = list(
    .lookbackType = NA_character_,
    .fixedLookBackTime = NA_complex_,
    .requireObservedLookBack = NULL
  ),

  active = list(
    lookbackType = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        vv <- private$.lookbackType
        return(vv)
      }

      checkmate::assert_choice(x = value, choices = c("any", "fixed"))
      private[['.lookbackType']] <- value

    },

    fixedLookBackTime = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        vv <- private$.fixedLookBackTime
        return(vv)
      }

      checkmate::assert_numeric(x = value, null.ok = TRUE)
      private[['.fixedLookBackTime']] <- value

    },

    requireObservedLookBack = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        vv <- private$.requireObservedLookBack
        return(vv)
      }

      checkmate::assert_logical(x = value, null.ok = TRUE)
      private[['.requireObservedLookBack']] <- value

    }

  )
)

# Prevalence ---------------------
PrevalenceOptions <- R6::R6Class(
  classname = "PrevalenceOptions",
  public = list(

    initialize = function(
      prevalenceType,
      denominatorType,
      n = NULL,
      reportMultiplier
    ) {
      # set option of prevalenceType
      checkmate::assert_choice(x = prevalenceType, choices = c("point", "period"))
      private[['.prevalenceType']] <- prevalenceType
    }
  )
)


# Incidence Class
# type = "proportion", "rate", "both"
# reportMultiplier

# Mortality Class
# when = "earliest" or "indicident"
# reportMultiplier
# SMR/survival not right now

# DemographicStrata
# options: age, gender, race, ethnicity, location
# or age: TRUE; gender: TRUE

# populationStandardization
# reference = 'acs', 'census' 'jpnPop'

