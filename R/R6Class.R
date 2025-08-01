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
      checkmate::assert_class(x = prevalence, classes = "PrevalenceOptions", null.ok = TRUE)
      private[['prevalence']] <- prevalence

      # set incidence
      checkmate::assert_class(x = incidence, classes = "IncidenceOptions", null.ok = TRUE)
      private[['incidence']] <- incidence

      # set mortality
      checkmate::assert_class(x = mortality, classes = "MortalityOptions", null.ok = TRUE)
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

# Prevalence class ----------------
AnnualPrevalenceAnalysis <- R6::R6Class(
  classname = "AnnualPrevalenceAnalysis",
  public = list(

    initialize = function(
    analysisCohorts,
    periodOfInterest,
    lookbackOptions,
    denominatorType,
    n = NULL,
    reportMultiplier,
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

      # set option of denominatorType
      checkmate::assert_choice(x = denominatorType, choices = c("day1", "complete", "anyTime", "sufficientTime"))
      private[['.denominatorType']] <- denominatorType

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
    .denominatorType = NA_complex_,
    .n = NULL,
    .reportMultiplier = NA_complex_,
    strata = NULL,
    populationStandarization = NULL
  ),
  active = list(

    getDenominatorType = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        vv <- private$.denominatorType
        return(vv)
      }

      checkmate::assert_choice(x = denominatorType, choices = c("day1", "complete", "anyTime", "sufficientTime"))
      private[['.denominatorType']] <- value

    },

    n = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        vv <- private$.n
        return(vv)
      }

      checkmate::assert_numeric(x = value, null.ok = TRUE)
      private[['.n']] <- value

    },

    reportMultiplier = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        vv <- private$.reportMultiplier
        return(vv)
      }

      checkmate::assert_numeric(x = value, null.ok = TRUE)
      private[['.reportMultiplier']] <- value

    }
  )
)

# Prevalence class ----------------
SpanPrevalenceAnalysis <- R6::R6Class(
  classname = "SpanPrevalenceAnalysis",
  public = list(

    initialize = function(
    analysisCohorts,
    periodOfInterest,
    lookbackOptions,
    denominatorType,
    n = NULL,
    reportMultiplier,
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

      # set option of denominatorType
      checkmate::assert_choice(x = denominatorType, choices = c("day1", "complete", "anyTime", "sufficientTime"))
      private[['.denominatorType']] <- denominatorType

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
    .denominatorType = NA_complex_,
    .n = NULL,
    .reportMultiplier = NA_complex_,
    strata = NULL,
    populationStandarization = NULL
  ),
  active = list(

    getDenominatorType = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        vv <- private$.denominatorType
        return(vv)
      }

      checkmate::assert_choice(x = denominatorType, choices = c("day1", "complete", "anyTime", "sufficientTime"))
      private[['.denominatorType']] <- value

    },

    n = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        vv <- private$.n
        return(vv)
      }

      checkmate::assert_numeric(x = value, null.ok = TRUE)
      private[['.n']] <- value

    },

    reportMultiplier = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        vv <- private$.reportMultiplier
        return(vv)
      }

      checkmate::assert_numeric(x = value, null.ok = TRUE)
      private[['.reportMultiplier']] <- value

    }
  )
)

# Incidence analysis class ----------------
IncidenceAnalysis <- R6::R6Class(
  classname = "IncidenceAnalysis",
  public = list(

    initialize = function(
    analysisCohorts,
    periodOfInterest,
    lookbackOptions,
    incidenceType,
    reportMultiplier,
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

      # set option of incidenceType
      checkmate::assert_choice(x = incidenceType, choices = c("proportion", "rate", "both"))
      private[['.incidenceType']] <- incidenceType

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
    .incidenceType = NA_character_,
    .reportMultiplier = NA_complex_,
    strata = NULL,
    populationStandarization = NULL
  ),
  active = list(
    getIncidenceType = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        vv <- private$.incidenceType
        return(vv)
      }

      checkmate::assert_choice(x = value, choices = c("proportion", "rate", "both"))
      private[['.incidenceType']] <- value

    },

    reportMultiplier = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        vv <- private$.reportMultiplier
        return(vv)
      }

      checkmate::assert_numeric(x = value, null.ok = TRUE)
      private[['.reportMultiplier']] <- value

    }
  )
)

# Mortality analysis class ----------------
MortalityAnalysis <- R6::R6Class(
  classname = "MortalityAnalysis",
  public = list(

    initialize = function(
    analysisCohorts,
    periodOfInterest,
    lookbackOptions,
    mortalityType,
    reportMultiplier,
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

      # set option for mortalityType
      checkmate::assert_choice(x = mortalityType, choices = c("earliest", "incident"))
      private[['.mortalityType']] <- mortalityType

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
    .mortalityType = NA_character_,
    .reportMultiplier = NA_complex_,
    strata = NULL,
    populationStandarization = NULL
  ),
  active = list(
    getMortalityType = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        vv <- private$.mortalityType
        return(vv)
      }

      checkmate::assert_choice(x = value, choices = c("earliest","incident"))
      private[['.mortalityType']] <- value

    },

    reportMultiplier = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        vv <- private$.reportMultiplier
        return(vv)
      }

      checkmate::assert_numeric(x = value, null.ok = TRUE)
      private[['.reportMultiplier']] <- value

    }
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

# DemographicStrata --------------------

DemographicStrata <- R6::R6Class(
  classname = "DemographicStrata",
  public = list(

    initialize = function(
    strataOptions
      )
    {
    # set option for strataOptions
      checkmate::assert_subset(x = strataOptions, choices = c("age", "gender", "race", "ethnicity", "location"))
      private[['.strataOptions']] <- strataOptions
    }
  ),
  private = list(
    .strataOptions = NA_character_
  ),

  active = list(
    getStrataOptions = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        vv <- private$.strataOptions
        return(vv)
      }

      checkmate::assert_subset(x = value, choices = c("age", "gender", "race", "ethnicity", "location"))
      private[['.strataOptions']] <- value

    }
  )
)
# options: age, gender, race, ethnicity, location
# or age: TRUE; gender: TRUE

# populationStandardization ----------------

populationStandardization <- R6::R6Class(
  classname = "populationStandardization",
  public = list(

    initialize = function(
      reference)
    {
      # set option for reference
      checkmate::assert_choice(x = reference, choices = c("acs", "census", "jpnPop"))
      private[['.strataOptions']] <- strataOptions
    }
  ),
  private = list(
    .reference = NA_character_
  ),

  active = list(
    getReference = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        vv <- private$.reference
        return(vv)
      }

      checkmate::assert_choice(x = value, choices = c("acs", "census", "jpnPop"))
      private[['.reference']] <- value

    }
  )
)

