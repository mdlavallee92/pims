/* Create year interval table for analysis years
*/
IF OBJECT_ID('#year_interval', 'U') IS NOT NULL
  DROP TABLE #year_interval;

CREATE TABLE #year_interval (calendar_year INTEGER);
INSERT INTO #year_interval (calendar_year) 
VALUES (@start_year), (@start_year + 1), (@start_year + 2), (@start_year + 3),
       (@start_year + 4), (@start_year + 5), (@start_year + 6), (@start_year + 7),
       (@start_year + 8), (@start_year + 9), (@start_year + 10), (@start_year + 11),
       (@start_year + 12), (@start_year + 13), (@start_year + 14), (@start_year + 15);

/* Find eligible observation periods per patient
1) does the op have a minimum amount of time
2) first op or any op
*/
IF OBJECT_ID('#obsPop', 'U') IS NOT NULL
  DROP TABLE #obsPop;

SELECT person_id, observation_period_start_date, observation_period_end_date, 
    DATEADD(day, 365, observation_period_start_date) as cohort_entry_date
INTO #obsPop
FROM (
    SELECT *,
    ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY observation_period_start_date, observation_period_end_date) AS ob_row
    FROM @cdm_database_schema.observation_period
    WHERE DATEDIFF(day, observation_period_start_date, observation_period_end_date) >= @required_op_length
)
--WHERE ob_row = 1  -- Take first observation period per person
;

-- Get counts
SELECT COUNT(DISTINCT person_id) as num_subjects, COUNT(person_id) as num_records FROM #obsPop;

/* Cross-join observation population with calendar years to create person-year combinations
*/
IF OBJECT_ID('#obsPop2', 'U') IS NOT NULL
  DROP TABLE #obsPop2;

SELECT
    a.person_id,
    b.calendar_year,
    DATEFROMPARTS(b.calendar_year, 1, 1) AS calendar_start_date,
    DATEFROMPARTS(b.calendar_year, 12, 31) AS calendar_end_date,
    a.cohort_entry_date,
    a.observation_period_start_date,
    a.observation_period_end_date
INTO #obsPop2
FROM #obsPop a
INNER JOIN #year_interval b /* Join on year interval to get all year combinations in period of interest*/
    ON YEAR(a.cohort_entry_date) <= b.calendar_year 
    AND YEAR(a.observation_period_end_date) >= b.calendar_year
;

SELECT COUNT(DISTINCT person_id) as num_subjects, COUNT(person_id) as num_records FROM #obsPop2;

/* Join filtered op to year intervals, patient demographics and target table
Creates final analysis dataset with:
a) cross join with years (person-year combinations)
b) patient demographics
c) left join ops to target table to get all patients and id case events 
*/
DROP TABLE IF EXISTS #obsPop2;  -- Drop the intermediate obsPop2

SELECT
    op2.person_id,
    CASE WHEN b.cohort_definition_id IS NOT NULL THEN 1 ELSE 0 END as case_event,
    op2.calendar_year,
    op2.calendar_start_date,
    op2.calendar_end_date,
    op2.cohort_entry_date AS index_date,
    COALESCE(b.cohort_start_date, op2.calendar_start_date) AS event_date,
    COALESCE(b.cohort_end_date, op2.observation_period_end_date) AS cohort_end_date,
    op2.observation_period_start_date,
    op2.observation_period_end_date,
    op2.age,
    op2.gender_concept_id,
    op2.race_concept_id,
    op2.ethnicity_concept_id
INTO #obsPop2  -- Reuse obsPop2 name instead of obsPop_final
FROM (
    SELECT
        op.person_id,
        op.calendar_year,
        op.cohort_entry_date,
        op.calendar_start_date,
        op.calendar_end_date,
        op.observation_period_start_date,
        op.observation_period_end_date,
        c.gender_concept_id,
        c.race_concept_id,
        c.ethnicity_concept_id,
        (op.calendar_year - c.year_of_birth) AS age
    FROM #obsPop2 op  -- Reference the intermediate obsPop2
    INNER JOIN @cdm_database_schema.person c ON op.person_id = c.person_id
) op2
LEFT JOIN (
    SELECT * FROM @cohort_database_schema.@cohort_table 
    WHERE cohort_definition_id = @target_cohort_id
) b
    ON op2.person_id = b.subject_id
    AND b.cohort_start_date <= op2.calendar_end_date
    AND (b.cohort_start_date >= op2.observation_period_start_date 
         AND b.cohort_end_date <= op2.observation_period_end_date)
;

-- Final summary uses obsPop2
SELECT COUNT(DISTINCT person_id) as num_subjects, 
       COUNT(person_id) as num_records,
       SUM(case_event) as num_cases
FROM #obsPop2;