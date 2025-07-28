/* Create Period prevalence*/
/* Prepare numerator for period prevalence */
DROP TABLE IF EXISTS #prev_num;
CREATE TABLE #prev_num AS
SELECT
  cohort_definition_id,
  calendar_year,
  gender_concept_id,
  age,
  {demographic_strata}
  COUNT(DISTINCT subject_id) AS prev_cases
FROM (
    SELECT *
    FROM #cohorts a
    INNER JOIN #obsPop b
    ON a.subject_id = b.person_id
      AND a.cohort_end_date >= b.observation_period_start_date
      AND a.cohort_start_date <= b.calendar_start_date
      AND a.cohort_end_date >= DATEADD(day, -1, b.calendar_end_date) /* event end date must be beyond calendar end date*/
)
GROUP BY cohort_definition_id, calendar_year, gender_concept_id, {demographic_strata} age
;


/* Prepare complete Denominator*/
DROP TABLE IF EXISTS #prev_denom;
CREATE TABLE #prev_denom AS
SELECT
  calendar_year,
  gender_concept_id,
  age,
  {demographic_strata}
  COUNT(person_id) AS num_person
FROM (
    SELECT *
    FROM #obsPop
    WHERE observation_period_end_date >= DATEADD(day, -1, calendar_end_date)
)
GROUP BY calendar_year, gender_concept_id, {demographic_strata} age
;


/*Create prevalence summary table */
DROP TABLE IF EXISTS #prev_tbl;
CREATE TABLE #prev_tbl AS
SELECT
    a.calendar_year,
    a.gender_concept_id,
    a.age,
    {demographic_strata},
    b.cohort_definition_id,
    b.prev_cases,
    a.num_person,
    (b.prev_cases / a.num_person) * @reportMultiplier AS prev
FROM #prev_denom a
LEFT JOIN #prev_num b
    ON a.calendar_year = b.calendar_year
    AND a.gender_concept_id = b.gender_concept_id
    AND a.age = b.age
    {demographic_strata_join}
;
