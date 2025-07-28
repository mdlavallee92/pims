/* Find eligible observation periods per patient
1) does the op have a minimum amount of time
2) do we want the first op or any op
*/
DROP TABLE IF EXISTS #obsPop;
CREATE TEMP TABLE #obsPop AS
SELECT person_id, observation_period_start_date, observation_period_end_date
FROM (
    SELECT *,
    ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY observation_period_start_date, observation_period_end_date) AS ob_row
FROM @cdm_database_schema.observation_period
WHERE DATEDIFF(day, observation_period_start_date, observation_period_end_date) >= @required_op_length
)
--WHERE ob_row = 1
;

/* Join filtered op to year intervals, patient demographics and target table
options:
1) do we want any event or only events observed in the observation period?
tasks:
a) cross join with years
b) get patient demographics
c) left join ops to target table get all patients and id case events
*/
DROP TABLE IF EXISTS #obsPop2;
CREATE TEMP TABLE #obsPop2 AS
SELECT
    op2.person_id,
    CASE WHEN b.cohort_definition_id IS NOT NULL THEN 1 ELSE 0 END as case_event,
    op2.calendar_year,
    op2.calendar_start_date,
    op2.calendar_end_date,
    COALESCE(b.cohort_start_date, op2.calendar_start_date) AS index_date,
    COALESCE(b.cohort_end_date, op2.observation_period_end_date) AS cohort_end_date,
    op2.observation_period_start_date,
    op2.observation_period_end_date,
    op2.age,
    op2.gender_concept_id,
    op2.race_concept_id,
    op2.ethnicity_concept_id,
    op2.location_id
FROM (
    SELECT
        op.person_id,
        op.calendar_year,
        op.calendar_start_date,
        op.calendar_end_date,
        op.observation_period_start_date,
        op.observation_period_end_date,
        c.gender_concept_id,
        (op.calendar_year - c.year_of_birth) AS age,
        c.race_concept_id,
        c.ethnicity_concept_id,
        c.location_id
    FROM (
        SELECT
            a.person_id,
            b.calendar_year,
            DATEFROMPARTS(b.calendar_year, 1, 1) AS calendar_start_date,
          	DATEFROMPARTS(b.calendar_year + 1, 1, 1) AS calendar_end_date,
            a.observation_period_start_date,
            a.observation_period_end_date
        FROM #obsPop a
        INNER JOIN #year_interval b /* Join on year interval to get all year combinations in POI*/
          ON (YEAR(a.observation_period_start_date) <= b.calendar_year AND YEAR(a.observation_period_end_date) >= b.calendar_year)
    ) op
    INNER JOIN @cdm_database_schema.person c ON op.person_id = c.person_id /* Join on person table for demographics*/
) op2
LEFT JOIN (
    SELECT * FROM @cohort_database_schema.@cohort_table WHERE cohort_definition_id = @target_cohort_id
) b
    ON op2.person_id = b.subject_id
    AND b.cohort_start_date <= op2.calendar_end_date /* remove events that take place after calendar dates*/
    --AND (b.cohort_start_date >= op2.observation_period_start_date AND b.cohort_end_date <= op2.observation_period_end_date) /* only observed events*/
;
