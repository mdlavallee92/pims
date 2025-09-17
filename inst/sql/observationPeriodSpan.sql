/* Find eligible observation periods per patient
1) does the op have a minimum amount of time
2) do we want the first op or any op
*/
DROP TABLE IF EXISTS #obsPop;
CREATE TEMP TABLE #obsPop
AS
SELECT
subject_id, observation_period_start_date, observation_period_end_date
FROM
(
    SELECT *, person_id as subject_id,
    ROW_NUMBER() OVER (
    PARTITION BY subject_id
    ORDER BY observation_period_start_date, observation_period_end_date)
    AS ob_row
FROM @cdm_database_schema.observation_period
WHERE DATEDIFF(day, observation_period_start_date, observation_period_end_date) >= @required_op_length
)
--WHERE ob_row = 1
;

-- get events table
drop table if exists #allEvents;
create temp table #allEvents
as
SELECT distinct
  COHORT.subject_id,
  COHORT.cohort_start_date,
  COHORT.cohort_end_date,
  OBSPOP.observation_period_start_date,
  OBSPOP.observation_period_end_date,
  COHORT.cohort_definition_id
FROM @cohort_database_schema.@cohort_table COHORT
join #obsPop OBSPOP on COHORT.subject_id = OBSPOP.subject_id
where COHORT.cohort_definition_id = @target_cohort_id
  and COHORT.cohort_start_date >= OBSPOP.observation_period_start_date
  and COHORT.cohort_start_date <= OBSPOP.observation_period_end_date;

-- get obsPop expanded by span
drop table if exists #obsPopSpan;
create table #obsPopSpan as
select
    o.subject_id,
    s.start_year,
    s.end_year,
    to_date(s.start_year || '-01-01') as span_start_date,
    to_date(s.end_year || '-12-31') AS span_end_date,
    s.start_year - p.year_of_birth as age,
    case when
        s.start_year - p.year_of_birth >= 18 THEN '>=18'
        else '< 18'
    end as age_group
from #obsPop o
cross join #spans s
join @cdm_database_schema.person p
    on o.subject_id = p.person_id
where o.observation_period_start_date <= to_date(s.end_year || '-12-31')
and o.observation_period_end_date >= to_date(s.start_year || '-01-01');
