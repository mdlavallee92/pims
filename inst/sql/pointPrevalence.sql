/* Do Point Prevalence*/
DROP TABLE IF EXISTS #point_prevalence;
SELECT *, (numerator / denom) * 100000 AS prev
INTO #point_prevalence
FROM (
SELECT calendar_year, gender_concept_id, age, SUM(prevalent_event) AS numerator, SUM(rn1) AS denom
FROM (
SELECT
    person_id,
    ROW_NUMBER() OVER (PARTITION BY person_id, calendar_year ORDER BY index_date, observation_period_start_date) as rn1,
    CASE WHEN case_event = 1 AND (index_date <= calendar_start_date /*AND DATEADD(day, -365, calendar_start_date) <= index_date*/) THEN 1 ELSE 0 END AS prevalent_event, /*no restriction on event*/
    calendar_year, gender_concept_id, age, race_concept_id, ethnicity_concept_id, location_id
FROM #obsPop2
)
WHERE rn1 = 1
GROUP BY calendar_year, gender_concept_id, age /* ONLY group by gender and age */
);
