/* Do incidence rate */
DROP TABLE IF EXISTS #incidence_rate;
CREATE TABLE #incidence_rate AS
SELECT *, (numerator / (denom / 365.25)) * @multiplier AS ir
FROM(
  SELECT
    calendar_year, gender_concept_id, age,
    SUM(post_index_event) AS numerator, SUM(time_at_risk) AS denom
  FROM (
    SELECT person_id, rn1, post_index_event,
        calendar_year,
        CASE WHEN post_index_event = 1 THEN DATEDIFF(day, calendar_start_date, index_date)
            ELSE DATEDIFF(day, calendar_start_date, calendar_end_date) END AS time_at_risk,
            gender_concept_id, age
    FROM (
        SELECT
            *,
            ROW_NUMBER() OVER (PARTITION BY person_id, calendar_year ORDER BY index_date, observation_period_start_date) as rn1,
            CASE WHEN case_event = 0 THEN 0
                WHEN case_event = 1 AND (index_date > calendar_start_date) THEN 1 ELSE 2 END AS post_index_event
        FROM #obsPop2
    )
    WHERE post_index_event != 2 and rn1 = 1
  )
  GROUP BY calendar_year, gender_concept_id, age /* ONLY group by gender and age */
)
;
