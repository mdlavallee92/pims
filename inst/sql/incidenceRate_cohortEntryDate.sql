/* Do incidence rate */
IF OBJECT_ID('#incidence_rate', 'U') IS NOT NULL
  DROP TABLE #incidence_rate;

SELECT calendar_year, @strata,
  numerator, (denom / 365.25) as years_at_risk,
  CASE WHEN denom > 0 THEN (numerator / (denom / 365.25)) * @multiplier ELSE 0 END AS ir
INTO #incidence_rate
FROM(
  SELECT
    calendar_year, @strata,
    SUM(post_index_event) AS numerator, 
    SUM(time_at_risk) AS denom
  FROM (
    SELECT person_id, rn1, post_index_event,
        calendar_year,
        CASE
        WHEN post_index_event = 1 AND YEAR(cohort_entry_date) = calendar_year THEN DATEDIFF(day, cohort_entry_date, index_date)
        WHEN post_index_event = 0 AND YEAR(cohort_entry_date) = calendar_year
            THEN DATEDIFF(day, cohort_entry_date, DATEFROMPARTS(calendar_year + 1, 1, 1) - 1)
        ELSE DATEDIFF(day, DATEFROMPARTS(calendar_year, 1, 1), DATEFROMPARTS(calendar_year + 1, 1, 1) - 1) END AS time_at_risk,
            @strata
    FROM (
        SELECT
            *,
            ROW_NUMBER() OVER (PARTITION BY person_id, calendar_year ORDER BY index_date, observation_period_start_date) as rn1,
            CASE WHEN case_event = 0 THEN 0
                WHEN case_event = 1 AND (index_date >= DATEFROMPARTS(calendar_year, 1, 1)) THEN 1 ELSE 2 END AS post_index_event
        FROM #obsPop2
    )
    WHERE post_index_event != 2 and rn1 = 1
  )
  GROUP BY calendar_year, @strata
)
;

