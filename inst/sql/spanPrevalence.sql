-- do span prevalence
drop table if exists #span_prevalence;
create table #span_prevalence as
with prev_event as(
    select
        @strata
        o.subject_id,
        o.start_year,
        o.end_year,
        case when
            e.subject_id is not null then 1
            else 0
        end as case_event
    from #obsPopSpan o
    left join #allEvents e
    on o.subject_id = e.subject_id
    and e.cohort_start_date <= o.span_end_date
    and e.cohort_end_date >= o.span_start_date
)
select
    @strata
    start_year,
    end_year,
    count(distinct case when case_event = 1 then subject_id end) as numerator,
    count(distinct subject_id) as denominator,
    numerator / denominator * 100000 as prev
from prev_event
group by @strata start_year, end_year
order by @strata start_year, end_year;
