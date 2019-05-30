select
    'Nitrite' as chemical,
    COUNT(*) as total,
    (select count(*)
        FROM CHEMDATA
        where chemparameter = 'Nitrite'
            and station_type='River/Stream'
            and duplicate == 0
            AND value is not NULL) as countFilled,
    (select count(*)
        FROM CHEMDATA
        where chemparameter = 'Nitrite'
            and station_type='River/Stream'
            and duplicate == 0
            AND value is NULL) as countNULL
    /*(select count(*)
        FROM CHEMDATA
        where chemparameter = 'Nitrite'
            and station_type='River/Stream'
            and duplicate == 0
            AND value = '') AS countQuotationMarks*/
from chemdata
where chemparameter = 'Nitrite'
and station_type='River/Stream'
and duplicate == 0;