/* Where was the information collected (what sites and where)? */
SELECT *
FROM SITES;

/* What chemical parameters were collected? (at each site) */
SELECT CHEMDATA.sta_seq,
       SITES.name as site_name,
       chemparameter
  FROM CHEMDATA
       INNER JOIN
       SITES ON CHEMDATA.sta_seq = SITES.sta_seq
ORDER BY CHEMDATA.sta_seq;
/* (in general) */
SELECT chemparameter
FROM CHEMDATA
GROUP BY chemparameter;
 
/* Does each parameters have the same unit of measurement? */
SELECT chemparameter, uom
FROM CHEMDATA
GROUP BY chemparameter
ORDER BY uom;
 
/* select results from 2012 */
SELECT *
  FROM CHEMDATA
 WHERE collect_date LIKE '%2012';
 
/* How many sites were in each basin? */
SELECT basin.sbasn AS basinID,
       basin.subregion AS basin_region_name,
       COUNT(sites.sbasn) AS site_count
  FROM sites
       INNER JOIN basin ON basinID = sites.sbasn
 GROUP BY sites.sbasn
 ORDER BY site_count DESC;

/* How many sites were collected in more than one year? */
SELECT
    sta_seq,
    COUNT(strftime('%Y', collect_date)) as '# Years Used'
FROM CHEMDATA
HAVING COUNT(strftime('%Y', collect_date)) > 1;