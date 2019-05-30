/*  
note: this query uses the extension-functions.dll, which contains the following functions:
Math: acos, asin, atan, atn2, atan2, acosh, asinh, atanh, difference, degrees, radians, cos, sin, tan, cot,
    cosh, sinh, tanh, coth, exp, log, log10, power, sign, sqrt, square, ceil, floor, pi.
String: replicate, charindex, leftstr, rightstr, ltrim, rtrim, trim, replace, reverse, proper, padl, padr,
    padc, strfilter.
Aggregate: stdev, variance, mode, median, lower_quartile, upper_quartile.
*/
DROP TABLE IF EXISTS temp_chem_summary;

CREATE TABLE temp_chem_summary (
    'M_BASIN' CHAR   UNIQUE
                        PRIMARY KEY
                        NOT NULL,
    'MIN' REAL,
    'Q1' REAL,
    'MEAN' REAL,
    'MEDIAN' REAL,
    'SE MEAN' REAL,
    'STDEV' REAL,
    'Q3' REAL,
    'MAX' REAL,
    'COUNT' INTEGER
);

/* STATEWIDE TOTAL */
INSERT INTO temp_chem_summary
SELECT
    'TOTAL',
    MIN(VALUE)   AS 'MIN',
    LOWER_QUARTILE(VALUE) AS 'Q1',
    MEDIAN(VALUE) AS 'MEDIAN',
    AVG(VALUE) AS 'MEAN',
    STDEV(VALUE) / SQRT(COUNT(*)) AS 'SE MEAN',
    STDEV(VALUE) AS 'STDEV',
    UPPER_QUARTILE(VALUE) AS 'Q3',
    MAX(VALUE + 0) AS 'MAX',
    COUNT(*) as 'COUNT'
FROM CHEMDATA
WHERE CHEMPARAMETER = 'Hardness'
    AND DUPLICATE = 0
    AND station_type='River/Stream'
;

/* MAJOR BASIN 1 */
INSERT INTO temp_chem_summary
SELECT
    MAJOR,
    MIN(VALUE) AS 'MIN',
    LOWER_QUARTILE(VALUE) AS 'Q1',
    MEDIAN(VALUE) AS 'MEDIAN',
    AVG(VALUE) AS 'MEAN',
    STDEV(VALUE) / SQRT(COUNT(*)) AS 'SE MEAN',
    STDEV(VALUE) AS 'STDEV',
    UPPER_QUARTILE(VALUE) AS 'Q3',
    MAX(VALUE + 0) AS 'MAX',
    COUNT(*) as 'COUNT'
FROM CHEMDATA
INNER JOIN SITES ON SITES.STA_SEQ = CHEMDATA.STA_SEQ
INNER JOIN BASIN ON BASIN.SBASN = SITES.SBASN
WHERE MBASN = 1
    AND CHEMPARAMETER = 'Hardness'
    AND DUPLICATE = 0
    AND station_type='River/Stream'
;
/* MAJOR BASIN 2 */
INSERT INTO temp_chem_summary
SELECT
    MAJOR,
    MIN(VALUE) AS 'MIN',
    LOWER_QUARTILE(VALUE) AS 'Q1',
    MEDIAN(VALUE) AS 'MEDIAN',
    AVG(VALUE) AS 'MEAN',
    STDEV(VALUE) / SQRT(COUNT(*)) AS 'SE MEAN',
    STDEV(VALUE) AS 'STDEV',
    UPPER_QUARTILE(VALUE) AS 'Q3',
    MAX(VALUE + 0) AS 'MAX',
    COUNT(*) as 'COUNT'
FROM CHEMDATA
INNER JOIN SITES ON SITES.STA_SEQ = CHEMDATA.STA_SEQ
INNER JOIN BASIN ON BASIN.SBASN = SITES.SBASN
WHERE MBASN = 2
    AND CHEMPARAMETER = 'Hardness'
    AND DUPLICATE = 0
    AND station_type='River/Stream'
;
/* MAJOR BASIN 3 */
INSERT INTO temp_chem_summary
SELECT
    MAJOR,
    MIN(VALUE) AS 'MIN',
    LOWER_QUARTILE(VALUE) AS 'Q1',
    MEDIAN(VALUE) AS 'MEDIAN',
    AVG(VALUE) AS 'MEAN',
    STDEV(VALUE) / SQRT(COUNT(*)) AS 'SE MEAN',
    STDEV(VALUE) AS 'STDEV',
    UPPER_QUARTILE(VALUE) AS 'Q3',
    MAX(VALUE + 0) AS 'MAX',
    COUNT(*) as 'COUNT'
FROM CHEMDATA
INNER JOIN SITES ON SITES.STA_SEQ = CHEMDATA.STA_SEQ
INNER JOIN BASIN ON BASIN.SBASN = SITES.SBASN
WHERE MBASN = 3
    AND CHEMPARAMETER = 'Hardness'
    AND DUPLICATE = 0
    AND station_type='River/Stream'
;
/* MAJOR BASIN 4 */
INSERT INTO temp_chem_summary
SELECT
    MAJOR,
    MIN(VALUE) AS 'MIN',
    LOWER_QUARTILE(VALUE) AS 'Q1',
    MEDIAN(VALUE) AS 'MEDIAN',
    AVG(VALUE) AS 'MEAN',
    STDEV(VALUE) / SQRT(COUNT(*)) AS 'SE MEAN',
    STDEV(VALUE) AS 'STDEV',
    UPPER_QUARTILE(VALUE) AS 'Q3',
    MAX(VALUE + 0) AS 'MAX',
    COUNT(*) as 'COUNT'
FROM CHEMDATA
INNER JOIN SITES ON SITES.STA_SEQ = CHEMDATA.STA_SEQ
INNER JOIN BASIN ON BASIN.SBASN = SITES.SBASN
WHERE MBASN = 4
    AND CHEMPARAMETER = 'Hardness'
    AND DUPLICATE = 0
    AND station_type='River/Stream'
;
/* MAJOR BASIN 5 */
INSERT INTO temp_chem_summary
SELECT
    MAJOR,
    MIN(VALUE) AS 'MIN',
    LOWER_QUARTILE(VALUE) AS 'Q1',
    MEDIAN(VALUE) AS 'MEDIAN',
    AVG(VALUE) AS 'MEAN',
    STDEV(VALUE) / SQRT(COUNT(*)) AS 'SE MEAN',
    STDEV(VALUE) AS 'STDEV',
    UPPER_QUARTILE(VALUE) AS 'Q3',
    MAX(VALUE + 0) AS 'MAX',
    COUNT(*) as 'COUNT'
FROM CHEMDATA
INNER JOIN SITES ON SITES.STA_SEQ = CHEMDATA.STA_SEQ
INNER JOIN BASIN ON BASIN.SBASN = SITES.SBASN
WHERE MBASN = 5
    AND CHEMPARAMETER = 'Hardness'
    AND DUPLICATE = 0
    AND station_type='River/Stream'
;
/* MAJOR BASIN 6 */
INSERT INTO temp_chem_summary
SELECT
    MAJOR,
    MIN(VALUE) AS 'MIN',
    LOWER_QUARTILE(VALUE) AS 'Q1',
    MEDIAN(VALUE) AS 'MEDIAN',
    AVG(VALUE) AS 'MEAN',
    STDEV(VALUE) / SQRT(COUNT(*)) AS 'SE MEAN',
    STDEV(VALUE) AS 'STDEV',
    UPPER_QUARTILE(VALUE) AS 'Q3',
    MAX(VALUE + 0) AS 'MAX',
    COUNT(*) as 'COUNT'
FROM CHEMDATA
INNER JOIN SITES ON SITES.STA_SEQ = CHEMDATA.STA_SEQ
INNER JOIN BASIN ON BASIN.SBASN = SITES.SBASN
WHERE MBASN = 6
    AND CHEMPARAMETER = 'Hardness'
    AND DUPLICATE = 0
    AND station_type='River/Stream'
;
/* MAJOR BASIN 7 */
INSERT INTO temp_chem_summary
SELECT
    MAJOR,
    MIN(VALUE) AS 'MIN',
    LOWER_QUARTILE(VALUE) AS 'Q1',
    MEDIAN(VALUE) AS 'MEDIAN',
    AVG(VALUE) AS 'MEAN',
    STDEV(VALUE) / SQRT(COUNT(*)) AS 'SE MEAN',
    STDEV(VALUE) AS 'STDEV',
    UPPER_QUARTILE(VALUE) AS 'Q3',
    MAX(VALUE + 0) AS 'MAX',
    COUNT(*) as 'COUNT'
FROM CHEMDATA
INNER JOIN SITES ON SITES.STA_SEQ = CHEMDATA.STA_SEQ
INNER JOIN BASIN ON BASIN.SBASN = SITES.SBASN
WHERE MBASN = 7
    AND CHEMPARAMETER = 'Hardness'
    AND DUPLICATE = 0
    AND station_type='River/Stream'
;
/* MAJOR BASIN 8 */
INSERT INTO temp_chem_summary
SELECT
    MAJOR,
    MIN(VALUE) AS 'MIN',
    LOWER_QUARTILE(VALUE) AS 'Q1',
    MEDIAN(VALUE) AS 'MEDIAN',
    AVG(VALUE) AS 'MEAN',
    STDEV(VALUE) / SQRT(COUNT(*)) AS 'SE MEAN',
    STDEV(VALUE) AS 'STDEV',
    UPPER_QUARTILE(VALUE) AS 'Q3',
    MAX(VALUE + 0) AS 'MAX',
    COUNT(*) as 'COUNT'
FROM CHEMDATA
INNER JOIN SITES ON SITES.STA_SEQ = CHEMDATA.STA_SEQ
INNER JOIN BASIN ON BASIN.SBASN = SITES.SBASN
WHERE MBASN = 8
    AND CHEMPARAMETER = 'Hardness'
    AND DUPLICATE = 0
    AND station_type='River/Stream'
;

/* DISPLAY FINAL TABLE */
SELECT *
FROM temp_chem_summary
order by M_BASIN asc;