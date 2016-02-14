/*
Transform the tb data to make it into 1 row per country year and with the 
total of TB cases across child, adult and elderly as well as by the sex 

Manually upload this file to github for further processing in R*/

SELECT country, year, SUM(child+adult+elderly) AS cases INTO OUTFILE 'c://data/tb_cases.csv'
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
ESCAPED BY '\\'
LINES TERMINATED BY '\n'
FROM tb
GROUP BY country, year
HAVING SUM(child+adult+elderly) IS NOT NULL
ORDER BY country, year;

-- Upload the tb_cases.csv to github
