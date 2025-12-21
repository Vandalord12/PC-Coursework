SELECT R1[0] AS val1, R3[1] AS val2
FROM "R.csv" AS R1
INNER JOIN "../csv-ql/R.csv" AS R2 ON R1[1] = R2[0]
INNER JOIN "./R.csv" AS R3 ON R2[1] = R3[0]
ORDER ALL