SELECT *
FROM "resources/E.csv" AS E
FULL JOIN "resources/D.csv" AS D ON E[0] = D[0]
