--SELECT *
--FROM "resources/A.csv" AS A
--INNER JOIN "resources/B.csv" AS B ON A[0] = B[0]

SELECT A[1], B[1]
FROM "resources/A.csv" AS A
JOIN "resources/B.csv" AS B ON A[0] = B[0]





