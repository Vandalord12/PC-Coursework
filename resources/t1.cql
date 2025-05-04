SELECT *
FROM "resources/A.csv" AS A
INNER JOIN "resources/B.csv" AS B ON A[0] = B[0]
WHERE A[0] = 1
ORDER BY A[0]
-- waiting for crossJoin logic to work to add in here 