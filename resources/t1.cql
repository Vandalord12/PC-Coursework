SELECT *
FROM "resources/A.csv" AS A
LEFT JOIN "resources/B.csv" AS B ON A[0] = B[0]

