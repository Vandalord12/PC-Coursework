SELECT *
FROM "resources/A.csv" AS A
INNER JOIN "resources/B.csv" AS B ON A[0] = B[0]
INNER JOIN "resources/B.csv" AS C ON A[0] = B[0]

