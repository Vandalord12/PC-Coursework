SELECT *
FROM "resources/A.csv" AS A
CROSS JOIN "resources/B.csv" AS B ON A[0] = B[0]
CROSS JOIN "resources/C.csv" AS C ON A[0] = C[0]
ORDER ALL
