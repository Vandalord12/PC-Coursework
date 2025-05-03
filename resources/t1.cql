SELECT *
FROM "resources/A.csv" AS A
INNER JOIN "resources/B.csv" AS B ON A[0] = B[0]
INNER JOIN "resources/P.csv" AS P ON P[0] = A[0]