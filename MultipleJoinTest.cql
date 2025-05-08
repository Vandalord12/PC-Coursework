SELECT *
FROM "resources/A.csv" AS A
JOIN "resources/B.csv" AS B ON A[0] = B[0]
JOIN "resources/C.csv" AS C ON B[1] = C[0]
JOIN "resources/D.csv" AS D ON C[1] = D[0]
ORDER ALL 