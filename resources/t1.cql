SELECT A[0], A[1], A[2], A[3], A[4]
FROM "resources/A.csv" AS A 
CROSS JOIN "resources/B.csv" AS B ON A[0] = B[0]
ORDER BY A[0]


