SELECT A[0], A[1]
FROM "resources/A.csv" AS A 
WHERE A[1] != ""
ORDER BY A[1]