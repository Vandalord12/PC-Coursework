SELECT A[2], A[0] 
FROM "resources/A.csv" AS A 
WHERE A[0] = A[1]
--could do this also ORDER BY A[0]