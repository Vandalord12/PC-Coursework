--SELECT A[2], A[0] 
--FROM "resources/A.csv" AS A 
--WHERE A[0] = A[1]
--ORDER BY A[2] 

DELETE FROM "resources/A.csv" AS A WHERE A[0] = "Brenda"
