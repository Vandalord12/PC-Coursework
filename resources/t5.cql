SELECT 
  COALESCE(P[0], Q[0]),
  COALESCE(P[1], Q[1]),
  COALESCE(P[2], Q[2]),
  COALESCE(P[3], Q[3])
FROM "resources/P.csv" AS P
JOIN "resources/Q.csv" AS Q ON P[0] = Q[0]
ORDER BY COALESCE(P[3], Q[3]) 
