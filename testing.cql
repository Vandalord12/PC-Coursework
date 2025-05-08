SELECT
  P[0],
  COALESCE(P[1], Q[1]) AS mergedname,
  COALESCE(P[2], Q[2]) AS mergedcity
FROM "P.csv" AS P
JOIN "Q.csv" AS Q ON P[0] = Q[0]

