SELECT 
  F[0] AS val1,
  G[2] AS val2
FROM "F.csv" AS F
INNER JOIN "G.csv" AS G ON F[1] = G[0]
WHERE 
  F[2] = G[1] AND
  F[1] != "" AND
  F[2] != ""
ORDER ALL