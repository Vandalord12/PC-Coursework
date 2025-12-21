SELECT S[2] AS val1, T[0] AS val2
FROM "S.csv" AS S
CROSS JOIN "T.csv" AS T ALL
WHERE (S[0] = S[1]) AND (T[1] = T[2])
ORDER ALL



