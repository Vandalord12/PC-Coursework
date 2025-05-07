SELECT e1[1], b1[0]
FROM "resources/A.csv" AS e1
CROSS JOIN "resources/B.csv" AS b1 ON e1[2] = b1[0]
WHERE (b1[0] >= 20 OR e1[1] = "Alice") AND e1[1] = "Eve"
ORDER ALL