---
title: Union of intervals in SQL
tags: SQL, algorithm
---

We given a collection of intervals, and we want to find its union, represented by a set of disjoint intervals. Note this is a common interview problem. It is [LeetCode 56. Merge Intervals](https://leetcode.com/problems/merge-intervals/). Assume the intervals are of the form $[a,b)$, where $a<b$. 
However, I have to solve this problem in Hive. So this is a problem I have to solve in Hive's SQL variant. 

First, here is the schema of the table and some sample inputs. 

```sql
CREATE TABLE t (
  a int,
  b int
);
INSERT INTO t VALUES
  (0,10),
  (20,30),
  (5,15);
```

The correct output should be the following.

```
a    b
-------
0    15
20   30
```

We do not allow empty intervals, so we cannot have $a=b$. I was surprised find a [very short solution on stackoverflow](https://stackoverflow.com/a/8120432/303863).

```sql
SELECT 
       t1.a,
       MIN(t2.b) AS b
FROM t t1 
INNER JOIN t t2 ON t1.a <= t2.b
  AND NOT EXISTS(SELECT * FROM t 
                 WHERE t2.b >= t.a AND t2.b < t.b) 
WHERE NOT EXISTS(SELECT * FROM t
                 WHERE t1.a > t.a AND t1.a <= t.b) 
GROUP BY t1.a
ORDER BY t1.a
```

Unfortunately, once you know how the entire algorithm goes, one can see its performance does not look promising. Also, making it work in Hive is next to impossible due to Hive's limitations on joins and subqueries.

Here we will try to implement an algorithm using the most basic of SQL, so it would even work in Hive. We first build a table, such that $(p,i)$ is in the table shows that there are precisely $i$ points overlapping $p$.

```sql
CREATE VIEW r AS 
SELECT p,
       SUM(d) OVER (ORDER BY p ROWS UNBOUNDED PRECEDING) AS c
FROM  (SELECT p,
              SUM(d) AS d
       FROM   (SELECT a AS p,
                      1 AS d
               FROM   t
               UNION ALL
               SELECT b  AS p,
                      -1 AS d
               FROM   t) e
       GROUP  BY p) f; 
```

Next, we produce all the endpoints in the union of the intervals.

```sql
CREATE VIEW s AS
SELECT p
FROM   (SELECT *,
               Lag(c) OVER (ORDER BY p)    AS d 
        FROM   r) e
WHERE  c=0 OR d=0 OR d is null;
```

Finally, we produce the set of intervals by pairing up adjacent rows. 

```sql
SELECT a, b
FROM   (SELECT p                              AS a,
               Lead(p) OVER (ORDER BY p)      AS b,
               Row_number() OVER (ORDER BY p) AS n
        FROM   s) f
WHERE  n%2 = 1;
```

You can find [the example in DB-fiddle](https://www.db-fiddle.com/f/aVaF6NDTVYmxBpifsHDFBf/5). I am interested to seeing simpler and faster code using the simplest of SQL. 