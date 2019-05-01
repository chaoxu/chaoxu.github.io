---
title: Union of intervals in SQL
tags: SQL, algorithm
---

# Introduction 

We given a collection of $n$ intervals, and we want to find its union, represented by a set of disjoint intervals. Assume the intervals are of the form $[a,b)$, where $a<b$. 
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

We do not allow empty intervals, so we cannot have $a=b$. 

# Previous Works

Note this is a common interview problem, [LeetCode 56. Merge Intervals](https://leetcode.com/problems/merge-intervals/). There is a $\Omega(n\log n)$ running time lower bound. There is an $O(n\log p)$ upper bound, where $p$ is the number of points required to stab all intervals. 
In higher dimension, this is called the [Klee's measure problem](https://en.wikipedia.org/wiki/Klee%27s_measure_problem). 

However, one would wonder how efficient can we solve the problem in SQL. I was surprised find a [very short solution on stackoverflow](https://stackoverflow.com/a/8120432/303863). 

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

Unfortunately, once you know how the entire algorithm goes, one can see its performance does not look promising. Indeed, this is a $O(n^2)$ time algorithm. After generating $10000$ random intervals in PostgreSQL, it took 13 seconds to run. Also making it work in Hive is next to impossible due to Hive's limitations on joins and subqueries.

[Itzik Ben-Gan](http://tsql.solidq.com/) has [written](https://www.itprotoday.com/development-techniques-and-management/packing-date-intervals) [multiple](https://blogs.solidq.com/en/sqlserver/packing-intervals/) [articles](https://www.itprotoday.com/sql-server/new-solution-packing-intervals-problem) on how to solve this problem. I recommend reading them to learn various tricks. In fact, my solution here is quite similar to one of Ben-Gan's. 

Thanks to [Peng Yu](https://scholar.google.com/citations?user=jB4qJYEAAAAJ&hl=en) who pointed out this kind of queries is very common in sessionization.

# Using basic SQL

Here we will try to implement an algorithm using the most basic of SQL, so it would even work in Hive. 


## Solution by simulate the standard sweep-line algorithm

We first build a table, such that $(a,c)$ is in the table shows that there are $c$ intervals the endpoint directly before $a$.
Next, we notice that $c=0$ if and only if $a$ is the start of a new interval in the union. Hence we can assign everything between consecutive $c=0$ a name. 

```sql
WITH 
  weighted_endpoints AS (
    SELECT a, Sum(d) AS d
    FROM   (SELECT a,  1 AS d FROM t
            UNION ALL
            SELECT b, -1 AS d FROM t) e
    GROUP  BY a),
  endpoints_with_coverage AS (
    SELECT *, Sum(d) OVER (ORDER BY a) - d AS c
    FROM weighted_endpoints),
  equivalence_classes AS (
    SELECT a, COUNT(CASE WHEN c=0 THEN 1 END) OVER (ORDER BY a) AS class
    FROM endpoints_with_coverage)
SELECT min(a) AS a, max(a) AS b
FROM equivalence_classes
GROUP BY class;
```
The equivalence classes idea is from Peng Yu. This code took 100ms to handle 10000 random intervals in PostgreSQL.
You can find [the example in DB-fiddle](https://www.db-fiddle.com/f/aVaF6NDTVYmxBpifsHDFBf/9). I am interested to seeing simpler and faster code using the simplest of SQL.

## Solution through gaps
There is another solution, which uses the idea of gaps. Interestingly, gaps are much easier to compute. Here we modify [Oleg K's solution](https://stackoverflow.com/a/53163029/303863). 

```sql
WITH largest_prev AS (SELECT 
                      MAX(b) OVER (ORDER BY a) AS b,
                      LEAD(a) OVER (ORDER BY a) AS a 
                      FROM t),
     gaps AS (SELECT * FROM largest_prev WHERE b<a
              UNION ALL
              SELECT min(a), min(a) from t
              UNION ALL
              SELECT max(b), null from t),
     intervals AS (SELECT a, LEAD(b) OVER (ORDER BY b) as b FROM gaps)
SELECT * FROM intervals WHERE a IS NOT null;
```

This one can also be tested on [DB-fiddle](https://www.db-fiddle.com/f/k1GTRiRgaiPmRfxZoWBhTs/1).
This code took 70ms to handle 10000 random intervals in PostgreSQL.

As a real application, for example, if we want to know the length of union of intervals grouped by some keys. The following is how we do it in hive. Note in this application, we don't have to remove the null rows because we are taking a sum.

```sql
SET hivevar:key=id1,id2;
SET hivevar:input=t;
WITH largest_prev AS (SELECT
                      ${key},
                      MAX(b) OVER (PARTITION BY ${key} ORDER BY a) AS b,
                      LEAD(a) OVER (PARTITION BY ${key} ORDER BY a) AS a
                      FROM ${input}),
     gaps AS (SELECT * FROM largest_prev WHERE b<a
              UNION ALL
              SELECT ${key}, min(a), min(a) from ${input} GROUP BY ${key}
              UNION ALL
              SELECT ${key}, max(b), null from ${input} GROUP BY ${key}),
     intervals AS (SELECT ${key}, 
                          a,
                          LEAD(b) OVER (PARTITION BY ${key} ORDER BY b) as b
                   FROM gaps)
SELECT ${key}, SUM(b-a) as score
FROM intervals
GROUP BY ${key};
```
