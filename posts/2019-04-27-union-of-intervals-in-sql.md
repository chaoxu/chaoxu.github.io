---
title: Union of intervals in SQL
tags: SQL, algorithm
---

# Introduction 

We given a collection of intervals, and we want to find its union, represented by a set of disjoint intervals. Assume the intervals are of the form $[a,b)$, where $a<b$. 
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

Note this is a common interview problem, [LeetCode 56. Merge Intervals](https://leetcode.com/problems/merge-intervals/). 
I was surprised find a [very short solution on stackoverflow](https://stackoverflow.com/a/8120432/303863). 

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

[Itzik Ben-Gan](http://tsql.solidq.com/) has [written](https://www.itprotoday.com/development-techniques-and-management/packing-date-intervals) [multiple](https://blogs.solidq.com/en/sqlserver/packing-intervals/) [articles](https://www.itprotoday.com/sql-server/new-solution-packing-intervals-problem) on how to solve this problem. I recommend reading them to learn various tricks. In fact, my solution here is quite similar to one of Ben-Gan's. 

# Using basic SQL
Here we will try to implement an algorithm using the most basic of SQL, so it would even work in Hive. We first build a table, such that $(a,i,j)$ is in the table shows that there are $j$ intervals covering $a$, and there are $i$ intervals covering the previous point.

```sql
CREATE VIEW r AS 
SELECT a,
       Sum(d) OVER (ORDER BY a ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS i,
       Sum(d) OVER (ORDER BY a ROWS UNBOUNDED PRECEDING) AS j
FROM  (SELECT a, Sum(d) AS d
       FROM   (SELECT a,  1 AS d FROM t
               UNION ALL
               SELECT b, -1 AS d FROM t) e
       GROUP  BY a) f; 
```

Next, we produce all the endpoints in the union of the intervals and pair up adjacent ones. Finally, we produce the set of intervals by only pick the odd-numbered rows. 

```sql
SELECT a, b
FROM (SELECT a,
             Lead(a)      OVER (ORDER BY a) AS b,
             Row_number() OVER (ORDER BY a) AS n
      FROM   r
      WHERE  j=0 OR i=0 OR i is null) e
WHERE  n%2 = 1;
```

You can find [the example in DB-fiddle](https://www.db-fiddle.com/f/aVaF6NDTVYmxBpifsHDFBf/6). I am interested to seeing simpler and faster code using the simplest of SQL. 