# Database Maintenance Reference

## Table Storage Size

```sql
-- Total size (table + indexes + TOAST)
SELECT pg_size_pretty(pg_total_relation_size('your_table_name'));

-- Table data only (no indexes)
SELECT pg_size_pretty(pg_relation_size('your_table_name'));
```

To see all tables ranked by size:

```sql
SELECT
  relname AS table_name,
  pg_size_pretty(pg_total_relation_size(relid)) AS total_size,
  pg_size_pretty(pg_relation_size(relid)) AS table_size,
  pg_size_pretty(pg_total_relation_size(relid) - pg_relation_size(relid)) AS indexes_size
FROM pg_catalog.pg_statio_user_tables
ORDER BY pg_total_relation_size(relid) DESC;
```

## Bloat / Dead Tuples

Tables with the most dead rows — candidates for manual `VACUUM`:

```sql
SELECT
  relname AS table_name,
  n_live_tup AS live_rows,
  n_dead_tup AS dead_rows,
  round(n_dead_tup::numeric / nullif(n_live_tup + n_dead_tup, 0) * 100, 2) AS dead_pct,
  pg_size_pretty(pg_total_relation_size(relid)) AS total_size
FROM pg_stat_user_tables
ORDER BY n_dead_tup DESC;
```

To manually vacuum a table: `VACUUM (VERBOSE, ANALYZE) your_table_name;`

## Last Vacuum / Analyze Times

```sql
SELECT
  relname AS table_name,
  last_vacuum,
  last_autovacuum,
  last_analyze,
  last_autoanalyze,
  n_dead_tup AS dead_rows,
  n_live_tup AS live_rows
FROM pg_stat_user_tables
ORDER BY last_autovacuum ASC NULLS FIRST;
```

## Unused Indexes

Indexes that have never been scanned since the last stats reset — safe candidates for removal after verification:

```sql
SELECT
  schemaname,
  relname AS table_name,
  indexrelname AS index_name,
  pg_size_pretty(pg_relation_size(indexrelid)) AS index_size,
  idx_scan AS scans
FROM pg_stat_user_indexes
WHERE idx_scan = 0
  AND indexrelname NOT LIKE '%_pkey'
ORDER BY pg_relation_size(indexrelid) DESC;
```

> Note: Stats reset on server restart. An index with 0 scans may still be needed — cross-check before dropping.

## Long-Running Queries

Queries running longer than 30 seconds:

```sql
SELECT
  pid,
  now() - pg_stat_activity.query_start AS duration,
  state,
  query
FROM pg_stat_activity
WHERE query_start IS NOT NULL
  AND state != 'idle'
  AND now() - query_start > interval '30 seconds'
ORDER BY duration DESC;
```

To kill a query: `SELECT pg_cancel_backend(<pid>);` (graceful) or `SELECT pg_terminate_backend(<pid>);` (hard kill)

## Blocking Locks

Which query is blocking which:

```sql
SELECT
  blocked.pid AS blocked_pid,
  blocked.query AS blocked_query,
  blocking.pid AS blocking_pid,
  blocking.query AS blocking_query,
  now() - blocked.query_start AS blocked_for
FROM pg_stat_activity blocked
JOIN pg_stat_activity blocking
  ON blocking.pid = ANY(pg_blocking_pids(blocked.pid))
ORDER BY blocked_for DESC;
```

## Cache Hit Rate

Buffer cache efficiency per table. A hit rate below ~99% indicates heavy disk I/O:

```sql
SELECT
  relname AS table_name,
  round(
    heap_blks_hit::numeric / nullif(heap_blks_hit + heap_blks_read, 0) * 100, 2
  ) AS cache_hit_pct,
  heap_blks_hit AS cache_hits,
  heap_blks_read AS disk_reads
FROM pg_statio_user_tables
WHERE heap_blks_hit + heap_blks_read > 0
ORDER BY disk_reads DESC;
```

Overall database cache hit rate:

```sql
SELECT
  round(sum(blks_hit)::numeric / nullif(sum(blks_hit) + sum(blks_read), 0) * 100, 2) AS cache_hit_pct
FROM pg_stat_database;
```

## Sequence Overflow Check

Sequences approaching their max value (`int4` max is ~2.1B, `int8` max is ~9.2 quintillion):

```sql
SELECT
  sequence_schema,
  sequence_name,
  data_type,
  maximum_value::bigint AS max_value,
  last_value,
  round(last_value::numeric / maximum_value::numeric * 100, 2) AS pct_used
FROM information_schema.sequences
JOIN (
  SELECT sequencename, last_value
  FROM pg_sequences
) seq ON seq.sequencename = sequence_name
WHERE last_value IS NOT NULL
ORDER BY pct_used DESC;
```

> Alert at ~75% usage. `int4` serial columns are the common risk — consider migrating to `bigint` / `bigserial`.

## Index Bloat

Indexes significantly larger than expected relative to the table — candidates for `REINDEX`:

```sql
SELECT
  schemaname,
  relname AS table_name,
  indexrelname AS index_name,
  pg_size_pretty(pg_relation_size(indexrelid)) AS index_size,
  pg_size_pretty(pg_relation_size(relid)) AS table_size,
  round(
    pg_relation_size(indexrelid)::numeric / nullif(pg_relation_size(relid), 0), 2
  ) AS index_to_table_ratio
FROM pg_stat_user_indexes
ORDER BY pg_relation_size(indexrelid) DESC;
```

To rebuild an index without locking: `REINDEX INDEX CONCURRENTLY index_name;`
