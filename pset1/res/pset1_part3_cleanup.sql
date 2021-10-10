-- Delete tables; note that order matters here; we have to go in topological order
-- (basically reverse of creation order, since that is topological too).
-- Usage:
-- $ psql -U ece464 -d ece464_pset1_part3 <pset1_part3_cleanup.sql
DROP TABLE incidents;
DROP TABLE reserves;
DROP TABLE equipment_sales;
DROP TABLE payments;
DROP TABLE equipment;
DROP TABLE clock_times;
DROP TABLE boats;
DROP TABLE employees;
DROP TABLE sailors;
