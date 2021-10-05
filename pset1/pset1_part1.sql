-- Usage: (-a flag to print out script lines as they are read)
-- $ psql -a -U ece464 -d ece464_pset1 <pset1_part1.sql

-- NOTE: If there is a tie (non-unique answer), all valid answers
-- will be returned.

-- List, for every boat, the number of times it has been reserved,
-- excluding those boats that have never been reserved (list the id
-- and the name).
SELECT bid, bname, reserve_count
FROM (
     SELECT bid, COUNT(*) reserve_count
     FROM reserves
     JOIN boats USING (bid)
     GROUP BY bid
) _
JOIN boats USING (bid)
ORDER BY bid ASC;

-- List those sailors who have reserved every red boat (list the id
-- and the name).
-- sailors for which there are no red boats that are not reserved
-- by them
SELECT sid, sname
FROM sailors s
WHERE NOT EXISTS (
      -- red boat not reserved by them
      SELECT bid
      FROM boats
      WHERE color='red'
      EXCEPT (
             SELECT bid
             FROM reserves r
             WHERE r.sid=s.sid
      )
);

-- alternatively:
SELECT sid, sname
FROM sailors s
WHERE NOT EXISTS (
      -- red boat not reserved by them
      SELECT bid
      FROM boats b
      WHERE color='red'
      AND NOT EXISTS (
            SELECT bid
            FROM reserves r
            WHERE r.sid=s.sid AND r.bid=b.bid
      )
);

-- List those sailors who have reserved only red boats.
-- sailors who have reserved a red boat
-- - sailors that have reserved other colors
SELECT sid, sname
FROM (
     -- sailors who have reserved red boats
     SELECT DISTINCT sid
     FROM reserves
     JOIN (
          SELECT bid
          FROM boats
          WHERE color='red'
     ) _ USING (bid)
     EXCEPT (
            -- sailors who have reserved other color boats
            SELECT DISTINCT sid
            FROM reserves
            JOIN (
                 SELECT bid
                 FROM boats
                 WHERE color<>'red'
            ) _ USING (bid)
     )
) _
JOIN sailors USING (sid);

-- For which boat are there the most reservations?
WITH reserves_by_boat AS (
     SELECT bid, COUNT(*) res_count
     FROM reserves
     GROUP BY bid
)
SELECT bid, bname, res_count
FROM reserves_by_boat
JOIN boats USING (bid)
WHERE res_count=(
      SELECT MAX(res_count)
      FROM reserves_by_boat
);

-- Select all sailors who have never reserved a red boat.
SELECT sid, sname
FROM sailors
EXCEPT (
       SELECT sid, sname
       FROM sailors
       JOIN reserves USING (sid)
       JOIN (
            SELECT bid
            FROM boats
            WHERE color='red'
       ) _ USING (bid)
)
ORDER BY sid;

-- Find the average age of sailors with a rating of 10.
SELECT AVG(age) ave_age
FROM sailors
WHERE rating=10;

-- For each rating, find the name and id of the youngest sailor.
-- NOTE: will not show ratings that are not assigned to anyone
SELECT sname, sid, rating, age
FROM sailors
JOIN (
      SELECT rating, MIN(age) age
      FROM sailors
      GROUP BY rating
) _ USING (rating, age)
ORDER BY rating DESC;

-- Select, for each boat, the sailor who made the highest number of
-- reservations for that boat.
-- NOTE: assumes that all boats have had at least one reservation
-- (true in this case)
WITH reservation_counts AS (
     -- reservation count for bid, sid pairs
     SELECT bid, sid, COUNT(*) res_count
     FROM boats
     JOIN reserves USING (bid)
     GROUP BY bid, sid
)
SELECT bid, sid, sname, res_count
FROM (
     -- select maximum reservations for each boat
     SELECT bid, MAX(res_count) res_count
     FROM reservation_counts
     GROUP BY bid
) _
JOIN reservation_counts USING (bid, res_count)
JOIN sailors USING (sid)
ORDER BY bid;
