-- Assuming a postgresql install with user postgres:
-- $ createuser -U postgres ece464              # create new user
-- $ createdb -U ece464 ece464_pset1_part3      # create new database
-- $ psql -a -U ece464 -d ece464_pset1_part3 <pset1_part3_setup.sql

-- differences from the original test fixtures are commented here

-- age Int -> dob DATE
CREATE TABLE sailors (
       sid SERIAL PRIMARY KEY, 
       sname VARCHAR(30) NOT NULL,
       rating INT NOT NULL,     -- 1-10
       dob DATE NOT NULL
);

-- new table: keep track of employees
CREATE TABLE employees (
       eid SERIAL PRIMARY KEY,
       ename VARCHAR(30) NOT NULL,
       dob DATE NOT NULL,
       wage INT NOT NULL        -- hourly wage (in cents)
);

CREATE TABLE boats (
       bid SERIAL PRIMARY KEY,
       bname VARCHAR(20) NOT NULL,
       color VARCHAR(10) NOT NULL,
       length INT NOT NULL
);

-- new table: keep track of employee hours
CREATE TABLE clock_times (
       eid INT,
       time TIMESTAMP,
       type BOOLEAN NOT NULL,   -- true = clock in, false = clock out
       FOREIGN KEY (eid) REFERENCES employees,
       PRIMARY KEY (eid, time)
);

-- new table
CREATE TABLE equipment (
       eqid SERIAL PRIMARY KEY,
       name VARCHAR(30) NOT NULL,       -- short title
       dsc TEXT,                -- long description
       count INT NOT NULL,      -- quantity
       cost INT NOT NULL        -- cost in cents
);

-- new table: keep track of customer payments
-- assume that payment method information (e.g., credit card info) is kept
-- elsewhere securely (e.g., managed by third-party payment library); we are
-- only keeping track of payment amounts
CREATE TABLE payments (
       pid SERIAL PRIMARY KEY,
       sid INT NOT NULL,
       cost INT NOT NULL,       -- in cents
       time TIMESTAMP NOT NULL, -- timestamp of transaction
       type INT NOT NULL        -- 0=reservation, 1=equipment sale, 2=incident
);

-- new table
CREATE TABLE equipment_sales (
       pid INT PRIMARY KEY,     -- payment
       eqid INT NOT NULL,       -- equipment bought
       sid INT NOT NULL,        -- sailor buying equipment
       count INT NOT NULL,      -- quantity sold

       FOREIGN KEY (pid) REFERENCES payments,
       FOREIGN KEY (eqid) REFERENCES equipment,
       FOREIGN KEY (sid) REFERENCES sailors
);

-- changed primary key to rid
-- added eid field
-- added foreign key constaints
CREATE TABLE reserves (
       rid SERIAL PRIMARY KEY,
       sid INT NOT NULL,        -- sailor
       bid INT NOT NULL,        -- boat
       eid INT NOT NULL,        -- attending employee
       pid INT NOT NULL,        -- payment information
       day DATE NOT NULL,       -- date of boat reservation
       FOREIGN KEY (sid) REFERENCES sailors,
       FOREIGN KEY (bid) REFERENCES boats,
       FOREIGN KEY (eid) REFERENCES employees,
       FOREIGN KEY (pid) REFERENCES payments,
       UNIQUE (sid, bid, day)
);

-- new table
CREATE TABLE incidents (
       iid SERIAL PRIMARY KEY,

       -- incident description
       rid INT NOT NULL,
       time TIMESTAMP NOT NULL, -- time of incident
       sev INT NOT NULL,        -- severity level: 1-10, 10 is most severe
       dsc TEXT NOT NULL,       -- description
       resolved BOOLEAN NOT NULL,  -- whether incident has been resolved

       -- incident resolution
       eid INT,                 -- attending employee
       resolution TEXT,         -- description of resolution
       pid INT,                 -- costs associated with incident (if any)

       FOREIGN KEY (pid) REFERENCES payments,
       FOREIGN KEY (rid) REFERENCES reserves,
       FOREIGN KEY (eid) REFERENCES employees
);
