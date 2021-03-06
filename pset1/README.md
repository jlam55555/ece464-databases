# Project 1: SQL and ORMs
(and a lot of Haskell)

---

### Project setup

##### Directory structure
The [assignment file][assignment] and SQL scripts used for setting up the schemas can be found in the [res][res] directory.

This folder is arranged as a standard [Stack][stack] (Haskell) application, as Haskell is used for parts 2 and 3.

##### SQL setup
PostgreSQL is used in lieu of MySQL due to poor support for it in the ORM library (Beam only officially supports SQLite and PostgreSQL). The queries are mostly the same between the two.

Two databases must be created: `ece464_pset1` (for parts 1 and 2) and `ece464_pset1_part3`. A postgres user `ece464` has to be created with no-password access (`trust` role) to these databases. This may require configuration of the [pg_hba.conf][pg_hba.conf] file, like the following:

```conf
# `local` is used if directly connecting via the `psql` CLI
# `host` is used by Haskell's connection using the libpq library
local ece464_pset1       ece464              trust
local ece464_pset1_part3 ece464              trust
host  ece464_pset1       ece464 127.0.0.1/32 trust
host  ece464_pset1_part3 ece464 127.0.0.1/32 trust
```

Once the user and databases have been set up, the schemas can be created with the following commands (also located at the top of their respective SQL files):
```bash
$ cd PATH_TO_REPO/pset1/res
$ psql -a -U ece464 -d ece464_pset1 <pset1_setup.sql
$ psql -a -U ece464 -d ece464_pset1_part3 <pset1_part3_setup.sql
```

##### Haskell setup
Haskell is used with the [Stackage][stackage] package manager. The [Beam][beam] library (documentation: [beam-core][beam-core], [beam-postgres][beam-postgres]) is used as an ORM. The description of Beam (from the [documentation][beam-core]):

> Beam is a Haskell library for type-safe querying and manipulation of SQL databases. Beam is modular and supports various backends. In order to use beam, you will need to use beam-core along with a specific backend (such as beam-postgres or beam-sqlite) as well as the corresponding backend. For more information, see the user manual and tutorial on GitHub pages.

This is my first time using Haskell outside of a tutorial context, so it's probably not very good Haskell. If the code indentation looks strange, I am using [brittany][brittany] to format all of my code.

To run the Haskell tests, Stack needs to be installed.
```bash
$ cd PATH_TO_REPO/pset1
$ stack test
```
(This will take a long time on first run, as Stack will install `ghci`, as well as compile and install the required project libraries. After that, it will be relatively quick.)

As is standard for a Stack project, the source ("library") modules are under the `src/` directory, and the tests are located under the `tests/` directory. The filenames should be easy to follow.

The Haskell code can also be called interactively using `stack ghci`, which should automatically load the project files. Then any of the sample queries provided in [`PsetOne.PartTwo.Queries`][p2queries] or [`PsetOne.PartThree.Queries`][p3queries] can be run by calling their name, e.g.:
```bash
$ cd PATH_TO_REPO/pset1
$ stack ghci
> querySailorsSpending
[(SailorId 1,"Hershel",13531.12),(SailorId 2,"Joeann",1.5),(SailorId ...
```

---

### Test environment information
(for reproducibility reasons)

```bash
$ cat /etc/issue     # linux version
Debian GNU/Linux 11 \n \l
$ psql --version     # postgres version
psql (PostgreSQL) 13.3 (Debian 13.3-1)
$ stack --version    # stack/haskell version
Version 2.7.3, Git revision 7927a3aec32e2b2e5e4fb5be76d0d50eddcc197f x86_64 hpack-0.34.4
```

--- 

### Part 1

The SQL queries are located in [`res/pset1_part1.sql`][p1queries]. The queries can be run using:
```bash
$ psql -a -U ece464 -d ece464_pset1 <pset1_part1.sql
```
(The `-a` flag also prints out the queries.) The output of running this command is found in [`res/pset1_part1.out`][p1out].

Notes:
- I did not try to optimize for smaller intermediate tables like we did in class. I assume that PostgreSQL (being a widely-used, commercial DBMS) optimizes away unused columns in intermediate tables.
- I used CTE's when possible to avoid repeated queries.

---

### Part 2

The queries are implemented in Haskell using the Beam library, in the package [`PsetOne.PartTwo`][p2package]. The test case outputs match the outputs when running the raw SQL, so there is not much to say about correctness.

There is, however, a lot to comment on usability. ORM's are designed to allow easier interfacing between the structure of SQL data and the syntax of a programming language. In particular, object-oriented programming is through an API on struct-like objects and imperative, whereas SQL is expression-driven and declarative. Functional languages like Haskell are inherently declarative and the idea of "objects" is looser -- the idea of fields is abstracted away and we only have functions rather than the dot syntax and single-dispatch functions that are so dear to OOP.

As a result, SQL feels very similar to Haskell code, and this is reflected in the way that the ORM code looks. In fact, often the Haskell code looks longer than the SQL queries, which is due to a combination of the brittany code formatter doing its best to make things readable, and also because Haskell is not a DSL that can be terse by omitting many unnecessary constructs like SQL can. SQL's nested queries feel very natural to programmers who are used to highly-nested closures, and the nesting syntax is similar.

The Beam ORM is more of a SQL library than anything -- it only uses the term ORM in the most high-level description on GitHub, but advertises itself as a type-safe query library otherwise. It doesn't have features like SQLAlchemy's lazy evaluation and keeping track of a "dirty" session that can be written back to the database. Beam is probably fairly lightweight in comparison to SQLAlchemy for a number of Haskell-ish reasons:
- Haskell is already lazy-by-default, so it is relatively trivial to write lazy operations in. (All selectors (record field access methods) are inherently lazy.) There is no need to include this functionality in the library.
- Haskell is statically typed, which allows for aggressive type-checking and optimizations (e.g., type erasure). Adding too many layers of complexity might overcomplicate the type system.
- There is a relatively small community actively working on Beam and Haskell. Beam is already one of the larger RDBMS systems in Haskell, and yet it doesn't have good support for backends other than SQLite and PostgreSQL because it is not that large. (Speaking relative to Python and many OOP languages.)

If anything, the best ORM feature about the Beam library is its type-checking. This did cause a lot of frustration for me because of how strict it is, but the compiled code never produced a SQL error, and the Haskell type bugs that are produced are much more informative than the SQL bugs (most of which follow the unhelpful format "You have a SQL error near x"). That being said, as someone who is relatively new to Haskell's H-M type system (that stems from the ML family of programming languages), understanding and overcoming type errors (and knowing when to give up) took by far the most time of my work on this project.

One example of an issue that I was unable to solve was due to the Beam library being over-strict about scoping in a way that prevents a certain type of correlated subqueries. In particular, Beam and a more academic RDBMS library called Selda introduced the idea of a "state threading parameter" which prevents incompatible queries from being used together in certain operations, introduced in [(Eckbald 2019)][eckbald]. This is most apparent with nested operations: the outermost query is inferred to have the context `QBaseScope`, and a nested subquery created by a nesting command such as `EXISTS` has scope `QNested (QBaseScope)`, and this nesting can continue. The problem is that some subquery operations do not allow ther operands to come from different scopes because the Haskell types are incompatible. In particular, I had a problem with query 2, find sailors who have reserved all red boats, which I had written the following query for:
```sql
SELECT s.sid, s.sname
FROM sailors s
WHERE NOT EXISTS (
      SELECT b.bid
      FROM boats b
      WHERE color='red'
      EXCEPT (
             SELECT r.bid
             FROM reserves r
             WHERE r.sid=s.sid
      )
);
```
This gives a type error in the Haskell implementation (the SQL works fine by itself), while the following does not:

```sql
SELECT s.sid, s.sname
FROM sailors s
WHERE NOT EXISTS (
    SELECT b.bid
    FROM boats b
    WHERE color='red'
    AND NOT EXISTS (
        SELECT r.bid
        FROM reserves r
        WHERE r.bid=b.bid AND r.sid=s.sid
    )
);
```
This example was the only one of the queries I tried that was rejected by the type system. Other types of nesting (e.g., correlated subqueries that return a single value, CTEs) have special constructs that allow them to work. It only seems that nested subqueries with set operations (e.g., `EXCEPT`, `UNION`) are a problem. We can see why when looking at the type declarations for the `except_` and `exists_` Beam library functions (simplified for legibility):
```haskell
> :t except_
except_ :: Q be db (QNested s) a
     -> Q be db (QNested s) a
     -> Q be db s (WithRewrittenThread (QNested s) s a)
> :t exists_
exists_ :: Q be db s a -> QExpr be s Bool
```
What is important to take note of is that `except_` is written as a binary expression requiring two queries with a nested context, which ensures that the outer result cannot be used inside either of the subqueries, especially in the case of lateral joins or correlated subqueries returning a single value (which are supported in Beam using the alternate syntaxes `lateral_` and `subquery_`). However, the inner context is inferred to be `QBaseScope` because it involves the outer scope's `s` table so this breaks the correlated query returning an array of values. On the other hand, `exists_` is written as a unary operation, and there doesn't have to be an agreement among its "operands." Luckily, it is not difficult to rewrite `EXCEPT` clauses in terms of `EXISTS`. I believe this is a Beam bug but there might be some mathematical reason it is broken. Other ORM's like SQLAlchemy probably don't have this restriction, but they also either produce incorrect code or require run-time checking, both of which are tradeoffs.

This qualm about Beam is perhaps a very Haskell-like issue. [Here][rdbms-comparison] is a good comparison between Beam and other Haskell RDBMS (ORM-like) libraries -- among them, Beam may be the most enterprise ready. The author mentions that it's "finnicky" and requires a lot of deeply parameterized type boilerplate and type trickery, but that also makes it extremely well-typed when it works. The others on the list tend to be less type-finnicky but also sometimes missing features, not backend-agnostic (which Beam is), or pretty much a transliteration of SQL into Haskell (defeating the purpose of an ORM). If nothing else, the complicated types in Beam were a great learning experience into practical Haskell.

---

### Part 3

The following changes were made to the original tables in the schema (these changes are also detailed in the [SQL file][p3queries]):

- Changed surrogate key types from `INT` to `SERIAL` (PostgreSQL-specific auto-increment field).
- Added `FOREIGN KEY` constraints to all foreign keys (with the default `ON UPDATE`/`ON DELETE` actions).
- Changed `age INT` field in `sailors` table to `dob DATE` (for reasons discussed in class).
- Changed `color CHAR(20)` field in boats to `color VARCHAR(20)` (why was it fixed-length in the first place?).
- Updated the `reserves` table to include more items: the attending employee, the payment associated with the reservation. Also changed the primary key to a surrogate key `rid SERIAL`, because the reservation is referenced from other tables and the multi-field key would be inconvenient.

The following tables were added to the database schema:
- Employees: Stores employee name, date of birth, and hourly wage.
- Clock times: Stores clock in/out timestamps of employees. Primary key is employee and timestamp pair.
- Equipment: Stores sellable sailing/fishing merchandise, cost, and inventory (count).
- Equipment sales: Records transactions between sailors and the equipment, keeping track of how many items are bought and the corresponding payment.
- Incidents: Stores information about boating incidents, such as the associated reservation, timestamp, severity level, description, whether the incident has been resolved, attending employee (if any), resolution description, and payment ID if there are costs incurred.
- Payments: Keeps track of payments by sailors (customer, cost, and timestamp). There are three types of payments: reservations, incidents, and equipment sales.

Unlike parts one and two, in which the test fixture is given to us as part of the setup SQL file, I decided to write helper functions to insert data which get called by `createFixture` which sets up some test data. Programmatically creating the test fixture allows us to more semantically and correctly enforce relationships. For example, in the original schema, it may be easy to accidentally input the wrong sailor ID into the reservations table without detecting an error (and this is compounded by the fact that foreign key constraints were not placed in the given schema). However, by programmatically inserting test fixture entries, we get a number of benefits: we can programmatically check constraints that are not enforced by the DBMS (e.g., we can check that a boat is not checked out more than once on a day, or that clock in/out times are possible), relational keys cannot be mistyped, and dependent entities (such as the payment associated with an equipment sale) may be created automatically.

A few test cases are provided in the same manner as for part two to demonstate the general behavior. These test cases are by no means exhaustive. This is a much larger schema, and the number of useful queries (probably) grows at least quadratically with the number of tables. These queries are more to show that the relations used can model complex relationships between more than two entities (sailors, employees, boats, reservations, incidents, payments, equipment, equipment sales, and work shifts) in varied types of relationships. The test cases programatically create the test fixture on startup by calling `createFixture`. (Note: this also considerably slows down the testing and is not strictly necessary; this just ensures a consistent environment, which would be useful if any of the test cases updated the database.)

A sample run of the test cases from calling `stack test` (including those from part 2) gives the following output:
```text
pset1-0.1.0.0: unregistering (local file changes: test/PsetOne/PartThree/QueriesSpec.hs)
pset1> build (lib + test)
Preprocessing library for pset1-0.1.0.0..
Building library for pset1-0.1.0.0..
Preprocessing test suite 'pset1-test' for pset1-0.1.0.0..
Building test suite 'pset1-test' for pset1-0.1.0.0..
[3 of 4] Compiling PsetOne.PartThree.QueriesSpec
[4 of 4] Compiling Main [PsetOne.PartThree.QueriesSpec changed]
Linking .stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/pset1-test/pset1-test ...
pset1> copy/register
Installing library in /home/jon/Documents/ece464_assignments/pset1/.stack-work/install/x86_64-linux-tinfo6/7835068286f9d809102dce0bb72ae339ae3e52a68062e0367651b963cb7ef75a/8.10.7/lib/x86_64-linux-ghc-8.10.7/pset1-0.1.0.0-5sYq2SRQCXXCVFgYiCTO9E
Registering library for pset1-0.1.0.0..
pset1> test (suite: pset1-test)


PsetOne.PartThree.Queries
  part 3 test cases
    get sailors who have reserved all red boats
    get sailors who have reserved only red boats
    get sailors along with how much they have spent
    get all pairs of sailors and employees who have met through some transaction (i.e., through reservations or incidents)
    get the sailor who has bought the most boat hook ends
    count the total number of hours employees have worked
PsetOne.PartTwo.Queries
  part 2 test cases
    query 1: list, for every boat, the number of times it has been reserved, excluding those boats that have never been reserved
    query 2: list those sailors who have reserved every red boat
    query 3: list those sailors who have reserved only red boats
    query 4: for which boat are there the most reservations
    query 5: select all sailors who have never reserved a red boat
    query 6: average age of sailors with a rating of 10
    query 7: for each rating, find the name and id of the youngest sailor
    query 8: select, for each boat, the sailor who made the highest number of reservations for that boat

Finished in 2.6029 seconds
14 examples, 0 failures

pset1> Test suite pset1-test passed
Completed 2 action(s).
```

[res]: ./res
[assignment]: ./res/pset1_assignment.md
[stack]: https://docs.haskellstack.org/en/stable/README/
[stackage]: https://www.stackage.org/
[beam]: https://haskell-beam.github.io/beam/
[beam-core]: https://hackage.haskell.org/package/beam-core-0.9.1.0
[beam-postgres]: https://hackage.haskell.org/package/beam-core-0.9.1.0
[pg_hba.conf]: https://www.postgresql.org/docs/9.1/auth-pg-hba-conf.html
[p1queries]: ./res/pset1_part1.sql
[p1out]: ./res/pset1_part1.out
[p2queries]: ./src/PsetOne/PartTwo/Queries.hs
[p3queries]: ./src/PsetOne/PartThree/Queries.hs
[brittany]: https://hackage.haskell.org/package/brittany
[p2package]: ./src/PsetOne/PartTwo
[eckbald]: https://icfp19.sigplan.org/details/haskellsymp-2019-papers/10/Scoping-Monadic-Relational-Database-Queries
[rdbms-comparison]: https://www.williamyaoh.com/posts/2019-12-14-typesafe-db-libraries.html
