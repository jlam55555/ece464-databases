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

This is my first time using Haskell outside of a tutorial context, so it's probably not very good Haskell.

To run the Haskell tests, Stack needs to be installed.
```bash
$ cd PATH_TO_REPO/pset1
$ stack test
```
The Haskell code can also be called interactively using `stack ghci`, which should automatically load the project files. Then any of the sample queries provided in `PsetOne.PartTwo.Queries` or `PsetOne.PartThree.Queries` can be run by calling their name, e.g.:
```bash
$ cd PATH_TO_REPO/pset1
$ stack ghci
> querySailorsSpending
[(SailorId 1,"Hershel",13531.12),(SailorId 2,"Joeann",1.5),(SailorId 3,"Vania",51.5),(SailorId 4,"Katheryn",0.5),(SailorId 5,"Shanika",0.5),(SailorId 6,"Madeleine",14.46),(SailorId 7,"Li",0.5),(SailorId 8,"Zachariah",0.5),(SailorId 9,"Marinda",7.07),(SailorId 10,"Clara",7.23)]
```

--- 

### Part 1

[res]: ./res
[assignment]: ./res/pset1_assignment.md
[stack]: https://docs.haskellstack.org/en/stable/README/
[stackage]: https://www.stackage.org/
[beam]: https://haskell-beam.github.io/beam/
[beam-core]: https://hackage.haskell.org/package/beam-core-0.9.1.0
[beam-postgres]: https://hackage.haskell.org/package/beam-core-0.9.1.0
[pg_hba.conf]: https://www.postgresql.org/docs/9.1/auth-pg-hba-conf.html
