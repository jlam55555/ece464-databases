# Problem set 2: Web scraping and MongoDB

---

### Project setup
The [assignment file][assignment] can be found in the [res][res] directory.

##### Prerequisites
Common Lisp (`sbcl`) and QuickLisp are required for this project. On Debian:
```bash
$ apt install sbcl cl-quicklisp
$ sbcl
> ;;; in the SBCL REPL
> (quicklisp-quickstart:install)
```

---

### Build instructions

##### Interactive (REPL) instructions
The path to the quicklisp installation may have to be customized for your system.
```bash
$ sbcl --load ~/quicklisp/setup.lisp --load driver.lisp
> ; interact with scraper library here
```

Of course, using a purpose-built Common Lisp environment (SLIME) is a better experience than using the REPL directly.

##### Compilation instructions
TODO: https://stackoverflow.com/q/14171849/2397327

---

### Web scraping

##### Overview

The web scraping process begins with a search query `QUERY`, e.g., "rtx 8000". It then performs the search on eBay (`https://ebay.com/sch/i.html?_nkw=QUERY`) and retrieves a list of item ID's by scanning for links of the form `https://ebay.com/itm/ITEMID`. Pages 1, 2, ... of the paginated search are scanned for item ID's until no new ID's are returned (this is because if you use a page number larger than the last page, eBay returns the last search page.)

With the list of item ID's, the corresponding item pages are scraped and the scraped info dumped into the collection `item_QUERY` in database `ece464_pset2`. The seller's page is also scraped, and the info dumped in the collection `sellers`.

The data fetched includes:

- **Item**:
  - eBay ID
  - Item name
  - Item properties (e.g., price, currency, availability)
  - About this item (e.g., condition, MPN, type, etc.)
  - Description (text, inner iframe text)
  - Condition message
  - Seller name
  - Item location
  - Ships to locations (and excluded locations)
  - Item policies (shipping, return, and payment policies)
  - List of thumbnail images
- **Seller**:
  - Seller name
  - Bio
  - Feedback scores
  - Feedback ratings
  - Creation date
  - Location

Because there may be thousands of items and sellers, the process is heavily accelerated with parallelization.

##### Challenges
- **Finding correct elements on webpage**: This is the expected part of web scraping. This involves the Developer Tools and the `lquery` library.
- **Parallelization**: Much CPU time is wasted idle if parallelization is not used. Parallelization is achieved using parallel maps with the `lparallel` library.
  - **Synchronization**: The `cl-mongo` library is not thread-safe, and some requests will be dropped if unsynchronized. Calls to `cl-mongo`'s CRUD operations are protected with a mutex.
  - **Memory usage**: The parallelizability is limited by the default SBCL RAM (heap) ceiling due to storing too many records at once (some of the scraped documents can be quite large). Initially, the number of threads for `lparallel` was set to 50, but this caused the heap to be exhausted. Lowering this setting to 20 threads did not decrease performance by much, and never exhausted the heap in the test cases.
- **Data formats**: This was a matter of understanding the document format used in the `cl-mongo` library, and choosing what is most convenient to work with. The main confusion is with lists and vectors; `cl-mongo` only recognizes CL lists (and not vectors) as BSON vectors. There is also a problem with inserting CL date objects directly. Custom conversion methods from the scraped data and `cl-mongo`'s documents were implemented.
- **Inconsistent/missing elements**: Some items may include items that other items do not. As a result, the inputs may be of different length or even different types between items, which may cause the program to crash if not handled correctly (especially in a language like Common Lisp, which has strong run-time typing). Care needs to be taken to validate input to prevent crashing.
- **Element volatility**: The element selectors are proprietary and not meant for commercial consumption, and may change at any time without notice. This may cause inconsistent/missing elements (see above), and also introduce additional material for scraping. Thus, this scraper will probably not be valid for very long without regular updates.

##### Future work
- **Webpage automation**: Some data was not loaded when the webpage was served, and thus could not be scraped with an ordinary HTTP-request-scraper like this one. This data is usually more-dynamic data that is likely being fetched often from some eBay API. It will be possible to fetch this data using a web automation tool such as Selenium, but this will also cause a marked performance drop.

---

### Sample queries

TODO

[assignment]: ./res/pset2_assignment.md
[res]: ./res/
