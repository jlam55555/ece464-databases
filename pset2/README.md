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
- **Library idiosyncrasies**: There were several problems with the libraries that made them not work out of the box. `cl-mongo` required a manual conversion for the date type and had problems with printing documents. Since `cl-mongo` uses CL lists for BSON list types and there is no distinction between CL's `nil` and an empty list `'()`, it is also difficult to express an empty list in `cl-mongo`. `dexador` had problems with synchronization that required initializing its internal hashtable with the `:synchronized` option. `lquery` considered the content of `<style>`, `<script>`, and `<!-- -->` (comment) tags to be text; these tags had to be manually removed before taking text content.

##### Future work
- **Webpage automation**: Some data was not loaded when the webpage was served, and thus could not be scraped with an ordinary HTTP-request-scraper like this one. This data is usually more-dynamic data that is likely being fetched often from some eBay API. It will be possible to fetch this data using a web automation tool such as Selenium, but this will also cause a marked performance drop.
- **Remove nested object `_id`s**: As shown in the example below, nested documents have an extraneous `_id` field. These is caused by the manual conversion process, and can be removed.

##### Sample fetched documents

Item
```javascript
{
  "_id" : ObjectId("030337ca15aa42c98db46ffd"),
  "id" : 333680539915,
  "name" : "Pny RTX6KNVLINKX16S2RKIT Nvidia Quadro Rtx Nvlink Hb 2-slot [rtx8000, Rtx6000]",
  "itemprops" : {
    "price" : 130.61,
    "availability" : "https://schema.org/InStock",
    "priceCurrency" : "USD",
    "_id" : ObjectId("cef98002a7c245a8a2fa5e16")
  },
  "about-this-item" : {
    "Condition:" : "New: A brand-new, unused, unopened, undamaged item in its original packaging (where packaging is ... Read moreabout the conditionNew: A brand-new, unused, unopened, undamaged item in its original packaging (where packaging is applicable). Packaging should be the same as what is found in a retail store, unless the item is handmade or was packaged by the manufacturer in non-retail packaging, such as an unprinted box or plastic bag. See the seller's listing for full details. See all condition definitionsopens in a new window or tab",
    "Type:" : "Wireless Routers",
    "UPC:" : "751492623856",
    "Brand:" : "NVIDIA, PNY",
    "Chipset Manufacturer:" : "NVIDIA",
    "MPN:" : "RTX6KNVLINKX16S2RKIT",
    "Chipset/GPU Model:" : "NVIDIA Quadro 6000",
    "_id" : ObjectId("c314ce1af2944cbeb60de197")
  },
  "desc-text" : "beachaudio beachaudio (293212 ) 99.1% Visit Store: beachaudio Categories Movies & TV Business & Industrial Computers/Tablets & Networking",
  "desc-iframe-text" : "eBay [if IE]> <style> .contentbox {text-align: left; width: 100%; padding: 10px; color: #0E4B69; font-size: 13px} </style> <![endif] Pny RTX6KNVLINKX16S2RKIT Nvidia Quadro Rtx Nvlink Hb 2-slot [rtx8000, Rtx6000] Pny RTX6KNVLINKX16S2RKIT Nvidia Quadro Rtx Nvlink Hb 2-slot [rtx8000, Rtx6000]International Customers: All electronic products are packaged for sale in the US with US voltage.This item is brand-new, factory sealed. ShippingMost orders placed prior to 11AM PST ship same business day Some lightweight items ship Postal and can be shipped to a PO BOX. Most other orders will ship via UPS or FedEx. Please note we cannot ship UPS or FedEx to PO Box addresses. Expedited orders will ship 2nd Day Air ReturnsThis item is eligible for return within 30 days of delivery. Opened items must be in re-sellable condition including original packaging and all accessories. Please visit our Returns Page for more information on Excluded Items, Damage Claims, and more details on requesting RA information.[ Read our full policy ]<p> About Beach AudioFounded in August 2002, Beach Audio´s mission is to provide the best experience on the Internet for buying Consumer Electronics. We focus on making purchasing online a pleasant experience. We offer flat rate Ground shipping on all items less than 75 pounds that can be shipped via common carrier. Our sales office is located in beautiful town of Redondo Beach, California, just a few miles South of Los Angeles International Airport. We do not stock any inventory at our sales office. Instead, we ship from several warehouses located throughout the country. Our sophisticated fulfillment system will automatically ship from the warehouse location that is closest to your shipping address! Why are our prices so low? It´s simple. We have modeled our business much like a wholesaler. We sell in huge volume and have streamlined our operations to provide you with the best combination of price and service available anywhere. Hours of Operation 8AM - 5PM PST MON - THU, 8AM - NOON PST FRIDAY[ Read more ] Copyright© Beach Audio Inc, 2010",
  "condition-message" : "",
  "seller-name" : "beachaudio",
  "item-location" : "Jonestown, Pennsylvania, United States",
  "ships-to-locations" : {
    "includes" : ["United States", "Canada", "United Kingdom", "Mexico", "Germany", "Japan", "Brazil", "France", "Australia", "Denmark", "Romania", "Slovakia", "Bulgaria", "Czech Republic", "Finland", "Hungary", "Latvia", "Lithuania", "Malta", "Estonia", "Greece", "Portugal", "Cyprus", "Slovenia", "Sweden", "Korea", "South", "Indonesia", "Taiwan", "South Africa", "Thailand", "Belgium", "Ireland", "Netherlands", "Poland", "Spain", "Italy", "Austria", "Bahamas", "Israel", "New Zealand", "Philippines", "Singapore", "Switzerland", "Norway", "Saudi Arabia", "Ukraine", "United Arab Emirates", "Qatar", "Kuwait", "Bahrain", "Croatia", "Republic of", "Malaysia", "Chile", "Colombia", "Costa Rica", "Dominican Republic", "Panama", "Trinidad and Tobago", "Guatemala", "El Salvador", "Honduras", "Jamaica", "Antigua and Barbuda", "Aruba", "Belize", "Dominica", "Grenada", "Saint Kitts-Nevis", "Saint Lucia", "Montserrat", "Turks and Caicos Islands", "Barbados", "Bangladesh", "Bermuda", "Brunei Darussalam", "Bolivia", "Ecuador", "Egypt", "French Guiana", "Guernsey", "Gibraltar", "Guadeloupe", "Iceland", "Jersey", "Jordan", "Cambodia", "Cayman Islands", "Liechtenstein", "Sri Lanka", "Luxembourg", "Monaco", "Macau", "Martinique", "Maldives", "Nicaragua", "Oman", "Peru", "Pakistan", "Paraguay", "Reunion", "Vietnam", "Uruguay"],
    "excludes" : ["Alaska/Hawaii", "US Protectorates", "APO/FPO", "PO Box", "Angola", "Cameroon", "French Polynesia", "Libya", "Mongolia", "Suriname", "Guyana", "Mauritius", "Chad", "Madagascar", "New Caledonia", "Iran", "Western Sahara", "Laos", "Congo", "Republic of the", "Seychelles", "Sudan", "Venezuela", "Somalia", "Burma", "Cuba", "Republic of", "Yemen", "Liberia", "Sierra Leone", "Central African Republic", "Niger", "Saint Pierre and Miquelon", "Tajikistan", "Anguilla", "British Virgin Islands", "Cape Verde Islands", "Saint Vincent and the Grenadines", "Botswana", "Eritrea", "Swaziland", "Lesotho"],
    "_id" : ObjectId("0e3cf38081084c50a93ff828")
  },
  "policies" : {
    "shipping" : [
      "Free shipping Free United States Standard Shipping (FedEx Ground or FedEx Home Delivery®) Free 3 day shipping Get it by Thu. Dec. 02 US $8.94 US $5.46 United States ExedEx 2Day®) Estimated between Wed. Dec. 1 and Fri. Dec. 3",
      "",
      "Taxes may be applicable at checkout. Learn more"
    ],
    "return" : [
      "After receiving the item, contact seller within Refund will be given as Return shipping 30 days Money back Seller pays for return shipping",
      "",
      ""
    ],
    "payment" : null,
    "_id" : ObjectId("de7f587f98624d23b38fa057")
  },
  "images" : [
    "https://i.ebayimg.com/images/g/s0sAAOSwq~xfkyyl/s-l2000.jpg"
  ]
}
```

Seller
```javascript
{
  "_id" : ObjectId("c542c90de79b4f5b9c3264c7"),
  "name" : "serversetc",
  "bio" : "We are LA Micro Group (UK) Ltd. trading as Servers Etc. We are an IT consultancy group with over 10 years of experience in the industry. We hold a large variety of Dell & HP Servers, Storage & Options",
  "feedback-scores" : {
    "Positive" : 317,
    "Neutral" : 1,
    "Negative" : 0,
    "_id" : ObjectId("55d26a23c48c4606a7078dd8")
  },
  "feedback-ratings" : {
    "Item as described" : 276,
    "Communication" : 269,
    "Shipping time" : 279,
    "Shipping charges" : 274,
    "_id" : ObjectId("f96b3fb14607425e948b7149")
  },
  "creation-date" : ISODate("2001-10-30T05:00:00Z"),
  "location" : "United Kingdom"
}
{
  "_id" : ObjectId("59e1c698ec7b4681bed65702"),
  "name" : "laptopsforles",
  "bio" : "Based in United States, laptopsforles has been an eBay member since Mar 10, 2003",
  "feedback-scores" : {
    "Positive" : 886,
    "Neutral" : 1,
    "Negative" : 0,
    "_id" : ObjectId("bb1a2c5685ea428fbdf19f92")
  },
  "feedback-ratings" : {
    "Item as described" : 688,
    "Communication" : 709,
    "Shipping time" : 715,
    "Shipping charges" : 675,
    "_id" : ObjectId("0f886c64301849ff8d989f5c")
  },
  "creation-date" : ISODate("2003-03-10T05:00:00Z"),
  "location" : "United States"
}
```

---

### Sample queries

TODO

[assignment]: ./res/pset2_assignment.md
[res]: ./res/
