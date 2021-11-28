// This list shows some sample queries on the "gpu" query.
//
// To execute this file:
// $ mongo localhost:27017/ece464_pset2 queries.js

// Try changing this to one of the other queries.
const query = "gpu";

// Helper function to print out the results of a query.
function dumpCursor(cursor) {
    while (cursor.hasNext()) {
        printjson(cursor.next());
    }
}
const sellers = db["seller_" + query.replace(" ", "_")];
const items = db["item_" + query.replace(" ", "_")];

print("Examining collection: " + items);

print("Item count:");
printjson(items.count());

print("Top ten most frequent sellers:");
dumpCursor(items.aggregate([
    {
        $group: {
            _id: "$seller-name",
            "items-count": { $count: {} }
        }
    },
    {
        $sort: {
            "items-count": -1
        }
    },
    {
        $project: {
            seller: "$_id",
            "items-count": true,
            _id: false
        }
    },
    {
        $limit: 10
    }
]));

print("Items shipping to the United States:");
printjson(items.find({
    "ships-to-locations.includes": "United States"
}).count());

print("Price distribution (only if priced in USD):");
dumpCursor(items.aggregate([
    {
        $match: {
            "itemprops.priceCurrency": "USD",
        }
    },
    {
        $project: {
            "itemprops.price": true,
            _id: false
        }
    },
    {
        $bucket: {
            groupBy: "$itemprops.price",
            boundaries: [
                0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000,
                2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000,
                20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000,
                100000, 200000, 300000, 400000, 500000
            ],
        }
    },
    {
        $project: {
            "bin-lower-bound": "$_id",
            count: true,
            _id: false,
        }
    }
]));

print("Distinct brands:");
printjson(items.distinct("about-this-item.Brand:"));

print("Average price of items with MPN '900-1G142-2520-000' (Nvidia FE 3060 Ti):");
dumpCursor(items.aggregate([
    {
        $match: {
            "about-this-item.MPN:": "900-1G142-2520-000"
        }
    },
    {
        $group: {
            _id: null,
            "avgPrice": { $avg: "$itemprops.price" },
        }
    },
    {
        $project: {
            _id: false,
        }
    }
]));

print("Sellers count:");
printjson(sellers.count());

print("Seller creation dates:");
dumpCursor(sellers.aggregate([
    {
        $group: {
            _id: null,
            "oldest": { $min: "$creation-date" },
            "newest": { $max: "$creation-date" },
        }
    },
    {
        $project: {
            _id: false,
        }
    }
]));

print("Seller bio lengths:");
dumpCursor(sellers.aggregate([
    {
        $match: {
            bio: { $ne: null },
        }
    },
    {
        $bucket: {
            groupBy: { $strLenBytes: "$bio" },
            boundaries: [0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000]
        }
    },
    {
        $project: {
            "bin-lower-bound": "$_id",
            count: true,
            _id: false,
        }
    }
]));

print("Seller average ratings:");
dumpCursor(sellers.aggregate([
    {
        $group: {
            _id: null,
            "positive": { $avg: "$feedback-scores.Positive" },
            "neutral": { $avg: "$feedback-scores.Neutral" },
            "negative": { $avg: "$feedback-scores.Negative" },
        }
    },
    {
        $project: {
            _id: false,
        }
    }
]));

print("Seller average feedback ratings:");
dumpCursor(sellers.aggregate([
    {
        $group: {
            _id: null,
            "item-as-described": { $avg: "$feedback-ratings.Item as described" },
            "communication": { $avg: "$feedback-ratings.Communication" },
            "shipping-time": { $avg: "$feedback-ratings.Shipping time" },
            "shipping-charges": { $avg: "$feedback-ratings.Shipping charges" },
        }
    },
    {
        $project: {
            _id: false,
        }
    }
]));

print("Seller locations:");
printjson(sellers.distinct("location"));
