// To execute this file:
// $ mongo localhost:27017/ece464_pset2 queries.js

// Helper function to print out the results of a query.
function dumpCursor(cursor) {
    while (cursor.hasNext()) {
        printjson(cursor.next());
    }
}

let sellers = db.sellers;
let item = db.item_rtx_8000;

print("Examining collection: " + item);

print("Item count:");
printjson(item.count());

print("Sellers:");
printjson(sellers.distinct("name"));

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
