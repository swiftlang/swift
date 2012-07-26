// RUN: cd %S && %swift -i %s | FileCheck %s
//
// XFAIL: *
//
// CHECK: 3: a
// CHECK: 2: c
// CHECK: 1: d
// CHECK: 1: b

// Open the input file.
var f = new File("test.txt")

// Count the unique lines.
var d : Dictionary<String, Int>
for ln in f.getAsLines() {
  ++d[ln]
}

// Order the items by count.
var items = d.getItems()
sort(items, { $0.value > $1.value })

// Print the result.
for item in items {
    printf("%d: %s\n", item.value, item.key)
}
