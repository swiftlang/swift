// RUN: cd %S && %swift -i %s | FileCheck %s
//
// CHECK: 3: a
// CHECK: 2: c
// CHECK: 1: d
// CHECK: 1: b

import dictionary_items
import string_split

// FIXME: Move to standard library.
extension File {
  func getAsLines() -> String[] {
    return String(this).split('\n')
  }
}

////////

// FIXME: Remove this once we get generic sort working.
func sort(array : DictionaryStringIntItem[],
          lessThan : (DictionaryStringIntItem, DictionaryStringIntItem) -> Bool) {
  for i in 0..array.length {
    for j in i+1..array.length {
      if lessThan(array[j], array[i]) {
        var temp = array[i]
        array[i] = array[j]
        array[j] = temp
      }
    }
  }
}

////////

// Open the input file.
var f = new File("test.txt")

// Count the unique lines.
//
// TODO: var d = Dictionary<String, Int>()
var d : DictionaryStringInt
// TODO: remove this
d.setBucketCount(13)
for ln in f.getAsLines() {
  d[ln] = d[ln] + 1
}

// Order the items by count (descending).
var items = d.getItems()
sort(items, { $0.value > $1.value })

// Print the result.
for item in items {
    printf("%d: %s\n", item.value, item.key)
}
