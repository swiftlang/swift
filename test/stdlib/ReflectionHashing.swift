// RUN: %target-build-swift -module-name a %s -o %t.out
// RUN: %target-run %t.out
// REQUIRES: executable_test

// This file contains reflection tests that depend on hash values.
// Don't add other tests here.
//

import StdlibUnittest


var Reflection = TestSuite("Reflection")

Reflection.test("Dictionary/Empty") {
  let dict = [Int : Int]()

  var output = ""
  dump(dict, to: &output)

  var expected = "- 0 key/value pairs\n"

  expectEqual(expected, output)
}

Reflection.test("Dictionary") {
  let dict = ["One": 1, "Two": 2, "Three": 3, "Four": 4, "Five": 5]

  var output = ""
  dump(dict, to: &output)

  // The order of elements in output depends on the hash values of dict's items,
  // which isn't deterministic. However, iterating over dict will get us the
  // same elements in the same order.
  var expected = ""
  expected += "▿ 5 key/value pairs\n"
  for (key, value) in dict {
    expected += "  ▿ (2 elements)\n"
    expected += "    - key: \"\(key)\"\n"
    expected += "    - value: \(value)\n"
  }
  expectEqual(expected, output)
}

Reflection.test("Set") {
  let s = Set(1...5)

  var output = ""
  dump(s, to: &output)

  // The order of elements in output depends on the hash values of dict's items,
  // which isn't deterministic. However, iterating over dict will get us the
  // same elements in the same order.
  var expected = ""
  expected += "▿ 5 members\n"
  for i in s {
    expected += "  - \(i)\n"
  }

  expectEqual(expected, output)
}

runAllTests()

