// RUN: rm -rf %t ; mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out -swift-version 4 && %target-run %t/a.out

// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

var DictionaryTestSuite = TestSuite("Dictionary4")

DictionaryTestSuite.test("Hashable") {
  let d1: Dictionary<Int, String> = [1: "meow", 2: "meow", 3: "meow"]
  let d2: Dictionary<Int, String> = [1: "meow", 2: "meow", 3: "mooo"]
  let d3: Dictionary<Int, String> = [1: "meow", 2: "meow", 4: "meow"]
  let d4: Dictionary<Int, String> = [1: "meow", 2: "meow", 4: "mooo"]
  checkHashable([d1, d2, d3, d4], equalityOracle: { $0 == $1 })

  let dd1: Dictionary<Int, Dictionary<Int, String>> = [1: [2: "meow"]]
  let dd2: Dictionary<Int, Dictionary<Int, String>> = [2: [1: "meow"]]
  let dd3: Dictionary<Int, Dictionary<Int, String>> = [2: [2: "meow"]]
  let dd4: Dictionary<Int, Dictionary<Int, String>> = [1: [1: "meow"]]
  let dd5: Dictionary<Int, Dictionary<Int, String>> = [2: [2: "mooo"]]
  let dd6: Dictionary<Int, Dictionary<Int, String>> = [2: [:]]
  let dd7: Dictionary<Int, Dictionary<Int, String>> = [:]
  checkHashable(
    [dd1, dd2, dd3, dd4, dd5, dd6, dd7],
    equalityOracle: { $0 == $1 })

  // Check that hash is equal even though dictionary is traversed differently
  var d5: Dictionary<Int, String> =
    [1: "meow", 2: "meow", 3: "mooo", 4: "woof", 5: "baah", 6: "mooo"]
  let expected = d5.hashValue
  for capacity in [4, 8, 16, 32, 64, 128, 256] {
    d5.reserveCapacity(capacity)
    expectEqual(d5.hashValue, expected)
  }
}

runAllTests()
