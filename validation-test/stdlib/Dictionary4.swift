// RUN: rm -rf %t ; mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out -swift-version 4 && %target-run %t/a.out

// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

var DictionaryTestSuite = TestSuite("Dictionary4")

DictionaryTestSuite.test("Hashable") {
  let d1: [Dictionary<Int, String>] = [
    [1: "meow", 2: "meow", 3: "meow"],
    [1: "meow", 2: "meow", 3: "mooo"],
    [1: "meow", 2: "meow", 4: "meow"],
    [1: "meow", 2: "meow", 4: "mooo"]]
  checkHashable(d1, equalityOracle: { $0 == $1 })

  let d2: [Dictionary<Int, Dictionary<Int, String>>] = [
    [1: [2: "meow"]],
    [2: [1: "meow"]],
    [2: [2: "meow"]],
    [1: [1: "meow"]],
    [2: [2: "mooo"]],
    [2: [:]],
    [:]]
  checkHashable(d2, equalityOracle: { $0 == $1 })

  // Dictionary should hash itself in a way that ensures instances get correctly
  // delineated even when they are nested in other commutative collections.
  // These are different Sets, so they should produce different hashes:
  let remix: [Set<Dictionary<String, Int>>] = [
    [["Blanche": 1, "Rose": 2], ["Dorothy": 3, "Sophia": 4]],
    [["Blanche": 1, "Dorothy": 3], ["Rose": 2, "Sophia": 4]],
    [["Blanche": 1, "Sophia": 4], ["Rose": 2, "Dorothy": 3]]
  ]
  checkHashable(remix, equalityOracle: { $0 == $1 })

  // Dictionary ordering is not guaranteed to be consistent across equal
  // instances. In particular, ordering is highly sensitive to the size of the
  // allocated storage buffer. Generate a few copies of the same dictionary with
  // different capacities, and verify that they compare and hash the same.
  var variants: [Dictionary<String, Int>] = []
  for i in 4 ..< 12 {
    var set: Dictionary<String, Int> = [
      "one": 1,   "two": 2,
      "three": 3, "four": 4,
      "five": 5,  "six": 6]
    set.reserveCapacity(1 << i)
    variants.append(set)
  }
  checkHashable(variants, equalityOracle: { _, _ in true })
}

runAllTests()
