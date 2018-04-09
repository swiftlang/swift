// -*- swift -*-
// RUN: %target-run-simple-swiftgyb
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

var CollectionSearchTestSuite = TestSuite("CollectionSearch")

//===----------------------------------------------------------------------===//
// firstRange tests
//===----------------------------------------------------------------------===//

CollectionSearchTestSuite.test("firstRange/foundAtEnd") {
  let string = Array("abbabaababaabba")
  guard let range = string.firstRange(of: Array("aabb")) else {
    failTest("Failed to find range of substring")
    return
  }

  expectEqual(String(string[range]), "aabb")
  let expectedRange = 10..<14
  expectEqual(expectedRange, range)
}

CollectionSearchTestSuite.test("firstRange/foundAtStart") {
  let string = Array("aabbabaababaabba")
  guard let range = string.firstRange(of: Array("aabb")) else {
    failTest("Failed to find range of substring")
    return
  }

  expectEqual(String(string[range]), "aabb")
  let expectedRange = 0..<4
  expectEqual(expectedRange, range)
}

CollectionSearchTestSuite.test("firstRange/foundAlmostAtStart") {
  let string = Array("aaabbabaababaabba")
  guard let range = string.firstRange(of: Array("aabb")) else {
    failTest("Failed to find range of substring")
    return
  }

  expectEqual(String(string[range]), "aabb")
  let expectedRange = 1..<5
  expectEqual(expectedRange, range)
}

CollectionSearchTestSuite.test("firstRange/notFound") {
  let string = Array("abbabaababaabba")
  expectNil(string.firstRange(of: Array("aabbbbb")))
}

//===----------------------------------------------------------------------===//
// lastRange tests
//===----------------------------------------------------------------------===//

CollectionSearchTestSuite.test("lastRange/found") {
  let string = Array("aabbabaababaabba")
  guard let range = string.lastRange(of: Array("aabb")) else {
    failTest("Failed to find range of substring")
    return
  }

  expectEqual(String(string[range]), "aabb")
  let expectedRange = 11..<15
  expectEqual(expectedRange, range)
}

CollectionSearchTestSuite.test("lastRange/foundAtEnd") {
  let string = Array("aabbabaababaabb")
  guard let range = string.lastRange(of: Array("aabb")) else {
    failTest("Failed to find range of substring")
    return
  }

  expectEqual(String(string[range]), "aabb")
  let expectedRange = 11..<15
  expectEqual(expectedRange, range)
}

CollectionSearchTestSuite.test("lastRange/foundAlmostAtEnd") {
  let string = Array("aaabbabaababaabba")
  guard let range = string.lastRange(of: Array("aabb")) else {
    failTest("Failed to find range of substring")
    return
  }

  expectEqual(String(string[range]), "aabb")
  let expectedRange = 12..<16
  expectEqual(expectedRange, range)
}

CollectionSearchTestSuite.test("lastRange/notFound") {
  let string = Array("abbabaababaabba")
  expectNil(string.lastRange(of: Array("aabbbbb")))
}

//===----------------------------------------------------------------------===//
// count(of:) tests
//===----------------------------------------------------------------------===//

CollectionSearchTestSuite.test("count/doesn't") {
  let string = "aa--bb--cc--dd"
  let count = string.countOccurrences(of: "---")

  expectEqual(0, count)
}

CollectionSearchTestSuite.test("count/mid-string") {
  let string = "aa--bb--cc--dd"
  let count = string.countOccurrences(of: "--")

  expectEqual(3, count)
}

CollectionSearchTestSuite.test("count/beginning") {
  let string = "--bbccdd"
  let count = string.countOccurrences(of: "--")

  expectEqual(1, count)
}

CollectionSearchTestSuite.test("count/end") {
  let string = "bbccdd--"
  let count = string.countOccurrences(of: "--")

  expectEqual(1, count)
}

CollectionSearchTestSuite.test("count/overlapping") {
  let string = "bb---cc----dd--"
  let count = string.countOccurrences(of: "--", allowingOverlaps: true)

  expectEqual(6, count)
}

//===----------------------------------------------------------------------===//
// contains(occurrenceOf:) tests
//===----------------------------------------------------------------------===//

CollectionSearchTestSuite.test("contains/doesn't") {
  let string = "aa--bb--cc--dd"
  let found = string.contains(occurrenceOf: "---")

  expectFalse(found)
}

CollectionSearchTestSuite.test("contains/mid-string") {
  let string = "aa--bb--cc--dd"
  let found = string.contains(occurrenceOf: "--")

  expectTrue(found)
}

CollectionSearchTestSuite.test("contains/beginning") {
  let string = "--bbccdd"
  let found = string.contains(occurrenceOf: "--")

  expectTrue(found)
}

CollectionSearchTestSuite.test("contains/end") {
  let string = "bbccdd--"
  let found = string.contains(occurrenceOf: "--")

  expectTrue(found)
}

runAllTests()
