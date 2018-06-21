// -*- swift -*-
// RUN: %target-run-simple-swift
// REQUIRES: executable_test


import StdlibUnittest
import StdlibCollectionUnittest

var CollectionSearchTestSuite = TestSuite("CollectionSearch")

//===----------------------------------------------------------------------===//
// LazyRangeCollection tests
//===----------------------------------------------------------------------===//

CollectionSearchTestSuite.test("LazyRangeCollection/sanityCheck") {
  let string = "--123--456--"
  do {
    let lazyCollection = LazyRangeCollection(string, Array("--"), overlapping: false) //TODO: make sure this tests passes with true
    let reversed = lazyCollection.reversed()
  
    expectEqual(3, lazyCollection.count)
    expectTrue(lazyCollection.allSatisfy { string[$0].elementsEqual("--") } )
    expectEqual(3, Set(lazyCollection).count)
  
    expectEqual(3, reversed.count)
    expectTrue(reversed.allSatisfy { string[$0].elementsEqual("--") } )
    expectEqual(3, Set(reversed).count)
  
    let slice = lazyCollection[...]
    let sliceRange = slice.first!
    expectEqual("--", string[sliceRange])

    let end = lazyCollection.endIndex
    let lastIndex = lazyCollection.index(before: end)
    let range = lazyCollection[lastIndex]
    let index1234 = string.index(before: range.lowerBound)
    expectEqual("6", string[index1234])
  
    let secondToLastIndex = lazyCollection.index(before: lastIndex)
    let secondToLastRange = lazyCollection[secondToLastIndex]
    let indexBefore = string.index(before: secondToLastRange.lowerBound)
    expectEqual("3", string[indexBefore])
  }

  do {
    let coll = LazySplitCollection(string, separator: "--", omittingEmptySubsequences: true, behavior: .excluded)
    let slice = coll[...]
    let component = slice.first!
    expectEqual("123", component)

    let end = coll.endIndex
    let lastIndex = coll.index(before: end)
    expectEqual("456", coll[lastIndex])
  }
}

//===----------------------------------------------------------------------===//
// firstRange tests
//===----------------------------------------------------------------------===//

CollectionSearchTestSuite.test("firstRange/sanityCheck") {
  do {
    let string = "--123--456--"
    let sIndex = string.index(after: string.startIndex)
    guard let sliceRange = string[sIndex...].firstRange(of: "--") else {
      failTest("Failed to find sliceRange of substring")
      return
    }
    let index = string.index(before: sliceRange.lowerBound)
    let c = string[index]
    expectEqual("3", c)
  }
  do {
    let string = "--123--456--"
    let reversed = string.reversed()
    let index = reversed.index(after: reversed.startIndex)
    guard let sliceRange = reversed[index...].firstRange(of: "--") else {
      failTest("Failed to find sliceRange of substring")
      return
    }
    let reversedIndex = reversed.index(before: sliceRange.lowerBound)

    expectEqual("4", reversed[reversedIndex])
    expectEqual("4", string[string.index(before: reversedIndex.base)])
  }
}

CollectionSearchTestSuite.test("firstRange/foundAtEnd") {
  let string = Array("abbabaababaabba")
  guard let sliceRange = string.firstRange(of: Array("aabb")) else {
    failTest("Failed to find sliceRange of substring")
    return
  }

  expectEqual(String(string[sliceRange]), "aabb")
  let expectedRange = 10..<14
  expectEqual(expectedRange, sliceRange)
}

CollectionSearchTestSuite.test("firstRange/foundAtStart") {
  let string = Array("aabbabaababaabba")
  guard let sliceRange = string.firstRange(of: Array("aabb")) else {
    failTest("Failed to find sliceRange of substring")
    return
  }

  expectEqual(String(string[sliceRange]), "aabb")
  let expectedRange = 0..<4
  expectEqual(expectedRange, sliceRange)
}

CollectionSearchTestSuite.test("firstRange/foundAlmostAtStart") {
  let string = Array("aaabbabaababaabba")
  guard let sliceRange = string.firstRange(of: Array("aabb")) else {
    failTest("Failed to find sliceRange of substring")
    return
  }

  expectEqual(String(string[sliceRange]), "aabb")
  let expectedRange = 1..<5
  expectEqual(expectedRange, sliceRange)
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
  guard let sliceRange = string.lastRange(of: Array("aabb")) else {
    failTest("Failed to find sliceRange of substring")
    return
  }

  expectEqual(String(string[sliceRange]), "aabb")
  let expectedRange = 11..<15
  expectEqual(expectedRange, sliceRange)
}

CollectionSearchTestSuite.test("lastRange/foundAtEnd") {
  let string = Array("aabbabaababaabb")
  guard let sliceRange = string.lastRange(of: Array("aabb")) else {
    failTest("Failed to find sliceRange of substring")
    return
  }

  expectEqual(String(string[sliceRange]), "aabb")
  let expectedRange = 11..<15
  expectEqual(expectedRange, sliceRange)
}

CollectionSearchTestSuite.test("lastRange/foundAlmostAtEnd") {
  let string = Array("aaabbabaababaabba")
  guard let sliceRange = string.lastRange(of: Array("aabb")) else {
    failTest("Failed to find sliceRange of substring")
    return
  }

  expectEqual(String(string[sliceRange]), "aabb")
  let expectedRange = 12..<16
  expectEqual(expectedRange, sliceRange)
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
  let count = string.count(of: "---")

  expectEqual(0, count)
}

CollectionSearchTestSuite.test("count/mid-string") {
  let string = "aa--bb--cc--dd"
  let count = string.count(of: "--")

  expectEqual(3, count)
}

CollectionSearchTestSuite.test("count/beginning") {
  let string = "--bbccdd"
  let count = string.count(of: "--")

  expectEqual(1, count)
}

CollectionSearchTestSuite.test("count/end") {
  let string = "bbccdd--"
  let count = string.count(of: "--")

  expectEqual(1, count)
}

CollectionSearchTestSuite.test("count/overlapping") {
  let string = "bb---cc----dd--"
  let count = string.count(of: "--", overlapping: true)

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
