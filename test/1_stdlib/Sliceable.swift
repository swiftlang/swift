// RUN: %target-run-simple-swift

// XFAIL: interpret

import StdlibUnittest

var SliceableTests = TestSuite("SliceableTests")

SliceableTests.test("dropFirstLast") {
  if true {
    let a = [2, 3, 5, 7, 11]
    expectEqual(a[a.startIndex.successor()..<a.endIndex], dropFirst(a))
    expectEqual(a[a.startIndex..<a.endIndex.predecessor()], dropLast(a))
  }
  if true {
    let a = "bird in the hand"
    expectEqual(a[a.startIndex.successor()..<a.endIndex], dropFirst(a))
    expectEqual(a[a.startIndex..<a.endIndex.predecessor()], dropLast(a))
  }
}

SliceableTests.test("prefixSuffix") {
  if true {
    let a = [2, 3, 5, 7, 11]
    let count = Swift.count(a)
    expectEqualSequence([], prefix(a, -10))
    expectEqualSequence([], suffix(a, -10))
    expectEqualSequence(a, prefix(a, count + 1))
    expectEqualSequence(a, prefix(a, count))
    expectEqualSequence(dropLast(a), prefix(a, count - 1))
    expectEqualSequence(dropLast(dropLast(a)), prefix(a, count - 2))
    
    expectEqualSequence(a, suffix(a, count + 1))
    expectEqualSequence(a, suffix(a, count))
    expectEqualSequence(dropFirst(a), suffix(a, count - 1))
    expectEqualSequence(dropFirst(dropFirst(a)), suffix(a, count - 2))
  }

  if true {
    let a = "bird in the hand"
    let count = Swift.count(a)
    expectEqualSequence("", prefix(a, -10))
    expectEqualSequence("", suffix(a, -10))
    expectEqualSequence(a, prefix(a, count + 1))
    expectEqualSequence(a, prefix(a, count))
    expectEqualSequence(dropLast(a), prefix(a, count - 1))
    expectEqualSequence(dropLast(dropLast(a)), prefix(a, count - 2))
    
    expectEqualSequence(a, suffix(a, count + 1))
    expectEqualSequence(a, suffix(a, count))
    expectEqualSequence(dropFirst(a), suffix(a, count - 1))
    expectEqualSequence(dropFirst(dropFirst(a)), suffix(a, count - 2))
  }
}

runAllTests()

