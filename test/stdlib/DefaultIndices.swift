// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

import StdlibUnittest

var suite = TestSuite("DefaultIndices")
defer { runAllTests() }

extension Collection {
  func genericDistance(from start: Index, to end: Index) -> Int {
    return distance(from: start, to: end)
  }

  func genericIndex(_ i: Index, offsetBy distance: Int) -> Index {
    return index(i, offsetBy: distance)
  }

  func genericIndex(
    _ i: Index, offsetBy distance: Int, limitedBy limit: Index
  ) -> Index? {
    return index(i, offsetBy: distance, limitedBy: limit)
  }
}

suite.test("Bidirectional dispatch") {
  guard #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) else {
    // This used to cause a runtime trap until https://github.com/apple/swift/pull/32019
    return
  }
  var r = (0...10).indices
  expectType(DefaultIndices<ClosedRange<Int>>.self, &r)

  let d = r.distance(from: r.endIndex, to: r.startIndex)
  let d2 = r.genericDistance(from: r.endIndex, to: r.startIndex)
  expectEqual(d, d2)

  let i = r.index(r.endIndex, offsetBy: -1)
  let i2 = r.genericIndex(r.endIndex, offsetBy: -1)
  expectEqual(i, i2)

  let j = r.index(r.startIndex, offsetBy: -1, limitedBy: r.startIndex)
  let j2 = r.genericIndex(r.startIndex, offsetBy: -1, limitedBy: r.startIndex)
  expectEqual(j, j2)
}
