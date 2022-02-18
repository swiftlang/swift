// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

import StdlibUnittest

var SortABITestSuite =  TestSuite("SortABI")

SortABITestSuite.test("_merge-ABI") {
  let result = withUnsafeTemporaryAllocation(of: Int.self, capacity: 10) {
    buffer -> Bool in
    _ = buffer.initialize(from: 0..<10)
    return withUnsafeTemporaryAllocation(of: Int.self, capacity: 10) {
      _merge(
        low: buffer.baseAddress! + 5,
        mid: buffer.baseAddress! + 8,
        high: buffer.baseAddress! + 9,
        buffer: $0.baseAddress!,
        by: <
      )
    }
  }
  // The return value is legacy ABI. It was originally added as a
  // workaround for a compiler bug (now fixed). See SR-14750 (rdar://45044610).
  // `_merge` always returns `true`
  expectEqual(result, true)
}

SortABITestSuite.test("_mergeRuns-ABI") {
  let result = withUnsafeTemporaryAllocation(of: Int.self, capacity: 10) {
    temp -> Bool in
    _ = temp.initialize(from: 0..<10)
    var buffer = temp
    var runs = [5..<8, 8..<9]
    return withUnsafeTemporaryAllocation(of: Int.self, capacity: 10) {
      buffer._mergeRuns(&runs, at: 1, buffer: $0.baseAddress!, by: <)
    }
  }
  // The return value is legacy ABI. It was originally added as a
  // workaround for a compiler bug (now fixed). See SR-14750 (rdar://45044610).
  // `_mergeRuns` always returns `true`
  expectEqual(result, true)
}

SortABITestSuite.test("_mergeTopRuns-ABI") {
  let result = withUnsafeTemporaryAllocation(of: Int.self, capacity: 10) {
    temp -> Bool in
    _ = temp.initialize(from: 0..<10)
    var buffer = temp
    var runs = [5..<8, 8..<9]
    return withUnsafeTemporaryAllocation(of: Int.self, capacity: 10) {
      buffer._mergeTopRuns(&runs, buffer: $0.baseAddress!, by: <)
    }
  }
  // The return value is legacy ABI. It was originally added as a
  // workaround for a compiler bug (now fixed). See SR-14750 (rdar://45044610).
  // `_mergeTopRuns` always returns `true`
  expectEqual(result, true)
}

SortABITestSuite.test("_finalizeRuns-ABI") {
  let result = withUnsafeTemporaryAllocation(of: Int.self, capacity: 10) {
    temp -> Bool in
    _ = temp.initialize(from: 0..<10)
    var buffer = temp
    var runs = [5..<8, 8..<9]
    return withUnsafeTemporaryAllocation(of: Int.self, capacity: 10) {
      buffer._finalizeRuns(&runs, buffer: $0.baseAddress!, by: <)
    }
  }
  // The return value is legacy ABI. It was originally added as a
  // workaround for a compiler bug (now fixed). See SR-14750 (rdar://45044610).
  // `_finalizeRuns` always returns `true`
  expectEqual(result, true)
}

runAllTests()
