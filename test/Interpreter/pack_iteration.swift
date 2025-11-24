// RUN: %target-run-simple-swift

// REQUIRES: executable_test

import StdlibUnittest

var packIter = TestSuite("PackIteration")

packIter.test("break") {
  func breakTest<each T: Hashable>(_ ts: (repeat (each T)?)) -> [AnyHashable] {
    var result: [AnyHashable] = []

    for t in repeat each ts {
      if let t {
        result.append(t)
      } else {
	    break
      }
    }

    return result
  }

  let results = breakTest((0, "hello", nil as Bool?, 3.14))
  expectEqual(results, [0, "hello"] as [AnyHashable])
}

packIter.test("break") {
  func continueTest<each T: Hashable>(_ ts: (repeat (each T)?)) -> [AnyHashable] {
    var result: [AnyHashable] = []

    for t in repeat each ts {
      if let t {
        result.append(t)
      } else {
	    continue
      }
    }

    return result
  }

  let results = continueTest((0, "hello", nil as Bool?, 3.14))
  expectEqual(results, [0, "hello", 3.14] as [AnyHashable])
}

runAllTests()