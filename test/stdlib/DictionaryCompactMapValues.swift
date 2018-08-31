// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

var tests = TestSuite("CompactMapValues")

tests.test("DefaultReturnType") {
  var result = ["a": "1", "c": "3"].compactMapValues { $0 }
  expectType([String: String].self, &result)
}

tests.test("ExplicitTypeContext") {
  expectEqual(["a": "1", "c": "3"],
    ["a": "1", "b": nil, "c": "3"].compactMapValues({$0})
  )
  expectEqual(["a": 1, "b": 2],
    ["a": "1", "b": "2", "c": "three"].compactMapValues(Int.init)
  )
}

runAllTests()
