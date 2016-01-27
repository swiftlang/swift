// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
#if _runtime(_ObjC)
import ObjectiveC
#endif

let PrintTests = TestSuite("PrintBoolean")

PrintTests.test("CustomStringConvertible") {
  func hasDescription(any: Any) {
    expectTrue(any is CustomStringConvertible)
  }

  hasDescription(Bool(true))
  hasDescription(CBool(true))
}

PrintTests.test("Printable") {
  expectPrinted("true", CBool(true))
  expectPrinted("false", CBool(false))
  
  expectPrinted("true", Bool(true))
  expectPrinted("false", Bool(false))
  
  expectPrinted("true", true)
  expectPrinted("false", false)
}

runAllTests()
