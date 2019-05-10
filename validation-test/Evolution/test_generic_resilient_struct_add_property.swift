// RUN: %target-resilience-test
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import StdlibUnittest
import generic_resilient_struct_add_property

extension S {
  public func answer() -> Int {
    return T.compute()
  }
}

var GenericStructAddPropertyTest = TestSuite("GenericStructAddPropertyTest")
GenericStructAddPropertyTest.test("AddStoredProperty") {
  let s = S<A>()
  expectEqual(s.answer(), 42)
}

runAllTests()
