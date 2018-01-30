// RUN: %target-resilience-test
// REQUIRES: executable_test

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
