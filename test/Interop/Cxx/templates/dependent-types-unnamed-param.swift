// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default)
//
// REQUIRES: executable_test

// A dependent parameter or result type is imported as "Any", so the call goes
// through a forwarding thunk that casts the arguments. SILGen only creates a
// binding for a parameter that has a body name, so the thunk gives unnamed C++
// parameters one instead of crashing while lowering the synthesized body.

import StdlibUnittest
import DependentTypesUnnamedParam

var DependentTypesTestSuite = TestSuite("Thunks for dependent types with unnamed parameters")

DependentTypesTestSuite.test("dependent parameter type") {
  expectEqual(dependentUnnamedParam(7 as CInt, (9 as CInt) as Any), 7)
}

DependentTypesTestSuite.test("dependent result type") {
  expectEqual(dependentResultUnnamedParam(1 as CInt, 42 as CInt) as! CInt, 42)
}

runAllTests()
