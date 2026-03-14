// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++17)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20)

// REQUIRES: executable_test

// In Microsoft STL, all overloads of std::any_cast return a dependent templated
// type, which Swift isn't able to instantiate. 
// UNSUPPORTED: OS=windows-msvc

import StdlibUnittest
import StdAny
import CxxStdlib

var StdAnyTestSuite = TestSuite("StdAny")

StdAnyTestSuite.test("std.any_cast<std.string>") {
  let a1 = getStdAnyString()
  let c1 = std.any_cast(a1) as std.string
  expectEqual(c1, std.string("abc210"))
}

runAllTests()
