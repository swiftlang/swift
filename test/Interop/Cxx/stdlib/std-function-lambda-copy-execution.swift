// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -Xcc -std=c++14)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -Xcc -std=c++17)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -Xcc -std=c++20)
//
// REQUIRES: executable_test

// Keep this separate from use-std-function.swift, whose other uses of
// std::function could emit the copy constructor and hide this regression.
import StdFunction
import StdlibUnittest

var StdFunctionLambdaCopyTestSuite = TestSuite("StdFunctionLambdaCopy")

StdFunctionLambdaCopyTestSuite.test("copy lambda capturing std::function by move") {
  expectEqual(42, IntFunctionLambdaCaller().call(41))
}

runAllTests()
