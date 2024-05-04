// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++14)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++17)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20)

// REQUIRES: executable_test

import StdlibUnittest
import StdFunction

var StdFunctionTestSuite = TestSuite("StdFunction")

StdFunctionTestSuite.test("init empty") {
  let f = FunctionIntToInt()
  expectTrue(isEmptyFunction(f))

  let copied = f
  expectTrue(isEmptyFunction(copied))
}

StdFunctionTestSuite.test("call") {
  let f = getIdentityFunction()
  expectEqual(123, f(123))
}

StdFunctionTestSuite.test("retrieve and pass back as parameter") {
  let res = invokeFunction(getIdentityFunction(), 456)
  expectEqual(456, res)
}

runAllTests()
