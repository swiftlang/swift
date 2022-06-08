// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test
//
// Crash when running on windows: rdar://88391102
// XFAIL: OS=windows-msvc

import StdlibUnittest
import Methods

var CxxMethodTestSuite = TestSuite("CxxMethods")

CxxMethodTestSuite.test("() -> Void") {
  var instance = HasMethods()

  instance.nonConstMethod()
  instance.constMethod()
}

CxxMethodTestSuite.test("(Int) -> Int") {
  var instance = HasMethods()

  expectEqual(42, instance.nonConstPassThrough(42))
  expectEqual(42, instance.constPassThrough(42))
}

CxxMethodTestSuite.test("(Int, Int) -> Int") {
  var instance = HasMethods()

  expectEqual(42, instance.nonConstSum(40, 2))
  expectEqual(42, instance.constSum(40, 2))
}

CxxMethodTestSuite.test("(NonTrivialInWrapper, NonTrivialInWrapper) -> Int") {
  var instance = HasMethods()

  expectEqual(42, instance.nonConstSum(NonTrivialInWrapper(value: 40), NonTrivialInWrapper(value: 2)))
  expectEqual(42, instance.constSum(NonTrivialInWrapper(value: 40), NonTrivialInWrapper(value: 2)))
}

CxxMethodTestSuite.test("(NonTrivialInWrapper, NonTrivialInWrapper) -> NonTrivialInWrapper") {
  var instance = HasMethods()

  expectEqual(42, instance.nonConstSumAsWrapper(NonTrivialInWrapper(value: 40), NonTrivialInWrapper(value: 2)).value)
  expectEqual(42, instance.constSumAsWrapper(NonTrivialInWrapper(value: 40), NonTrivialInWrapper(value: 2)).value)
}

CxxMethodTestSuite.test("(Int) -> NonTrivialInWrapper") {
  var instance = HasMethods()

  expectEqual(42, instance.nonConstPassThroughAsWrapper(42).value)
  expectEqual(42, instance.constPassThroughAsWrapper(42).value)
}

runAllTests()
