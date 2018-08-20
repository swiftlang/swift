// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var SimpleMathTests = TestSuite("SimpleMath")

SimpleMathTests.test("Arithmetics") {
  func foo(_ x: Float, _ y: Float) -> Float {
    return -x * y
  }
  let dfoo = #gradient(foo)
  expectEqual(dfoo(3, 4), (-4, -3))
}

SimpleMathTests.test("FunctionCall") {
  func foo(_ x: Float, _ y: Float) -> Float {
    return x * 3 + bar(3) * y
  }
  func bar(_ x: Float) -> Float {
    return 3 * x
  }
  let dfoo = #gradient(foo)
  expectEqual(dfoo(3, 4), (3, 9))
}

runAllTests()
