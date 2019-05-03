// RUN: %target-run-simple-swift

import StdlibUnittest

var CurryingAutodiffTests = TestSuite("CurryingAutodiff")

CurryingAutodiffTests.test("StructMember") {
  struct A {
    @differentiable(wrt: (value))
      func v(_ value: Float) -> Float { return value * value }
  }

  let a = A()
  // This implicitly constructs a function (A) -> (Float) -> Float
  // which gets called with a:
  let g: @differentiable (Float) -> Float = a.v


  expectEqual(6.0, Float(3.0).gradient(in: g))
}

runAllTests()
