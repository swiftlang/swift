// RUN: %target-run-simple-swift

import StdlibUnittest
import DifferentiationUnittest

var CurryingAutodiffTests = TestSuite("CurryingAutodiff")

CurryingAutodiffTests.testWithLeakChecking("StructMember") {
  struct A {
    @differentiable(wrt: (value))
    func v(_ value: Tracked<Float>) -> Tracked<Float> { return value * value }
  }

  let a = A()
  // This implicitly constructs a function (A) -> (Tracked<Float>) -> Tracked<Float>
  // which gets called with a:
  let g: @differentiable (Tracked<Float>) -> Tracked<Float> = a.v


  expectEqual(6.0, Tracked<Float>(3.0).gradient(in: g))
}

runAllTests()
