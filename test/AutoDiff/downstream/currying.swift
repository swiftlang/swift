// RUN: %target-run-simple-swift

import StdlibUnittest
import DifferentiationUnittest

var CurryingAutodiffTests = TestSuite("CurryingAutodiff")

CurryingAutodiffTests.testWithLeakChecking("StructMember") {
  struct A {
    @differentiable(wrt: (value))
    func instanceMethod(_ value: Tracked<Float>) -> Tracked<Float> { return value * value }
  }

  let a = A()
  // Referencing `a.instanceMethod` implicitly applies the curried function
  // `A.instanceMethod` of type `(A) -> (Tracked<Float>) -> Tracked<Float>` to
  // the value `a`, producing a `(Tracked<Float>) -> Tracked<Float>` value.
  // This value is then converted to a `@differentiable` function-typed value.
  let g: @differentiable (Tracked<Float>) -> Tracked<Float> = a.instanceMethod

  expectEqual(Tracked<Float>(6.0), gradient(at: 3, in: g))
}

runAllTests()
