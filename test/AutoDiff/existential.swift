// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var ExistentialTests = TestSuite("Existential")

protocol A {
  @differentiable(wrt: x)
  func a(_ x: Tracked<Float>) -> Tracked<Float>
}
func b(g: A) -> Tracked<Float> { return (3.0 as Tracked<Float>).gradient() { x in g.a(x) } }

struct B : A {
  @differentiable(wrt: x)
  func a(_ x: Tracked<Float>) -> Tracked<Float> { return x * 5.0 }
}

ExistentialTests.testWithLeakChecking("vjp/adjoint constructed with existentials.") {
  expectEqual(5.0, b(g: B()))
}

runAllTests()
