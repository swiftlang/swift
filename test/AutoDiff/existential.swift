// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var ExistentialTests = TestSuite("Existential")

protocol A {
  @differentiable(wrt: x)
  func a(_ x: Float) -> Float
}
func b(g: A) -> Float { return (3.0 as Float).gradient() { x in g.a(x) } }

struct B : A {
  @differentiable(wrt: x)
  func a(_ x: Float) -> Float { return x * 5.0 }
}

ExistentialTests.test("primal/adjoint constructed with existentials.") {
  expectEqual(5.0, b(g: B()))
}

runAllTests()
