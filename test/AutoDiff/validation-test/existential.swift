// RUN: %target-run-simple-swift(-Xfrontend -requirement-machine=off)
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var ExistentialTests = TestSuite("Existential")

protocol A {
  @differentiable(reverse, wrt: x)
  func a(_ x: Tracked<Float>) -> Tracked<Float>
}
func b(g: A) -> Tracked<Float> {
  return gradient(at: 3) { x in g.a(x) }
}

struct B : A {
  @differentiable(reverse, wrt: x)
  func a(_ x: Tracked<Float>) -> Tracked<Float> { return x * 5 }
}

ExistentialTests.testWithLeakChecking("Existential method VJP") {
  expectEqual(5.0, b(g: B()))
}

runAllTests()
