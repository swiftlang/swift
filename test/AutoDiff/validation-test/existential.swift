// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var ExistentialTests = TestSuite("Existential")

protocol A {
  @differentiable(reverse, wrt: x)
  func a(_ x: Float) -> Float
}
func b(g: A) -> Float {
  return gradient(at: 3) { x in g.a(x) }
}

struct B : A {
  @differentiable(reverse, wrt: x)
  func a(_ x: Float) -> Float { return x * 5 }
}

ExistentialTests.test("Existential method VJP-Tracked") {
  expectEqual(5.0, b(g: B()))
}

/* Temporary disabled until https://github.com/swiftlang/swift/issues/84840 is fixed
   We cannot use `Tracked<T>` :(
protocol ATracked {
  @differentiable(reverse, wrt: x)
  func a(_ x: Tracked<Float>) -> Tracked<Float>
}
func b(g: ATracked) -> Tracked<Float> {
  return gradient(at: 3) { x in g.a(x) }
}

struct BTracked : ATracked {
  @differentiable(reverse, wrt: x)
  func a(_ x: Tracked<Float>) -> Tracked<Float> { return x * 5 }
}

ExistentialTests.testWithLeakChecking("Existential method VJP-Tracked") {
  expectEqual(5.0, b(g: BTracked()))
}
*/

runAllTests()
