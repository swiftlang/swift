// RUN: %target-swift-frontend -emit-sil -verify %s

// SR-12744: Pullback generation crash for unhandled indirect result.
// May be due to inconsistent derivative function type calculation logic in
// `VJPEmitter::createEmptyPullback`.

import _Differentiation

class Class: Differentiable {
  @differentiable(wrt: (self, x))
  @differentiable(wrt: x)
  func f(_ x: Float) -> Float { x }
}

func test<C: Class>(_ c: C, _ x: Float) {
  _ = gradient(at: c, x) { c, x in c.f(x) }
  _ = gradient(at: x) { x in c.f(x) }
}
