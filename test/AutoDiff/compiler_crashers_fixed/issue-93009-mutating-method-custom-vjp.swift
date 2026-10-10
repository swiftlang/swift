// RUN: %target-swift-frontend -emit-sil -sil-verify-all %s -o /dev/null

import _Differentiation

extension Float {
  mutating func foo(_ x: Float, _ y: Double) {
    self += x + 2 * Float(y)
  }

  @derivative(of: foo, wrt: (self, x, y))
  mutating func vjpFoo(_ x: Float, _ y: Double)
    -> (value: Void, pullback: (inout Float) -> (Float, Double)) {
    foo(x, y)
    return ((), { ($0, Double(2 * $0)) })
  }
}
