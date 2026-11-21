// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil %s | %FileCheck %s

import _Differentiation

// Put a second __Pred__ marker in the tape's generic signature.
public protocol P__Pred__Suffix {}
extension Int: P__Pred__Suffix {}

@inline(never)
@differentiable(reverse, wrt: x)
public func branch<T: P__Pred__Suffix>(_ x: Float, _ t: T, _ flag: Bool) -> Float {
  if flag { return x * x }
  return x + x
}

@inline(never)
public func derivative<T: P__Pred__Suffix>(_ x: Float, _ t: T, _ flag: Bool) -> Float {
  gradient(at: x, of: { branch($0, t, flag) })
}

precondition(derivative(2, 1, true) == 4)
precondition(derivative(2, 1, false) == 2)

// CHECK-LABEL: {{^}}// specialized pullback of branch<A>(_:_:_:)
