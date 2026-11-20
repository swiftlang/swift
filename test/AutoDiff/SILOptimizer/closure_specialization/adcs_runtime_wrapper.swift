// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name adcs_runtime_wrapper %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation

public typealias Pullback = (Float) -> Float
public typealias Wrapper = (Float, Pullback) -> Float

@differentiable(reverse, wrt: x)
@inline(never)
public func runtimeWrapped(_ x: Float, _ label: String, _ wrapper: @escaping Wrapper) -> Float { 2*x }

@derivative(of: runtimeWrapped, wrt: x)
@inline(__always)
public func vjpRuntimeWrapped(_ x: Float, _ label: String, _ wrapper: @escaping Wrapper)
  -> (value: Float, pullback: Pullback) {
  let pullback: Pullback = { seed in withExtendedLifetime(label) { 2*seed } }
  return (2*x, { seed in wrapper(seed, pullback) })
}

@differentiable(reverse, wrt: x)
public func branchDynamic(_ x: Float, _ label: String, _ flag: Bool, _ wrapper: @escaping Wrapper) -> Float {
  if flag { return runtimeWrapped(x, label, wrapper) }
  return x*x
}

// CHECK-LABEL: {{^}}// reverse-mode derivative of branchDynamic
// CHECK-NOT:   } // end sil function
// CHECK:         // function_ref specialized pullback of branchDynamic
// CHECK-NEXT:    %[[#PB:]] = function_ref
// CHECK-NEXT:    %[[#PULLBACK:]] = partial_apply {{.*}} %[[#PB]]{{[<(]}}
// CHECK-NEXT:    %[[#RESULT:]] = tuple (%[[#]], %[[#PULLBACK]])
// CHECK-NOT:   } // end sil function
// CHECK:         return %[[#RESULT]]
// CHECK-NEXT:  } // end sil function

// CHECK-LABEL: {{^}}// specialized pullback of branchDynamic
// CHECK-NOT:     = partial_apply
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed (Float) -> Float
// CHECK:       } // end sil function
