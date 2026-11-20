// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name adcs_capture_generic %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation

@differentiable(reverse, wrt: x)
public func tagged<T>(_ x: Float, _ tag: T.Type) -> Float {
  tag == Int.self ? 2 * x : 3 * x
}

@derivative(of: tagged, wrt: x)
@inline(__always)
public func vjpTagged<T>(
  _ x: Float, _ tag: T.Type
) -> (value: Float, pullback: (Float) -> Float) {
  (tagged(x, tag), { seed in tag == Int.self ? 2 * seed : 3 * seed })
}

@differentiable(reverse, wrt: x)
public func genericBranch<T>(_ x: Float, _ tag: T.Type, _ flag: Bool) -> Float {
  if flag { return tagged(x, tag) }
  return tagged(x, tag) + x
}

// CHECK-LABEL: {{^}}// reverse-mode derivative of genericBranch<A>
// CHECK-NOT:   } // end sil function
// CHECK:         // function_ref specialized pullback of genericBranch<A>
// CHECK-NEXT:    %[[#PB:]] = function_ref
// CHECK-NEXT:    %[[#PULLBACK:]] = partial_apply {{.*}} %[[#PB]]{{[<(]}}
// CHECK-NEXT:    %[[#RESULT:]] = tuple (%[[#]], %[[#PULLBACK]])
// CHECK-NOT:   } // end sil function
// CHECK:         return %[[#RESULT]]
// CHECK-NEXT:  } // end sil function

// CHECK-LABEL: {{^}}// specialized pullback of genericBranch<A>
// CHECK:       sil {{.*}} <{{.*}}> (Float,
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function
