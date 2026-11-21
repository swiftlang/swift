// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name adcs_capture_shared %s -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s --check-prefix=FINAL < %t.sil
// RUN: %FileCheck %s < %t.passes

// CHECK-LABEL: {{^}}// reverse-mode derivative of stringShared(_:_:_:)
// CHECK:         %[[#OLD_SLOT:]] = alloc_stack $String
// CHECK:         %[[#OLD_ROOT:]] = partial_apply {{.*}}(%[[#OLD_SLOT]])
// CHECK:         %[[#OLD_SUBSET:]] = partial_apply {{.*}}(%[[#OLD_ROOT]])
// CHECK:         %[[#SHARED:]] = tuple (%[[#OLD_SUBSET]], %[[#]])
// CHECK:         = enum ${{.*}}_bb1__Pred__{{.*}}, %[[#SHARED]]
// CHECK:         = enum ${{.*}}_bb2__Pred__{{.*}}, %[[#SHARED]]
// CHECK-LABEL: {{^}}// reverse-mode derivative of stringShared(_:_:_:)
// CHECK-NOT:   alloc_stack $String
// CHECK-NOT:     dealloc_stack
// CHECK:       } // end sil function
// CHECK-LABEL: {{^}}// specialized pullback of stringShared(_:_:_:)
// CHECK:         %[[#ADAPTER_SLOT:]] = alloc_stack $String
// CHECK-NEXT:    store %[[#]] to [init] %[[#ADAPTER_SLOT]]
// CHECK-NEXT:    %[[#ADAPTER_ROOT:]] = partial_apply {{.*}}(%[[#ADAPTER_SLOT]])
// CHECK-NEXT:    dealloc_stack %[[#ADAPTER_SLOT]]
// CHECK-NEXT:    // function_ref autodiff subset parameters thunk for pullback
// CHECK-NEXT:    %[[#SUBSET:]] = function_ref
// CHECK-NEXT:  {{.*}} = apply %[[#SUBSET]](%[[#]], %[[#ADAPTER_ROOT]])
// CHECK:         destroy_value %[[#ADAPTER_ROOT]]
// CHECK:       } // end sil function

// CHECK-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: {{.*}}12stringDirect
// CHECK-NEXT:  // specialized pullback of stringDirect
// CHECK:       // specialized pullback of stringDirect
// CHECK-NOT:   } // end sil function
// CHECK:       bb[[#]](%[[#PAYLOAD:]] : @owned $(predecessor: {{.*}}, (_: String)))
// CHECK-NEXT:    (%[[#]], %[[#CAPTURES:]]) = destructure_tuple %[[#PAYLOAD]]
// CHECK-NEXT:    %[[#STRING:]] = destructure_tuple %[[#CAPTURES]]
// CHECK-NEXT:    %[[#SLOT:]] = alloc_stack $String
// CHECK-NEXT:    store %[[#STRING]] to [init] %[[#SLOT]]
// CHECK-NEXT:    // function_ref specialized closure #1 in vjpLabeled
// CHECK-NEXT:    %[[#ROOT:]] = function_ref {{.*}} : $@convention(thin) (Float, @in_guaranteed String) -> (Float, Float)
// CHECK-NEXT:    %[[#]] = apply %[[#ROOT]](%[[#]], %[[#SLOT]])
// CHECK-NEXT:    destroy_addr %[[#SLOT]]
// CHECK-NEXT:    dealloc_stack %[[#SLOT]]

import _Differentiation

@differentiable(reverse, wrt: (x, y))
public func labeled<T>(_ x: Float, _ y: Float, _ value: T) -> Float {
  2 * x + 3 * y
}

@derivative(of: labeled, wrt: (x, y))
@inline(__always)
public func vjpLabeled<T>(
  _ x: Float, _ y: Float, _ value: T
) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  (2 * x + 3 * y, { seed in withExtendedLifetime(value) { (2 * seed, 3 * seed) } })
}

// Mutually exclusive predecessor payloads share one indirect capture allocation.
// Cleanup must deallocate it only once.
@differentiable(reverse, wrt: x)
public func stringShared(_ x: Float, _ name: String, _ flag: Bool) -> Float {
  let y = labeled(x, Float(1), name)
  let z = x * x
  if flag { return y * z }
  return y + z
}

// Differentiating both inputs avoids a subset thunk and tests the direct
// call with an indirect String capture.
@differentiable(reverse, wrt: (x, y))
public func stringDirect(_ x: Float, _ y: Float, _ name: String, _ flag: Bool) -> Float {
  if flag { return labeled(x, y, name) }
  return x * y
}

// FINAL-LABEL: {{^}}// reverse-mode derivative of stringShared
// FINAL-NOT:   } // end sil function
// FINAL:         // function_ref specialized pullback of stringShared
// FINAL-NEXT:    %[[#PB:]] = function_ref
// FINAL-NEXT:    %[[#PULLBACK:]] = partial_apply {{.*}} %[[#PB]]{{[<(]}}
// FINAL-NEXT:    %[[#RESULT:]] = tuple (%[[#]], %[[#PULLBACK]])
// FINAL-NOT:   } // end sil function
// FINAL:         return %[[#RESULT]]
// FINAL-NEXT:  } // end sil function

// FINAL-LABEL: {{^}}// specialized pullback of stringShared
// FINAL-NOT:     apply {{.*}} : $@callee_guaranteed
// FINAL:       } // end sil function
