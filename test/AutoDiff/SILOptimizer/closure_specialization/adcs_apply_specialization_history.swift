// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name ApplyHistory %s -Xllvm -sil-print-generic-specialization-info -Xllvm -sil-print-around=autodiff-closure-specialization -o /dev/null 2> %t.passes
// RUN: %FileCheck %s < %t.passes

import _Differentiation

@inline(never)
@differentiable(reverse, wrt: x)
func branch<T>(_ x: Float, _ y: Float, _: T.Type, _ flag: Bool) -> Float {
  if flag { return x + y }
  return x * x
}

public func derivative(_ x: Float, _ y: Float, _ flag: Bool) -> Float {
  gradient(at: x) { branch($0, y, Int.self, flag) }
}

// Preserve the Int specialization's call-site history through both a bare
// multiplication closure and a subset thunk.
// CHECK-LABEL: {{^}}sil private [ossa] @{{[^ ]*6branch[^ ]*TJp[^ ]*Si_Tg5[^ ]*Tf1nb_n}} :
// CHECK:         %[[#MUL:]] = function_ref @[[MUL_NAME:[^ ]*_vjpMultiply[^ ]*]]
// CHECK-NEXT:  // Generic specialization information for call-site [[MUL_NAME]]:
// CHECK-NEXT:  // Caller:
// CHECK-NEXT:  // Parent: [[PULLBACK:\$s[^ ]*6branch[^ ]*TJp[^ ]*]]
// CHECK-NEXT:  // Substitutions: <Int>
// CHECK:         = apply %[[#MUL]](
// CHECK:         // function_ref autodiff subset parameters thunk for pullback
// CHECK-NEXT:    %[[#THUNK:]] = function_ref @[[THUNK_NAME:[^ ]+]]
// CHECK-NEXT:  // Generic specialization information for call-site [[THUNK_NAME]]:
// CHECK-NEXT:  // Caller:
// CHECK-NEXT:  // Parent: [[PULLBACK]]
// CHECK-NEXT:  // Substitutions: <Int>
// CHECK:         = apply %[[#THUNK]](
// CHECK-LABEL: } // end sil function
