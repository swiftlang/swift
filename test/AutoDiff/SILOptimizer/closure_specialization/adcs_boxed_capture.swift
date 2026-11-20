// RUN: %target-swift-frontend -O -sil-verify-all -sil-stop-optzns-before-lowering-ownership -emit-sil -module-name BoxedCapture -Xllvm -sil-disable-pass=autodiff-closure-specialization %s -o %t.sil
// RUN: %target-sil-opt -module-name BoxedCapture -enable-sil-verify-all -autodiff-closure-specialization -dce %t.sil -o - | %FileCheck %s

import _Differentiation

@differentiable(reverse)
@inline(never)
public func boxedSquare(_ x: Float) -> Float { x * x }

@derivative(of: boxedSquare)
@inline(__always)
public func boxedSquareVJP(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  var factor = 2 * x
  return (x * x, { seed in
    factor = 2 * x
    return factor * seed
  })
}

@differentiable(reverse, wrt: x)
public func branch(_ x: Float, _ flag: Bool) -> Float {
  if flag { return boxedSquare(x) }
  return x * x * x
}

// CHECK:      enum [[TAPE:_AD__[^ ]+_bb3__Pred__[^ ]+_spec_bb2_1_2_[^ ]+]] {
// CHECK-NEXT: case bb2((predecessor: {{[^,]+}}, (Float, Float), (Float, Float)))
// CHECK-NOT:  }
// CHECK:      case bb1((predecessor: {{[^,]+}}, {{(@callee_guaranteed )?}}(Float) -> Float))

// CHECK-LABEL: {{^}}sil [ossa] @$s12BoxedCapture6branchyS2f_SbtFTJrSUpSr :
// CHECK:         %[[#BOX:]] = alloc_box ${ var Float }
// CHECK:         %[[#ROOT:]] = function_ref @$s12BoxedCapture14boxedSquareVJPySf5value_S2fc8pullbacktSfFS2fcfU_
// CHECK-NEXT:    %[[#COPY:]] = copy_value %[[#BOX]]
// CHECK-NEXT:    %[[#CALLBACK:]] = partial_apply [callee_guaranteed] %[[#ROOT]](%[[#COPY]],
// CHECK:       tuple {{.*}}%[[#CALLBACK]])
// CHECK:       bb2:
// CHECK-NOT:     partial_apply
// CHECK:       enum $[[TAPE]], #{{[^ ]+}}.bb2!enumelt,
// CHECK:         // function_ref specialized pullback of branch(_:_:)
// CHECK:         return

// CHECK-LABEL: {{^}}// specialized pullback of branch(_:_:)
// CHECK:         %[[#MULTIPLY1:]] = function_ref @$sSf16_DifferentiationE12_vjpMultiply
// CHECK-NEXT:    %[[#]] = apply %[[#MULTIPLY1]](
// CHECK:         %[[#MULTIPLY2:]] = function_ref @$sSf16_DifferentiationE12_vjpMultiply
// CHECK-NEXT:    %[[#]] = apply %[[#MULTIPLY2]](
// CHECK:         (%[[#]], %[[#SAVED_CALLBACK:]]) = destructure_tuple
// CHECK-NEXT:    %[[#]] = apply %[[#SAVED_CALLBACK]](%[[#]]) : $@callee_guaranteed (Float) -> Float
// CHECK-NEXT:    destroy_value %[[#SAVED_CALLBACK]]
// CHECK:       } // end sil function
