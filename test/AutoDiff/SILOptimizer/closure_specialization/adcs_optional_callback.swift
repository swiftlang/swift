// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name adcs_optional_callback %s -Xllvm -sil-disable-skipping-passes -Xllvm -sil-print-after=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s < %t.passes
// RUN: %FileCheck %s --check-prefix=TUPLE < %t.passes

// The .some and .none payloads reach the same pullback argument. Its live
// optional stays intact when ADCS specializes the multiplication callback.
// CHECK:      stage HighLevel,Module+StackPromote, pass
// CHECK-NOT:  stage MidLevel,Function
// CHECK:      // reverse-mode derivative of recoveringSquare
// CHECK:        %[[#NONE:]] = enum $Optional<@callee_guaranteed (Float) -> Float>, #Optional.none!enumelt
// CHECK-NEXT:   %[[#ERROR_PAYLOAD:]] = tuple $(predecessor: {{.*}}, Optional<@callee_guaranteed (Float) -> Float>) (%[[#]], %[[#NONE]])
// CHECK-NEXT: enum ${{[^,]+}}, #[[ERROR_TAPE:[^.]+]].[[CASE:bb[0-9]+]]!enumelt, %[[#ERROR_PAYLOAD]]
// CHECK:        // function_ref closure #1 in positiveSquareVJP
// CHECK-NEXT:   %[[#ROOT:]] = function_ref
// CHECK-NEXT:   %[[#CLOSURE:]] = partial_apply {{.*}} %[[#ROOT]](
// CHECK-NEXT:   %[[#SOME:]] = enum $Optional<@callee_guaranteed (Float) -> Float>, #Optional.some!enumelt, %[[#CLOSURE]]
// CHECK-NEXT:   %[[#NORMAL_PAYLOAD:]] = tuple $(predecessor: {{.*}}, Optional<@callee_guaranteed (Float) -> Float>) (%[[#]], %[[#SOME]])
// CHECK-NEXT: enum ${{[^,]+}}, #[[NORMAL_TAPE:[^.]+]].[[CASE]]!enumelt, %[[#NORMAL_PAYLOAD]]
// CHECK-NOT:  } // end sil function
// CHECK:        // function_ref specialized pullback of recoveringSquare

// CHECK:      // specialized pullback of recoveringSquare
// CHECK:        %[[#MUL:]] = function_ref @{{.*}}_vjpMultiply
// CHECK-NEXT:   %[[#]] = apply %[[#MUL]](
// CHECK:        %[[#ERROR:]] = unchecked_enum_data {{.*}}, #[[ERROR_TAPE]].[[CASE]]!enumelt
// CHECK-NEXT:   br [[JOIN:bb[0-9]+]](%[[#]], %[[#ERROR]])
// CHECK:        %[[#NORMAL:]] = unchecked_enum_data {{.*}}, #[[NORMAL_TAPE]].[[CASE]]!enumelt
// CHECK-NEXT:   br [[JOIN]](%[[#]], %[[#NORMAL]])
// CHECK:      [[JOIN]](%[[#]] : $Float, %[[#PAYLOAD:]] : @owned $(predecessor: {{.*}}, Optional<@callee_guaranteed (Float) -> Float>))
// CHECK-NEXT:   (%[[#]], %[[#OPTIONAL:]]) = destructure_tuple %[[#PAYLOAD]]
// CHECK-NEXT:   switch_enum %[[#OPTIONAL]], case #Optional.some!enumelt: [[APPLY:bb[0-9]+]], case #Optional.none!enumelt:
// CHECK:      [[APPLY]](%[[#CALLBACK:]] : @owned $@callee_guaranteed (Float) -> Float)
// CHECK-NEXT:   %[[#]] = apply %[[#CALLBACK]](

// CHECK:     stage MidLevel,Function, pass
// CHECK:     // reverse-mode derivative of recoveringSquare
// CHECK-NOT: } // end sil function
// CHECK:       // function_ref specialized pullback of recoveringSquare

// TUPLE-LABEL: {{^}}// reverse-mode derivative of tupleRecovery(_:_:_:)
// TUPLE-NOT:     = partial_apply
// TUPLE-NOT:   enum $Optional<@callee_guaranteed
// TUPLE:         %[[#NEW_NONE:]] = enum $Optional<(_: Float)>, #Optional.none!enumelt
// TUPLE-NEXT:  {{.*}} = tuple $(predecessor: [[PRED:_AD__[^, ]+__Pred__src_0_1_wrt_0_1]], Optional<(_: Float)>) (%[[#]], %[[#NEW_NONE]])
// TUPLE-NOT:     = partial_apply
// TUPLE-NOT:   enum $Optional<@callee_guaranteed
// TUPLE:         // function_ref closure #1 in positiveSquareVJP
// TUPLE-NEXT:  {{.*}} = function_ref
// TUPLE-NEXT:    %[[#CAPTURE:]] = tuple (%[[#]])
// TUPLE-NEXT:    %[[#NEW_SOME:]] = enum $Optional<(_: Float)>, #Optional.some!enumelt, %[[#CAPTURE]]
// TUPLE-NEXT:  {{.*}} = tuple $(predecessor: [[PRED]], Optional<(_: Float)>) (%[[#]], %[[#NEW_SOME]])
// TUPLE-NOT:     = partial_apply
// TUPLE-NOT:   enum $Optional<@callee_guaranteed
// TUPLE:         // function_ref specialized pullback of tupleRecovery
// TUPLE-NEXT:    %[[#PB:]] = function_ref
// TUPLE-NEXT:  {{.*}} = partial_apply {{.*}} %[[#PB]](
// TUPLE:         return
// TUPLE-NEXT:  } // end sil function
// TUPLE-LABEL: {{^}}// specialized pullback of tupleRecovery(_:_:_:)
// TUPLE:       bb0(%[[#]] : $Float, %[[#]] : $Float, %[[#]] : $_AD__{{.*}}_spec{{.*}}):
// TUPLE-NOT:     destroy_value
// TUPLE:         return
// TUPLE-NEXT:  } // end sil function

import _Differentiation

public enum Negative: Error {
  case input
}

@differentiable(reverse)
public func positiveSquare(_ x: Float) throws -> Float {
  if x < 0 { throw Negative.input }
  return x * x
}

@derivative(of: positiveSquare)
@inline(__always)
public func positiveSquareVJP(_ x: Float) throws -> (value: Float, pullback: (Float) -> Float) {
  if x < 0 { throw Negative.input }
  return (x * x, { 2 * x * $0 })
}

@differentiable(reverse, wrt: x)
public func recoveringSquare(_ x: Float, _ flag: Bool) -> Float {
  if flag {
    do { return try positiveSquare(x) }
    catch { return x }
  }
  return x * x
}

// Selecting the second result leaves the pullback with only a tape destroy.
@differentiable(reverse, wrt: (x, y))
@inline(never)
public func tupleRecovery(_ x: Float, _ y: Float, _ flag: Bool) -> (Float, Float) {
  if flag {
    do { return (try positiveSquare(y), 1) }
    catch { return (x, 1) }
  }
  return (y * y, 6)
}

@differentiable(reverse, wrt: (x, y))
@inline(never)
public func recoverySecond(_ x: Float, _ y: Float, _ flag: Bool) -> Float {
  tupleRecovery(x, y, flag).1
}
