// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name adcs_same_case_source %s -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.before
// RUN: %FileCheck %s < %t.sil
// RUN: %FileCheck %s --check-prefix=BEFORE < %t.before
// RUN: %FileCheck %s --check-prefix=LATE < %t.before

// BEFORE-LABEL: {{^}}// reverse-mode derivative of twoSelections
// BEFORE:       bb3:
// BEFORE:         %[[#FIRST:]] = tuple (%[[#CALLBACK:]])
// BEFORE-NEXT:    %[[#]] = enum [[ENUMTYPE:\$_AD__[^, ]+]], [[ENUMCASE:#[^ ]+]], %[[#FIRST]]
// BEFORE-NOT:   } // end sil function
// BEFORE:       bb4:
// BEFORE:         %[[#SECOND:]] = tuple (%[[#CALLBACK]])
// BEFORE-NEXT:    %[[#]] = enum [[ENUMTYPE]], [[ENUMCASE]], %[[#SECOND]]
// BEFORE-NOT:   } // end sil function
// BEFORE:         return
// BEFORE-NEXT:  } // end sil function

// LATE:      stage MidLevel,Function
// LATE:      // reverse-mode derivative of twoSelections(_:_:_:)
// LATE:        // function_ref specialized pullback of twoSelections(_:_:_:)
// LATE-NEXT:   %[[#]] = function_ref @[[UNCHANGED:[^ ]+]]
// LATE:      } // end sil function
// LATE-NOT:  *** New SIL function
// LATE:      *** SIL function after {{.*}}stage MidLevel,Function
// LATE-NEXT: // reverse-mode derivative of twoSelections(_:_:_:)
// LATE:        function_ref @[[UNCHANGED]]

// LATE:      stage MidLevel,Function
// LATE:      // reverse-mode derivative of squaredSelection(_:_:_:)
// LATE:      bb0(%[[#X:]] : $Float,
// LATE:      bb3:
// LATE-NEXT:   %[[#LEFT_CAPTURE:]] = tuple (%[[#X]], %[[#X]])
// LATE-NEXT:   %[[#LEFT:]] = tuple (%[[#LEFT_CAPTURE]])
// LATE-NEXT:   %[[#]] = enum [[TYPE:\$_AD__[^, ]+]], [[CASE:#[^ ]+]], %[[#LEFT]]
// LATE:      bb4:
// LATE-NEXT:   %[[#RIGHT_CAPTURE:]] = tuple (%[[#X]], %[[#X]])
// LATE-NEXT:   %[[#RIGHT:]] = tuple (%[[#RIGHT_CAPTURE]])
// LATE-NEXT:   %[[#]] = enum [[TYPE]], [[CASE]], %[[#RIGHT]]
// LATE:        // function_ref closure #1 in delayedSquareVJP
// LATE-NEXT:   %[[#DELAYED_FN:]] = function_ref
// LATE-NEXT:   %[[#DELAYED:]] = partial_apply {{.*}} %[[#DELAYED_FN]](
// LATE:        = tuple $(predecessor: {{.*}}, @callee_guaranteed (Float) -> Float) (%[[#]], %[[#DELAYED]])
// LATE:      } // end sil function
// LATE:      *** New SIL function in {{.*}}stage MidLevel,Function, pass: AutodiffClosureSpecialization
// LATE-NEXT: // {{.*}}pullback of squaredSelection(_:_:_:)

// CFG duplication creates separate tuples for the same case with identical
// payload operands.

import _Differentiation

@differentiable(reverse)
public func twoSelections(_ x: Float, _ first: Float?, _ second: Float?) -> Float {
  let square = x * x
  let intermediate: Float
  if let value = first {
    if value < square { intermediate = value }
    else { intermediate = square }
  } else { intermediate = square }
  if let value = second {
    if value < intermediate { return value }
    return intermediate
  }
  return intermediate
}

// After the first ADCS visit specializes x * x, sinking duplicates its
// capture tuple. Late inlining exposes delayedSquare's separate callback.
@differentiable(reverse)
public func squaredSelection(_ x: Float, _ first: Float?, _ second: Float?) -> Float {
  let square = x * x
  var intermediate = square
  if let value = first {
    if value < square { intermediate = value }
  }
  let shifted = delayedSquare(intermediate)
  if let value = second {
    if value < shifted { return value }
    return shifted
  }
  return shifted
}

@inline(never)
@differentiable(reverse)
public func delayedSquare(_ x: Float) -> Float { x * x }

@_semantics("inline_late")
@derivative(of: delayedSquare)
public func delayedSquareVJP(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  (x * x, { seed in 2 * x * seed })
}

@inline(never)
@differentiable(reverse, wrt: x)
public func maybeSquare(_ x: Float, _ square: Bool) -> Float { square ? x * x : x }

@inline(__always)
@derivative(of: maybeSquare, wrt: x)
public func maybeSquareVJP(_ x: Float, _ square: Bool) -> (value: Float, pullback: (Float) -> Float) {
  if square { return (x * x, { seed in 2 * x * seed }) }
  return (x, { seed in seed })
}

@inline(__always)
func either(_ lhs: Bool, _ rhs: Bool) -> Bool { lhs || rhs }

@differentiable(reverse, wrt: x)
public func mixedConstructors(_ x: Float, _ negate: Bool, _ squareAgain: Bool) -> Float {
  let square = delayedSquare(x)
  let value = maybeSquare(square, squareAgain)
  if either(squareAgain, negate) {
    if squareAgain { return value }
    return -value
  }
  return 0
}

// CHECK-LABEL: {{^}}// reverse-mode derivative of twoSelections
// CHECK-NOT:   } // end sil function
// CHECK:         // function_ref specialized pullback of twoSelections
// CHECK-NEXT:    %[[#PB:]] = function_ref
// CHECK-NEXT:    %[[#PULLBACK:]] = partial_apply {{.*}} %[[#PB]]{{[<(]}}
// CHECK-NEXT:    %[[#RESULT:]] = tuple (%[[#]], %[[#PULLBACK]])
// CHECK-NOT:   } // end sil function
// CHECK:         return %[[#RESULT]]
// CHECK-NEXT:  } // end sil function

// CHECK-LABEL: {{^}}// specialized pullback of twoSelections
// CHECK-NOT:     = partial_apply
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// reverse-mode derivative of squaredSelection
// CHECK-NOT:     = partial_apply
// CHECK:         // function_ref {{.*}}pullback of squaredSelection
// CHECK-NEXT:    %[[#SQUARE_FN:]] = function_ref @[[SQUARE_PB:[^ ]+]]
// CHECK-NEXT:    %[[#SQUARE_CLOSURE:]] = partial_apply {{.*}} %[[#SQUARE_FN]](
// CHECK-NEXT:    %[[#SQUARE_RESULT:]] = tuple (%[[#]], %[[#SQUARE_CLOSURE]])
// CHECK-NEXT:    return %[[#SQUARE_RESULT]]
// CHECK:       sil private @[[SQUARE_PB]] :
// CHECK-NOT:     = partial_apply
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function

// The constructors agree on the delayedSquare capture but hold different
// callbacks in the neighboring field.
// CHECK-LABEL: {{^}}// reverse-mode derivative of mixedConstructors
// CHECK:       bb0(%[[#X:]] : $Float,
// CHECK:         %[[#IDENTITY:]] = thin_to_thick_function
// CHECK:         %[[#CAP1:]] = tuple (%[[#X]])
// CHECK-NEXT:    %[[#PAY1:]] = tuple (%[[#CAP1]], %[[#IDENTITY]])
// CHECK:         // function_ref closure #1 in maybeSquareVJP
// CHECK:         %[[#QUADRATIC:]] = partial_apply
// CHECK:         %[[#CAP2:]] = tuple (%[[#X]])
// CHECK-NEXT:    %[[#PAY2:]] = tuple (%[[#CAP2]], %[[#QUADRATIC]])
// CHECK:         = enum [[TAPE:\$_AD__[^, ]+]], [[CASE:#[^ ]+]], %[[#PAY2]]
// CHECK:         = enum [[TAPE]], [[CASE]], %[[#PAY1]]
// CHECK-LABEL: {{^}}// specialized pullback of mixedConstructors
// CHECK:         apply {{.*}} : $@callee_guaranteed (Float) -> Float
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function
