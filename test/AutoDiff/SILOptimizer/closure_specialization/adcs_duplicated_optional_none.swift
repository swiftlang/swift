// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name NoneCopies %s -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.log
// RUN: %FileCheck %s < %t.log

// CHECK:      stage MidLevel,Function, pass
// CHECK:      // reverse-mode derivative of recoverPair(_:_:_:_:)
// CHECK:        %[[#NONE:]] = enum $Optional<@callee_guaranteed (Float) -> Float>, #Optional.none!enumelt
// CHECK-NEXT:   %[[#FIRST_PAYLOAD:]] = tuple (%[[#NONE]])
// CHECK-NEXT:   %[[#PRED:]] = enum ${{_AD__[^, ]+}}, {{.*}}%[[#FIRST_PAYLOAD]]
// CHECK:        %[[#PAYLOAD1:]] = tuple $(predecessor: {{.*}}, Optional<@callee_guaranteed (Float) -> Float>) (%[[#PRED]], %[[#NONE]])
// CHECK-NEXT:   %[[#]] = enum $[[ERROR_TYPE:_AD__[^, ]+]], #[[ERROR_TYPE]].[[CASE:bb[0-9]+]]!enumelt, %[[#PAYLOAD1]]
// CHECK:        %[[#PAYLOAD2:]] = tuple $(predecessor: {{.*}}, Optional<@callee_guaranteed (Float) -> Float>) (%[[#PRED]], %[[#NONE]])
// CHECK-NEXT:   %[[#]] = enum $[[ERROR_TYPE]], #[[ERROR_TYPE]].[[CASE]]!enumelt, %[[#PAYLOAD2]]

// CHECK:        // function_ref closure #1 in squarePullback(_:)
// CHECK-NEXT:   %[[#FN:]] = function_ref
// CHECK-NEXT:   %[[#CALLBACK:]] = partial_apply [callee_guaranteed] %[[#FN]]
// CHECK-NEXT:   %[[#SOME:]] = enum $Optional<@callee_guaranteed (Float) -> Float>, #Optional.some!enumelt, %[[#CALLBACK]]
// CHECK-NEXT:   %[[#NORMAL_PAYLOAD:]] = tuple $(predecessor: {{.*}}, Optional<@callee_guaranteed (Float) -> Float>) (%[[#PRED]], %[[#SOME]])
// CHECK-NEXT:   %[[#]] = enum $[[NORMAL_TYPE:_AD__[^, ]+]], #[[NORMAL_TYPE]].[[CASE]]!enumelt, %[[#NORMAL_PAYLOAD]]
// CHECK:        return

// CHECK:      // reverse-mode derivative of recoverPair(_:_:_:_:)
// CHECK:        %[[#NEW_PRED:]] = enum ${{_AD__[^, ]+_spec[^, ]*}},
// CHECK:        %[[#NONE1:]] = enum $Optional<(_: Float)>, #Optional.none!enumelt
// CHECK-NEXT:   %[[#PAY1:]] = tuple {{.*}} (%[[#NEW_PRED]], %[[#NONE1]])
// CHECK-NEXT:   %[[#]] = enum $[[TAPE:_AD__[^, ]+_spec[^, ]*]], #[[TAPE]].[[CASE]]!enumelt, %[[#PAY1]]
// CHECK:        %[[#NONE2:]] = enum $Optional<(_: Float)>, #Optional.none!enumelt
// CHECK-NEXT:   %[[#PAY2:]] = tuple {{.*}} (%[[#NEW_PRED]], %[[#NONE2]])
// CHECK-NEXT:   %[[#]] = enum $[[TAPE]], #[[TAPE]].[[CASE]]!enumelt, %[[#PAY2]]
// CHECK:        // function_ref specialized pullback of recoverPair

import _Differentiation

public enum Failure: Error { case first, second }

@differentiable(reverse, wrt: x)
public func square(_ x: Float, _ flag: Bool) throws -> Float {
  if x < 0 { throw Failure.first }
  if flag { throw Failure.second }
  return x * x
}

@derivative(of: square, wrt: x)
@inline(__always)
public func squareVJP(_ x: Float, _ flag: Bool) throws
  -> (value: Float, pullback: (Float) -> Float) {
  if x < 0 { throw Failure.first }
  if flag { throw Failure.second }
  return (x * x, squarePullback(x))
}

// Expose the callback after jump threading has duplicated the error edge.
@_semantics("inline_late")
func squarePullback(_ x: Float) -> (Float) -> Float { { 2 * x * $0 } }

// CSE and jump threading leave two error constructors with the same case and
// operands, including the first error's dominating Optional.none.
@differentiable(reverse, wrt: (x, y))
@inline(never)
public func recover(_ x: Float, _ y: Float, _ a: Bool, _ b: Bool) -> Float {
  do { return try square(y, a) }
  catch {
    do { return try square(x, b) }
    catch { return x < 0 ? y : x }
  }
}

// Taking the constant component gives a pullback that only destroys its tape.
@differentiable(reverse, wrt: (x, y))
@inline(never)
public func recoverPair(_ x: Float, _ y: Float, _ a: Bool, _ b: Bool) -> (Float, Float) {
  do { return (try square(y, a), 1) }
  catch {
    do { return (try square(x, b), 2) }
    catch { return (x < 0 ? y : x, 3) }
  }
}

@differentiable(reverse, wrt: (x, y))
public func recoveryTag(_ x: Float, _ y: Float, _ a: Bool, _ b: Bool) -> Float {
  recoverPair(x, y, a, b).1
}
