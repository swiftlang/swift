// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-Onone -Xfrontend -sil-verify-all)
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all)
// RUN: %target-run-simple-swift(-Osize -Xfrontend -sil-verify-all)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name adcs_duplicated_optional_none %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

// CHECK-LABEL: enum _AD__{{[^ ]*recoverPair[^ ]*_spec_bb3_1_[^ ]*}} {
// CHECK-NEXT:    case bb3((predecessor: {{[^,]+}}, (_: Float)?))
// CHECK-LABEL: enum _AD__{{[^ ]*recoverPair[^ ]*_spec_bb3_1_[^ ]*}} {
// CHECK-NEXT:    case bb3((predecessor: {{[^,]+}}, (_: Float)?))
// CHECK-LABEL: // reverse-mode derivative of recoverPair(_:_:_:_:)
// CHECK:         // function_ref specialized pullback of recoverPair(_:_:_:_:)
// CHECK-NEXT:    %[[#PULLBACK:]] = function_ref {{.*}} : $@convention(thin) (Float, Float) -> (Float, Float)
// CHECK-NEXT:    %[[#CLOSURE:]] = thin_to_thick_function %[[#PULLBACK]]
// CHECK-NEXT:    %[[#RESULT:]] = tuple (%[[#]], %[[#]], %[[#CLOSURE]])
// CHECK-NEXT:    return %[[#RESULT]]

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

// Delay exposing the callback until jump threading duplicates the error edge.
@_semantics("inline_late")
func squarePullback(_ x: Float) -> (Float) -> Float { { 2 * x * $0 } }

// Jump threading duplicates the second error path. CSE reuses the first
// path's dominating Optional.none, so the duplicate constructors save the
// same SSA values.
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

for a in [false, true] {
  for b in [false, true] {
    for x: Float in [-2, 0, 2] {
      for y: Float in [-2, 0, 2] {
        let result = valueWithPullback(at: x, y, of: { recover($0, $1, a, b) })
        let expectedValue: Float
        let dx: Float
        let dy: Float
        if y >= 0 && !a {
          expectedValue = y * y
          dx = 0
          dy = 2 * y
        } else if x >= 0 && !b {
          expectedValue = x * x
          dx = 2 * x
          dy = 0
        } else if x < 0 {
          expectedValue = y
          dx = 0
          dy = 1
        } else {
          expectedValue = x
          dx = 1
          dy = 0
        }
        precondition(result.value == expectedValue)
        let tag = valueWithPullback(at: x, y, of: { recoveryTag($0, $1, a, b) })
        precondition(tag.value == (y >= 0 && !a ? 1 : x >= 0 && !b ? 2 : 3))
        precondition(tag.pullback(1) == (0, 0))
        for seed: Float in [0, 1, -2, 1] {
          let gradient = result.pullback(seed)
          precondition(gradient.0 == dx * seed && gradient.1 == dy * seed)
        }
      }
    }
  }
}
