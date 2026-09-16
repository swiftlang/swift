//===--- NumericDifferentiation.swift--- ----------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

extension Numeric where Self: Differentiable, Self == Self.TangentVector {
  @inlinable
  @_transparent
  @derivative(of: *)
  static func _vjpMultiply(lhs: Self, rhs: Self) ->
    (value: Self, pullback: (TangentVector) -> (TangentVector, TangentVector)) {
    return (lhs * rhs, { v in (rhs * v, lhs * v) })
  }

  @inlinable
  @_transparent
  @derivative(of: *)
  static func _jvpMultiply(lhs: Self, rhs: Self) ->
    (value: Self, differential: (TangentVector, TangentVector) -> TangentVector) {
    return (lhs * rhs, { (dlhs, drhs) in lhs * drhs + rhs * dlhs })
  }

  @inlinable
  @_transparent
  @derivative(of: *=)
  static func _vjpMultiplyAssign(_ lhs: inout Self, _ rhs: Self) ->
    (value: Void, pullback: (inout TangentVector) -> TangentVector) {
    defer { lhs *= rhs }
    return ((), { [lhs = lhs] v in
      let drhs = lhs * v
      v *= rhs
      return drhs
    })
  }

  @inlinable
  @_transparent
  @derivative(of: *=)
  static func _jvpMultiplyAssign(_ lhs: inout Self, _ rhs: Self) ->
    (value: Void, differential: (inout TangentVector, TangentVector) -> Void) {
    let oldLhs = lhs
    lhs *= rhs
    return ((), { $0 = $0 * rhs + oldLhs * $1 })
  }

}

extension SignedNumeric where Self: Differentiable, Self.TangentVector : SignedNumeric {
  @inlinable
  @_transparent
  @derivative(of: -)
  static func _vjpNegate(x: Self)
    -> (value: Self, pullback: (TangentVector) -> TangentVector) {
    return (-x, { v in -v })
  }

  @inlinable
  @_transparent
  @derivative(of: -)
  static func _jvpNegate(x: Self)
  -> (value: Self, differential: (TangentVector) -> TangentVector) {
    return (-x, { dx in -dx })
  }
}
