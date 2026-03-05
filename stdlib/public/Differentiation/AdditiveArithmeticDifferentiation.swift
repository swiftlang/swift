//===--- AdditiveArithmeticDifferentiation.swift--- ----------*- swift -*-===//
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

extension AdditiveArithmetic where Self: Differentiable {
  @derivative(of: +)
  @inlinable
  @_transparent
  static func _vjpPlus(_ x: Self) ->
    (value: Self, pullback: (TangentVector) -> (TangentVector)) {
    return (x, { v in v })
  }

  @inlinable
  @_transparent
  @derivative(of: +)
  static func _jvpPlus(x: Self) ->
    (value: Self, differential: (TangentVector) -> TangentVector) {
    return (x, { dx in dx })
  }

  @derivative(of: +)
  @inlinable
  @_transparent
  static func _vjpAdd(_ lhs: Self, _ rhs: Self) ->
    (value: Self, pullback: (TangentVector) -> (TangentVector, TangentVector)) {
    return (lhs + rhs, { v in (v, v) })
  }

  @inlinable
  @_transparent
  @derivative(of: +)
  static func _jvpAdd(lhs: Self, rhs: Self) ->
    (value: Self, differential: (TangentVector, TangentVector) -> TangentVector) {
    return (lhs + rhs, { (dlhs, drhs) in dlhs + drhs })
  }

  /*
  @inlinable
  @_alwaysEmitIntoClient  
  @derivative(of: +=)
  static func _vjpAddAssign(_ lhs: inout Self, _ rhs: Self) ->
    (value: Void, pullback: (inout TangentVector) -> TangentVector) {
    lhs += rhs
    return ((), { v in v })
  }

  @inlinable
  @_alwaysEmitIntoClient  
  @derivative(of: +=)
  static func _jvpAddAssign(_ lhs: inout Self, _ rhs: Self) ->
    (value: Void, differential: (inout TangentVector, TangentVector) -> Void) {
    lhs += rhs
    return ((), { $0 += $1 })
  }*/

  
  @inlinable
  @_transparent
  @derivative(of: -)
  static func _vjpSubtract(lhs: Self, rhs: Self) ->
    (value: Self, pullback: (TangentVector) -> (TangentVector, TangentVector)) {
    return (lhs - rhs, { v in (v, TangentVector.zero-v) })
  }

  @inlinable
  @_transparent
  @derivative(of: -)
  static func _jvpSubtract(lhs: Self, rhs: Self) ->
    (value: Self, differential: (TangentVector, TangentVector) -> TangentVector) {
    return (lhs - rhs, { (dlhs, drhs) in dlhs - drhs })
  }

  /*
  @inlinable
  @_alwaysEmitIntoClient
  @derivative(of: -=)
  static func _vjpSubtractAssign(_ lhs: inout Self, _ rhs: Self) ->
    (value: Void, pullback: (inout TangentVector) -> TangentVector) {
    lhs -= rhs
    return ((), { v in TangentVector.zero-v })
  }

  @inlinable
  @_alwaysEmitIntoClient
  @derivative(of: -=)
  static func _jvpSubtractAssign(_ lhs: inout Self, _ rhs: Self) ->
    (value: Void, differential: (inout TangentVector, TangentVector) -> Void) {
    lhs -= rhs
    return ((), { $0 -= $1 })
  }*/
}
