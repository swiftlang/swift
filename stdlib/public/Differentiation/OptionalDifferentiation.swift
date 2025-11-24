//===--- OptionalDifferentiation.swift ------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

extension Optional: Differentiable where Wrapped: Differentiable {
  @frozen
  public struct TangentVector: Differentiable, AdditiveArithmetic {
    public typealias TangentVector = Self

    public var value: Wrapped.TangentVector?

    @inlinable
    public init(_ value: Wrapped.TangentVector?) {
      self.value = value
    }

    @inlinable
    public static var zero: Self {
      return Self(.zero)
    }

    @inlinable
    public static func + (lhs: Self, rhs: Self) -> Self {
      switch (lhs.value, rhs.value) {
      case (nil, nil): return Self(nil)
      case let (x?, nil): return Self(x)
      case let (nil, y?): return Self(y)
      case let (x?, y?): return Self(x + y)
      }
    }

    @inlinable
    public static func - (lhs: Self, rhs: Self) -> Self {
      switch (lhs.value, rhs.value) {
      case (nil, nil): return Self(nil)
      case let (x?, nil): return Self(x)
      case let (nil, y?): return Self(.zero - y)
      case let (x?, y?): return Self(x - y)
      }
    }

    @inlinable
    public mutating func move(by offset: TangentVector) {
      if let value = offset.value {
        self.value?.move(by: value)
      }
    }
  }

  @inlinable
  public mutating func move(by offset: TangentVector) {
    if let value = offset.value {
      self?.move(by: value)
    }
  }
}

extension Optional.TangentVector: CustomReflectable {
  public var customMirror: Mirror {
    return value.customMirror
  }
}

@derivative(of: ??)
@_transparent
@_alwaysEmitIntoClient
func _vjpNilCoalescing<T: Differentiable>(optional: T?, defaultValue: @autoclosure () throws -> T)
 rethrows -> (value: T, pullback: (T.TangentVector) -> Optional<T>.TangentVector) {
  let hasValue = optional != nil
  let value = try optional ?? defaultValue()
  func pullback(_ v: T.TangentVector) -> Optional<T>.TangentVector {
    return hasValue ? .init(v) : .zero
  }
  return (value, pullback)
}
