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
  public struct TangentVector: Differentiable, AdditiveArithmetic {
    public typealias TangentVector = Self

    public var value: Wrapped.TangentVector?

    public init(_ value: Wrapped.TangentVector?) {
      self.value = value
    }

    public static var zero: Self {
      return Self(.zero)
    }

    public static func + (lhs: Self, rhs: Self) -> Self {
      switch (lhs.value, rhs.value) {
      case (nil, nil): return Self(nil)
      case let (x?, nil): return Self(x)
      case let (nil, y?): return Self(y)
      case let (x?, y?): return Self(x + y)
      }
    }

    public static func - (lhs: Self, rhs: Self) -> Self {
      switch (lhs.value, rhs.value) {
      case (nil, nil): return Self(nil)
      case let (x?, nil): return Self(x)
      case let (nil, y?): return Self(.zero - y)
      case let (x?, y?): return Self(x - y)
      }
    }

    public mutating func move(along direction: TangentVector) {
      if let value = direction.value {
        self.value?.move(along: value)
      }
    }

    @noDerivative
    public var zeroTangentVectorInitializer: () -> TangentVector {
      switch value {
      case nil:
        return { Self(nil) }
      case let x?:
        return { [zeroTanInit = x.zeroTangentVectorInitializer] in
          Self(zeroTanInit())
        }
      }
    }
  }

  public mutating func move(along direction: TangentVector) {
    if let value = direction.value {
      self?.move(along: value)
    }
  }

  @noDerivative
  public var zeroTangentVectorInitializer: () -> TangentVector {
    switch self {
    case nil:
      return { TangentVector(nil) }
    case let x?:
      return { [zeroTanInit = x.zeroTangentVectorInitializer] in
        TangentVector(zeroTanInit())
      }
    }
  }
}
