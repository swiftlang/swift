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
  public enum TangentVector: Differentiable, AdditiveArithmetic {
    case none
    case some(Wrapped.TangentVector)

    public typealias TangentVector = Self

    public init(_ value: Wrapped.TangentVector?) {
      switch value {
      case .some(let y):
	self = .some(y)
      case .none:
	self = .none
      }
    }

    public static var zero: Self {
      return Self(.zero)
    }

    public static func + (lhs: Self, rhs: Self) -> Self {
      switch (lhs, rhs) {
      case let (.some(x), .some(y)): return Self(x + y)
      case let (.some(x), .none): return Self(x)
      case let (.none, .some(y)): return Self(y)
      case (.none, .none): return .none
      }
    }

    public static func - (lhs: Self, rhs: Self) -> Self {
      switch (lhs, rhs) {
      case let (.some(x), .some(y)): return .some(x - y)
      case let (.some(x), .none): return .some(x)
      case let (.none, .some(y)): return .some(.zero - y)
      case (.none, .none): return .none
      }
    }

    public mutating func move(by offset: TangentVector) {
      switch (self, offset) {
      case let (.some(s), .some(o)): {
	  var res = s
	  res.move(by: o)
	  self = .some(res)
	}()
      default: ()
      }
    }
  }

  public mutating func move(by offset: TangentVector) {
    switch (offset) {
    case let .some(o): self?.move(by: o)
    default: ()
    }
  }
}

#if SWIFT_ENABLE_REFLECTION
extension Optional.TangentVector: CustomReflectable {
  public var customMirror: Mirror {
    switch self {
    case .some(let value):
      return Mirror(
        self,
        children: [ "some": value ])
    case .none:
      return Mirror(self, children: [:])
    }
  }
}
#endif
