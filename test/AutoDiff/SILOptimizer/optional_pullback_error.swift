// RUN: %target-swift-emit-sil -verify %s

import _Differentiation

public enum DiffEnum<Wrapped>: ExpressibleByNilLiteral {
  case none
  case some(Wrapped)

  @_transparent
  public init(_ some: Wrapped) { self = .some(some) }

  @_transparent
  public init(nilLiteral: ()) {
    self = .none
  }
}

extension DiffEnum: Differentiable where Wrapped: Differentiable {
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
    }
  }

  public mutating func move(by offset: TangentVector) {
  }

  // expected-error @+2 {{expression is not differentiable}}
  // expected-note @+1 {{differentiating enum values is not yet supported}}
  public func f() -> Wrapped {
    switch (self) {
    case let .some(v): return v
    default: fatalError()
    }
  }
}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+2 {{when differentiating this function definition}}
@differentiable(reverse)
func enumNotSupported<Element>(x: Element) -> Element where Element: Differentiable {
    // expected-note @+1 {{differentiating enum values is not yet supported}}
    let e = DiffEnum<Element>.some(x)
    return e.f()
}

@differentiable(reverse)
func enumOptional<Element>(x: Element) -> Element? where Element: Differentiable {
    return x
}
