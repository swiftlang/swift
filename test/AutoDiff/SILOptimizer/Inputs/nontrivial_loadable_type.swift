import _Differentiation

/// A non-trivial, loadable type.
///
/// Used to test differentiation transform coverage.
struct NontrivialLoadable<T> {
  fileprivate class Box {
    fileprivate var value: T
    init(_ value: T) {
      self.value = value
    }
  }
  private var handle: Box

  init(_ value: T) {
    self.handle = Box(value)
  }

  var value: T {
    get { handle.value }
    set { handle.value = newValue }
  }
}

extension NontrivialLoadable: ExpressibleByFloatLiteral
where T: ExpressibleByFloatLiteral {
  init(floatLiteral value: T.FloatLiteralType) {
    self.handle = Box(T(floatLiteral: value))
  }
}

extension NontrivialLoadable: ExpressibleByIntegerLiteral
where T: ExpressibleByIntegerLiteral {
  init(integerLiteral value: T.IntegerLiteralType) {
    self.handle = Box(T(integerLiteral: value))
  }
}

extension NontrivialLoadable: Equatable where T: Equatable {
  static func == (lhs: NontrivialLoadable, rhs: NontrivialLoadable) -> Bool {
    return lhs.value == rhs.value
  }
}

extension NontrivialLoadable: AdditiveArithmetic where T: AdditiveArithmetic {
  static var zero: NontrivialLoadable { return NontrivialLoadable(T.zero) }
  static func + (lhs: NontrivialLoadable, rhs: NontrivialLoadable)
    -> NontrivialLoadable
  {
    return NontrivialLoadable(lhs.value + rhs.value)
  }
  static func - (lhs: NontrivialLoadable, rhs: NontrivialLoadable)
    -> NontrivialLoadable
  {
    return NontrivialLoadable(lhs.value - rhs.value)
  }
}

extension NontrivialLoadable: Differentiable
where T: Differentiable, T == T.TangentVector {
  typealias TangentVector = NontrivialLoadable<T.TangentVector>
}
