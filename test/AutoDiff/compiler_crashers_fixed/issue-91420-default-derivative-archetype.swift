// RUN: %target-swift-frontend -emit-sil -module-name test %s

// https://github.com/swiftlang/swift/issues/91420
// Ensure we map archetype from protocol environment to the derivative one

import _Differentiation

public struct S<Element> {
  public var element: Element
}

extension S: Differentiable where Element: Differentiable {
  public typealias TangentVector = S<Element.TangentVector>
  public mutating func move(by offset: TangentVector) {
    self.element.move(by: offset.element)
  }
}

extension S: AdditiveArithmetic where Element: AdditiveArithmetic {
  public static func + (lhs: S<Element>, rhs: S<Element>) -> S<Element> { lhs }
}

extension S where Element: Differentiable & AdditiveArithmetic {
  @derivative(of: +)
  public static func _vjpAdd(lhs: S<Element>, rhs: S<Element>) ->
  (
    value: S<Element>,
    pullback: (S<Element.TangentVector>) -> (S<Element.TangentVector>, S<Element.TangentVector>)
  ) {
    (value: lhs, pullback: { v in (v, .zero) } )
  }
}

extension S: Equatable where Element: Equatable {
  public static func == (lhs: S<Element>, rhs: S<Element>) -> Bool {
    return lhs.element == rhs.element
  }
}

@differentiable(reverse)
func foo<T : Differentiable & AdditiveArithmetic>(_ a: S<T>, _ b : S<T>) -> S<T> {
    return a + b
}
