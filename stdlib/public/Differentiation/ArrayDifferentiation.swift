//===--- ArrayDifferentiation.swift ---------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

//===----------------------------------------------------------------------===//
// Protocol conformances
//===----------------------------------------------------------------------===//

// TODO(TF-938): Add `Element: Differentiable` requirement.
extension Array {
  /// The view of an array as the differentiable product manifold of `Element`
  /// multiplied with itself `count` times.
  @frozen
  public struct DifferentiableView {
    var _base: [Element]
  }
}

extension Array.DifferentiableView: Differentiable
where Element: Differentiable {
  /// The viewed array.
  public var base: [Element] {
    get { return _base }
    _modify { yield &_base }
  }

  @usableFromInline
  @derivative(of: base)
  func _vjpBase() -> (
    value: [Element], pullback: (Array<Element>.TangentVector) -> TangentVector
  ) {
    return (base, { $0 })
  }

  /// Creates a differentiable view of the given array.
  public init(_ base: [Element]) { self._base = base }

  @usableFromInline
  @derivative(of: init(_:))
  static func _vjpInit(_ base: [Element]) -> (
    value: Array.DifferentiableView, pullback: (TangentVector) -> TangentVector
  ) {
    return (Array.DifferentiableView(base), { $0 })
  }

  public typealias TangentVector =
    Array<Element.TangentVector>.DifferentiableView

  public mutating func move(along direction: TangentVector) {
    precondition(
      base.count == direction.base.count,
      "cannot move Array.DifferentiableView with count \(base.count) along "
        + "direction with different count \(direction.base.count)")
    for i in base.indices {
      base[i].move(along: direction.base[i])
    }
  }
}

extension Array.DifferentiableView: Equatable
where Element: Differentiable & Equatable {
  public static func == (
    lhs: Array.DifferentiableView,
    rhs: Array.DifferentiableView
  ) -> Bool {
    return lhs.base == rhs.base
  }
}

extension Array.DifferentiableView: ExpressibleByArrayLiteral
where Element: Differentiable {
  public init(arrayLiteral elements: Element...) {
    self.init(elements)
  }
}

extension Array.DifferentiableView: CustomStringConvertible
where Element: Differentiable {
  public var description: String {
    return base.description
  }
}

/// Makes `Array.DifferentiableView` additive as the product space.
///
/// Note that `Array.DifferentiableView([])` is the zero in the product spaces
/// of all counts.
extension Array.DifferentiableView: AdditiveArithmetic
where Element: AdditiveArithmetic & Differentiable {

  public static var zero: Array.DifferentiableView {
    return Array.DifferentiableView([])
  }

  public static func + (
    lhs: Array.DifferentiableView,
    rhs: Array.DifferentiableView
  ) -> Array.DifferentiableView {
    precondition(
      lhs.base.count == 0 || rhs.base.count == 0
        || lhs.base.count == rhs.base.count,
      "cannot add Array.DifferentiableViews with different counts: "
        + "\(lhs.base.count) and \(rhs.base.count)")
    if lhs.base.count == 0 {
      return rhs
    }
    if rhs.base.count == 0 {
      return lhs
    }
    return Array.DifferentiableView(zip(lhs.base, rhs.base).map(+))
  }

  public static func - (
    lhs: Array.DifferentiableView,
    rhs: Array.DifferentiableView
  ) -> Array.DifferentiableView {
    precondition(
      lhs.base.count == 0 || rhs.base.count == 0
        || lhs.base.count == rhs.base.count,
      "cannot subtract Array.DifferentiableViews with different counts: "
        + "\(lhs.base.count) and \(rhs.base.count)")
    if lhs.base.count == 0 {
      return rhs
    }
    if rhs.base.count == 0 {
      return lhs
    }
    return Array.DifferentiableView(zip(lhs.base, rhs.base).map(-))
  }

  @inlinable
  public subscript(_ index: Int) -> Element {
    if index < base.count {
      return base[index]
    } else {
      return Element.zero
    }
  }
}

/// Makes `Array` differentiable as the product manifold of `Element`
/// multiplied with itself `count` times.
extension Array: Differentiable where Element: Differentiable {
  // In an ideal world, `TangentVector` would be `[Element.TangentVector]`.
  // Unfortunately, we cannot conform `Array` to `AdditiveArithmetic` for
  // `TangentVector` because `Array` already has a static `+` method with
  // different semantics from `AdditiveArithmetic.+`. So we use
  // `Array.DifferentiableView` for all these associated types.
  public typealias TangentVector =
    Array<Element.TangentVector>.DifferentiableView

  public mutating func move(along direction: TangentVector) {
    var view = DifferentiableView(self)
    view.move(along: direction)
    self = view.base
  }

  /// A closure that produces a `TangentVector` of zeros with the same
  /// `count` as `self`.
  public var zeroTangentVectorInitializer: () -> TangentVector {
    { [count = self.count] in
      TangentVector(.init(repeating: .zero, count: count))
    }
  }
}

//===----------------------------------------------------------------------===//
// Derivatives
//===----------------------------------------------------------------------===//

extension Array where Element: Differentiable {
  @usableFromInline
  @derivative(of: subscript)
  func _vjpSubscript(index: Int) -> (
    value: Element, pullback: (Element.TangentVector) -> TangentVector
  ) {
    func pullback(_ gradientIn: Element.TangentVector) -> TangentVector {
      var gradientOut = [Element.TangentVector](
        repeating: .zero,
        count: count)
      gradientOut[index] = gradientIn
      return TangentVector(gradientOut)
    }
    return (self[index], pullback)
  }

  @usableFromInline
  @derivative(of: +)
  static func _vjpConcatenate(_ lhs: [Element], _ rhs: [Element]) -> (
    value: [Element],
    pullback: (TangentVector) -> (TangentVector, TangentVector)
  ) {
    func pullback(_ gradientIn: TangentVector) -> (TangentVector, TangentVector)
    {
      precondition(
        gradientIn.base.count == lhs.count + rhs.count,
        "+ should receive gradient with count equal to sum of operand "
          + "counts, but counts are: gradient \(gradientIn.base.count), "
          + "lhs \(lhs.count), rhs \(rhs.count)")
      return (
        TangentVector(
          [Element.TangentVector](
            gradientIn.base[0..<lhs.count])),
        TangentVector(
          [Element.TangentVector](
            gradientIn.base[lhs.count...]))
      )
    }
    return (lhs + rhs, pullback)
  }
}

extension Array where Element: Differentiable {
  @usableFromInline
  @derivative(of: append)
  mutating func _vjpAppend(_ element: Element) -> (
    value: Void, pullback: (inout TangentVector) -> Element.TangentVector
  ) {
    let appendedElementIndex = count
    defer { append(element) }
    return ((), { dself in dself.base[appendedElementIndex] })
  }

  @usableFromInline
  @derivative(of: append)
  mutating func _jvpAppend(_ element: Element) -> (
    value: Void,
    differential: (inout TangentVector, Element.TangentVector) -> Void
  ) {
    append(element)
    return ((), { $0.base.append($1) })
  }
}

extension Array where Element: Differentiable {
  @usableFromInline
  @derivative(of: init(repeating:count:))
  static func _vjpInit(repeating repeatedValue: Element, count: Int) -> (
    value: Self, pullback: (TangentVector) -> Element.TangentVector
  ) {
    (
      value: Self(repeating: repeatedValue, count: count),
      pullback: { v in
        v.base.reduce(.zero, +)
      }
    )
  }
}

//===----------------------------------------------------------------------===//
// Differentiable higher order functions for collections
//===----------------------------------------------------------------------===//

extension Array where Element: Differentiable {
  @differentiable(wrt: (self, initialResult))
  public func differentiableReduce<Result: Differentiable>(
    _ initialResult: Result,
    _ nextPartialResult: @differentiable (Result, Element) -> Result
  ) -> Result {
    reduce(initialResult, nextPartialResult)
  }

  @usableFromInline
  @derivative(of: differentiableReduce)
  internal func _vjpDifferentiableReduce<Result: Differentiable>(
    _ initialResult: Result,
    _ nextPartialResult: @differentiable (Result, Element) -> Result
  ) -> (
    value: Result,
    pullback: (Result.TangentVector)
      -> (Array.TangentVector, Result.TangentVector)
  ) {
    var pullbacks:
      [(Result.TangentVector) -> (Result.TangentVector, Element.TangentVector)] =
        []
    let count = self.count
    pullbacks.reserveCapacity(count)
    var result = initialResult
    for element in self {
      let (y, pb) =
        valueWithPullback(at: result, element, in: nextPartialResult)
      result = y
      pullbacks.append(pb)
    }
    return (
      value: result,
      pullback: { tangent in
        var resultTangent = tangent
        var elementTangents = TangentVector([])
        elementTangents.base.reserveCapacity(count)
        for pullback in pullbacks.reversed() {
          let (newResultTangent, elementTangent) = pullback(resultTangent)
          resultTangent = newResultTangent
          elementTangents.base.append(elementTangent)
        }
        return (TangentVector(elementTangents.base.reversed()), resultTangent)
      }
    )
  }
}

extension Array where Element: Differentiable {
  @differentiable(wrt: self)
  public func differentiableMap<Result: Differentiable>(
    _ body: @differentiable (Element) -> Result
  ) -> [Result] {
    map(body)
  }

  @usableFromInline
  @derivative(of: differentiableMap)
  internal func _vjpDifferentiableMap<Result: Differentiable>(
    _ body: @differentiable (Element) -> Result
  ) -> (
    value: [Result],
    pullback: (Array<Result>.TangentVector) -> Array.TangentVector
  ) {
    var values: [Result] = []
    var pullbacks: [(Result.TangentVector) -> Element.TangentVector] = []
    for x in self {
      let (y, pb) = valueWithPullback(at: x, in: body)
      values.append(y)
      pullbacks.append(pb)
    }
    func pullback(_ tans: Array<Result>.TangentVector) -> Array.TangentVector {
      .init(zip(tans.base, pullbacks).map { tan, pb in pb(tan) })
    }
    return (value: values, pullback: pullback)
  }
}
