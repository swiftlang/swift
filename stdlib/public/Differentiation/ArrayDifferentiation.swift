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

extension Array where Element: Differentiable {
  /// The view of an array as the differentiable product manifold of `Element`
  /// multiplied with itself `count` times.
  @frozen
  public struct DifferentiableView {
    @usableFromInline
    var _base: [Element]
  }
}

extension Array.DifferentiableView: Differentiable
where Element: Differentiable {
  /// The viewed array.
  @inlinable
  public var base: [Element] {
    get { _base }
    _modify { yield &_base }
  }

  @inlinable
  @derivative(of: base)
  func _vjpBase() -> (
    value: [Element], pullback: (Array<Element>.TangentVector) -> TangentVector
  ) {
    return (base, { $0 })
  }

  @inlinable
  @derivative(of: base)
  func _jvpBase() -> (
    value: [Element], differential: (Array<Element>.TangentVector) -> TangentVector
  ) {
    return (base, { $0 })
  }

  /// Creates a differentiable view of the given array.
  @inlinable
  public init(_ base: [Element]) { self._base = base }

  @inlinable
  @derivative(of: init(_:))
  static func _vjpInit(_ base: [Element]) -> (
    value: Array.DifferentiableView, pullback: (TangentVector) -> TangentVector
  ) {
    return (Array.DifferentiableView(base), { $0 })
  }

  @inlinable
  @derivative(of: init(_:))
  static func _jvpInit(_ base: [Element]) -> (
    value: Array.DifferentiableView, differential: (TangentVector) -> TangentVector
  ) {
    return (Array.DifferentiableView(base), { $0 })
  }

  public typealias TangentVector =
    Array<Element.TangentVector>.DifferentiableView

  @inlinable
  public mutating func move(by offset: TangentVector) {
    if offset.base.isEmpty {
      return
    }
    precondition(
      base.count == offset.base.count, """
        Count mismatch: \(base.count) ('self') and \(offset.base.count) \
        ('direction')
        """)
    for i in offset.base.indices {
      base[i].move(by: offset.base[i])
    }
  }
}

extension Array.DifferentiableView: Equatable
where Element: Differentiable & Equatable {
  @inlinable
  public static func == (
    lhs: Array.DifferentiableView,
    rhs: Array.DifferentiableView
  ) -> Bool {
    return lhs.base == rhs.base
  }
}

extension Array.DifferentiableView: ExpressibleByArrayLiteral
where Element: Differentiable {
  @inlinable
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

extension Array.DifferentiableView: CustomReflectable {
  public var customMirror: Mirror {
    return base.customMirror
  }
}

/// Makes `Array.DifferentiableView` additive as the product space.
///
/// Note that `Array.DifferentiableView([])` is the zero in the product spaces
/// of all counts.
extension Array.DifferentiableView: AdditiveArithmetic
where Element: AdditiveArithmetic & Differentiable {

  @inlinable
  public static var zero: Array.DifferentiableView {
    return Array.DifferentiableView([])
  }
  
  @inlinable
  public static func + (
    lhs: Array.DifferentiableView,
    rhs: Array.DifferentiableView
  ) -> Array.DifferentiableView {
    if lhs.base.count == 0 {
      return rhs
    }
    if rhs.base.count == 0 {
      return lhs
    }
    precondition(
      lhs.base.count == rhs.base.count,
      "Count mismatch: \(lhs.base.count) and \(rhs.base.count)")
    return Array.DifferentiableView(zip(lhs.base, rhs.base).map(+))
  }

  @inlinable
  public static func - (
    lhs: Array.DifferentiableView,
    rhs: Array.DifferentiableView
  ) -> Array.DifferentiableView {
    if lhs.base.count == 0 {
      return rhs
    }
    if rhs.base.count == 0 {
      return lhs
    }
    precondition(
      lhs.base.count == rhs.base.count,
      "Count mismatch: \(lhs.base.count) and \(rhs.base.count)")
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

  @inlinable
  public mutating func move(by offset: TangentVector) {
    var view = DifferentiableView(self)
    view.move(by: offset)
    self = view.base
  }
}

//===----------------------------------------------------------------------===//
// Derivatives
//===----------------------------------------------------------------------===//

extension Array where Element: Differentiable {
  @inlinable
  @derivative(of: subscript)
  func _vjpSubscript(index: Int) -> (
    value: Element, pullback: (Element.TangentVector) -> TangentVector
  ) {
    func pullback(_ v: Element.TangentVector) -> TangentVector {
      var dSelf = [Element.TangentVector](
        repeating: .zero,
        count: count)
      dSelf[index] = v
      return TangentVector(dSelf)
    }
    return (self[index], pullback)
  }

  @inlinable
  @derivative(of: subscript)
  func _jvpSubscript(index: Int) -> (
    value: Element, differential: (TangentVector) -> Element.TangentVector
  ) {
    func differential(_ v: TangentVector) -> Element.TangentVector {
      return v[index]
    }
    return (self[index], differential)
  }

  @inlinable
  @derivative(of: +)
  static func _vjpConcatenate(_ lhs: Self, _ rhs: Self) -> (
    value: Self,
    pullback: (TangentVector) -> (TangentVector, TangentVector)
  ) {
    func pullback(_ v: TangentVector) -> (TangentVector, TangentVector) {
      if v.base.isEmpty {
        return (.zero, .zero)
      }
      precondition(
        v.base.count == lhs.count + rhs.count, """
          Tangent vector with invalid count \(v.base.count); expected to \
          equal the sum of operand counts \(lhs.count) and \(rhs.count)
          """)
      return (
        TangentVector([Element.TangentVector](v.base[0..<lhs.count])),
        TangentVector([Element.TangentVector](v.base[lhs.count...]))
      )
    }
    return (lhs + rhs, pullback)
  }

  @inlinable
  @derivative(of: +)
  static func _jvpConcatenate(_ lhs: Self, _ rhs: Self) -> (
    value: Self,
    differential: (TangentVector, TangentVector) -> TangentVector
  ) {
    func differential(_ l: TangentVector, _ r: TangentVector) -> TangentVector {
      precondition(
        l.base.count == lhs.count && r.base.count == rhs.count, """
          Tangent vectors with invalid count; expected to equal the \
          operand counts \(lhs.count) and \(rhs.count)
          """)
      return .init(l.base + r.base)
    }
    return (lhs + rhs, differential)
  }
}


extension Array where Element: Differentiable {
  @inlinable
  @derivative(of: append)
  mutating func _vjpAppend(_ element: Element) -> (
    value: Void, pullback: (inout TangentVector) -> Element.TangentVector
  ) {
    let appendedElementIndex = count
    append(element)
    return ((), { v in
      defer { v.base.removeLast() }
      return v.base[appendedElementIndex]
    })
  }

  @inlinable
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
  @inlinable
  @derivative(of: +=)
  static func _vjpAppend(_ lhs: inout Self, _ rhs: Self) -> (
    value: Void, pullback: (inout TangentVector) -> TangentVector
  ) {
    let lhsCount = lhs.count
    lhs += rhs
    return ((), { v in
      let drhs =
        TangentVector(.init(v.base.dropFirst(lhsCount)))
      let rhsCount = drhs.base.count
      v.base.removeLast(rhsCount)
      return drhs
    })
  }

  @inlinable
  @derivative(of: +=)
  static func _jvpAppend(_ lhs: inout Self, _ rhs: Self) -> (
    value: Void, differential: (inout TangentVector, TangentVector) -> Void
  ) {
    lhs += rhs
    return ((), { $0.base += $1.base })
  }
}

extension Array where Element: Differentiable {
  @inlinable
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

  @inlinable
  @derivative(of: init(repeating:count:))
  static func _jvpInit(repeating repeatedValue: Element, count: Int) -> (
    value: Self, differential: (Element.TangentVector) -> TangentVector
  ) {
    (
      value: Self(repeating: repeatedValue, count: count),
      differential: { v in TangentVector(.init(repeating: v, count: count)) }
    )
  }
}

//===----------------------------------------------------------------------===//
// Differentiable higher order functions for collections
//===----------------------------------------------------------------------===//

extension Array where Element: Differentiable {
  @inlinable
  @differentiable(reverse, wrt: self)
  public func differentiableMap<Result: Differentiable>(
    _ body: @differentiable(reverse) (Element) -> Result
  ) -> [Result] {
    map(body)
  }

  @inlinable
  @derivative(of: differentiableMap)
  internal func _vjpDifferentiableMap<Result: Differentiable>(
    _ body: @differentiable(reverse) (Element) -> Result
  ) -> (
    value: [Result],
    pullback: (Array<Result>.TangentVector) -> Array.TangentVector
  ) {
    var values: [Result] = []
    var pullbacks: [(Result.TangentVector) -> Element.TangentVector] = []
    for x in self {
      let (y, pb) = valueWithPullback(at: x, of: body)
      values.append(y)
      pullbacks.append(pb)
    }
    func pullback(_ tans: Array<Result>.TangentVector) -> Array.TangentVector {
      .init(zip(tans.base, pullbacks).map { tan, pb in pb(tan) })
    }
    return (value: values, pullback: pullback)
  }

  @inlinable
  @derivative(of: differentiableMap)
  internal func _jvpDifferentiableMap<Result: Differentiable>(
    _ body: @differentiable(reverse) (Element) -> Result
  ) -> (
    value: [Result],
    differential: (Array.TangentVector) -> Array<Result>.TangentVector
  ) {
    var values: [Result] = []
    var differentials: [(Element.TangentVector) -> Result.TangentVector] = []
    for x in self {
      let (y, df) = valueWithDifferential(at: x, of: body)
      values.append(y)
      differentials.append(df)
    }
    func differential(_ tans: Array.TangentVector) -> Array<Result>.TangentVector {
      .init(zip(tans.base, differentials).map { tan, df in df(tan) })
    }
    return (value: values, differential: differential)
  }
}

extension Array where Element: Differentiable {
  @inlinable
  @differentiable(reverse, wrt: (self, initialResult))
  public func differentiableReduce<Result: Differentiable>(
    _ initialResult: Result,
    _ nextPartialResult: @differentiable(reverse) (Result, Element) -> Result
  ) -> Result {
    reduce(initialResult, nextPartialResult)
  }

  @inlinable
  @derivative(of: differentiableReduce)
  internal func _vjpDifferentiableReduce<Result: Differentiable>(
    _ initialResult: Result,
    _ nextPartialResult: @differentiable(reverse) (Result, Element) -> Result
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
        valueWithPullback(at: result, element, of: nextPartialResult)
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

  @inlinable
  @derivative(of: differentiableReduce, wrt: (self, initialResult))
  func _jvpDifferentiableReduce<Result: Differentiable>(
    _ initialResult: Result,
    _ nextPartialResult: @differentiable(reverse) (Result, Element) -> Result
  ) -> (value: Result,
        differential: (Array.TangentVector, Result.TangentVector)
          -> Result.TangentVector) {
    var differentials:
      [(Result.TangentVector, Element.TangentVector) -> Result.TangentVector]
        = []
    let count = self.count
    differentials.reserveCapacity(count)
    var result = initialResult
    for element in self {
      let (y, df) =
        valueWithDifferential(at: result, element, of: nextPartialResult)
      result = y
      differentials.append(df)
    }
    return (value: result, differential: { dSelf, dInitial in
      var dResult = dInitial
      for (dElement, df) in zip(dSelf.base, differentials) {
        dResult = df(dResult, dElement)
      }
      return dResult
    })
  }
}
