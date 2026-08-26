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
  @frozen
  public struct ArrayTangentVector {
    @frozen
    public enum Storage {
      case zero
      case oneHot(index: Int, value: Element, count: Int)
      case full([Element])
    }
    
    public var storage: Storage
    
    @inlinable
    public init(_ storage: Storage) {
      self.storage = storage
    }
  }
}

extension Array.ArrayTangentVector {
  @inlinable
  public var isEmpty: Bool {
    switch self.storage {
    case .zero:
      return true
    case .oneHot:
      return false // what if value == 0.0?
    case .full(let arr):
      return arr.isEmpty
    }
  }
  
  @inlinable
  var count: Int {
    switch self.storage {
    case .zero:
      return 0
    case .oneHot(_, _, let n):
      return n
    case .full(let arr):
      return arr.count
    }
  }
  
  @inlinable
  mutating func removeLast(_ k: Int = 1) {
    switch self.storage {
    case .zero:
      return
    case .oneHot(index: let i, value: let v, count: let n):
      precondition(n > k)
      if i >= n - k {
        self = .init(.zero)
      } else {
        self = .init(.oneHot(index: i, value: v, count: n - k))
      }
    case .full(var arr):
      arr.removeLast(k)
      self.storage = .full(arr)
    }
  }
  
  @inlinable
  func dropFirst(_ k: Int = 1) -> Self {
    switch self.storage {
    case .zero:
      return .init(.zero)
    case .oneHot(index: let i, value: let v, count: let n):
      if i < k {
        return .init(.zero)
      } else {
        return .init(.oneHot(index: i - k, value: v, count: n - k))
      }
    case .full(let arr):
      return .init(.full(Array(arr.dropFirst(k))))
    }
  }
}

extension Array.ArrayTangentVector where Element: AdditiveArithmetic {
  @inlinable
  subscript(index: Int) -> Element {
    switch storage {
    case .zero:
      return .zero
    case .oneHot(index: let i, value: let v, count: _):
      if index == i { return v }
      else { return .zero }
    case .full(let arr):
      return arr[index]
    }
  }
  
  @inlinable
  subscript(range: Range<Int>) -> Array<Element> {
    fatalError()
  }
}

extension Array.ArrayTangentVector: ExpressibleByArrayLiteral where Element: Differentiable {
  @inlinable
  public init(arrayLiteral elements: Element...) {
    self.init(.full(elements))
  }
}

extension Array.ArrayTangentVector: Equatable where Element: Equatable {
  @inlinable
  public static func ==(lhs: Self, rhs: Self) -> Bool {
    switch (lhs.storage, rhs.storage) {
    case (.zero, .zero):
      return true
    case (.oneHot(let il, let vl, let nl), .oneHot(let ir, let vr, let nr)):
      return il == ir && vl == vr && nl == nr
    case (.full(let lhs), .full(let rhs)):
      return lhs == rhs
    default:
      // we should consider also comparing:
      // - .oneHot to a .full array of zeroes with one non-zero value.
      // - .zero to .oneHot with a .zero value
      // - .zero to a .full array of .zero values.
      return false
    }
  }
}

extension Array.ArrayTangentVector: AdditiveArithmetic where Element: AdditiveArithmetic {
  @inlinable
  public static var zero: Self { .init(.zero) }
  
  @inlinable
  public static func + (lhs: Self, rhs: Self) -> Self {
    switch (lhs.storage, rhs.storage) {
    case (_, .zero):
      return lhs
    case (.zero, _):
      return rhs
    case (.oneHot(let il, let vl, let nl), .oneHot(let ir, let vr, let nr)):
      precondition(nl == nr)
      if il == ir {
        return .init(.oneHot(index: il, value: vl + vr, count: nl))
      } else {
        var arr = [Element](repeating: .zero, count: nl)
        arr[il] = vl
        arr[ir] = vr
        return .init(.full(arr))
      }
    case (.oneHot(let il, let vl, let nl), .full(var rhs)):
      precondition(nl == rhs.count)
      rhs[il] += vl
      return .init(.full(rhs))
    case (.full(var lhs), .oneHot(let ir, let vr, let nr)):
      precondition(lhs.count == nr)
      lhs[ir] += vr
      return .init(.full(lhs))
    case (.full(let lhs), .full(let rhs)):
      precondition(lhs.count == rhs.count, "Count mismatch: \(lhs.count) and \(rhs.count)")
      return .init(.full(zip(lhs, rhs).map(+)))
    }
  }
  
  @inlinable
  public static func - (lhs: Self, rhs: Self) -> Self {
    switch (lhs.storage, rhs.storage) {
    case (_, .zero):
      return lhs
    case (.zero, .oneHot(let ir, let vr, let nr)):
      return .init(.oneHot(index: ir, value: .zero - vr, count: nr))
    case (.zero, .full(let rhs)):
      return .init(.full(rhs.map { .zero - $0 }))
    case (.oneHot(let il, let vl, let nl), .oneHot(let ir, let vr, let nr)):
      precondition(nl == nr)
      if il == ir {
        return .init(.oneHot(index: il, value: vl - vr, count: nl))
      } else {
        var arr = [Element](repeating: .zero, count: nl)
        arr[il] = vl
        arr[ir] = .zero - vr
        return .init(.full(arr))
      }
    case (.oneHot(let il, let vl, let nl), .full(let rhs)):
      precondition(nl == rhs.count)
      var result = rhs.map { .zero - $0 }
      result[il] += vl
      return .init(.full(result))
    case (.full(var lhs), .oneHot(let ir, let vr, let nr)):
      precondition(lhs.count == nr)
      lhs[ir] -= vr
      return .init(.full(lhs))
    case (.full(let lhs), .full(let rhs)):
      precondition(lhs.count == rhs.count, "Count mismatch: \(lhs.count) and \(rhs.count)")
      return .init(.full(zip(lhs, rhs).map(-)))
    }
  }
}

extension Array.ArrayTangentVector: Differentiable where Element: AdditiveArithmetic & Differentiable {
  public typealias TangentVector = Array<Element.TangentVector>.ArrayTangentVector
  
  @inlinable
  public mutating func move(by offset: TangentVector) {
    switch (self.storage, offset.storage) {
    case (_, .zero):
      return
    case (.zero, .oneHot(let iOffset, let vOffset, let nOffset)):
      var v = Element.zero
      v.move(by: vOffset)
      self = .init(.oneHot(index: iOffset, value: v, count: nOffset))
    case (.oneHot(let i, var v, let n), .oneHot(let iOffset, let vOffset, let nOffset)):
      precondition(n == nOffset)
      if i == iOffset {
        v.move(by: vOffset)
        self = .init(.oneHot(index: i, value: v, count: n))
      } else {
        var arr = [Element](repeating: .zero, count: n)
        arr[i] = v
        arr[iOffset].move(by: vOffset)
        self = .init(.full(arr))
      }
    case (.full(var arr), .oneHot(let iOffset, let vOffset, let nOffset)):
      precondition(arr.count == nOffset)
      arr[iOffset].move(by: vOffset)
      self = .init(.full(arr))
    case (.zero, .full(let arrOffset)):
      var result = [Element](repeating: .zero, count: arrOffset.count)
      for i in arrOffset.indices { result[i].move(by: arrOffset[i]) }
      self = .init(.full(result))
    case (.oneHot(let i, let v, let n), .full(let arrOffset)):
      precondition(n == arrOffset.count)
      var arr = [Element](repeating: .zero, count: n)
      arr[i] = v
      for j in arrOffset.indices { arr[j].move(by: arrOffset[j]) }
      self = .init(.full(arr))
    case (.full(var arr), .full(let arrOffset)):
      for i in arr.indices { arr[i].move(by: arrOffset[i]) }
      self = .init(.full(arr))
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
    Array<Element.TangentVector>.ArrayTangentVector

  @inlinable
  public mutating func move(by offset: TangentVector) {
    switch offset.storage {
    case .zero:
      return
    case .oneHot(let i, let v, let n):
      precondition(self.count == n)
      self[i].move(by: v)
    case .full(let arr):
      for i in self.indices {
        self[i].move(by: arr[i])
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// Derivatives
//===----------------------------------------------------------------------===//

extension Array where Element: Differentiable {
  @inlinable
  @derivative(of: subscript.get)
  func _vjpSubscriptGet(index: Int) -> (
    value: Element, pullback: (Element.TangentVector) -> TangentVector
  ) {
    let n = self.count
    return (
      value: self[index],
      pullback: { v in
          .init(.oneHot(index: index, value: v, count: n))
      }
    )
  }

  @inlinable
  @derivative(of: +)
  static func _vjpConcatenate(_ lhs: Self, _ rhs: Self) -> (
    value: Self,
    pullback: (TangentVector) -> (TangentVector, TangentVector)
  ) {
    func pullback(_ v: TangentVector) -> (TangentVector, TangentVector) {
      if v.isEmpty {
        return (.zero, .zero)
      }
      precondition(
        v.count == lhs.count + rhs.count, """
          Tangent vector with invalid count \(v.count); expected to \
          equal the sum of operand counts \(lhs.count) and \(rhs.count)
          """)
      return (
        TangentVector(.full([Element.TangentVector](v[0..<lhs.count]))),
        TangentVector(.full([Element.TangentVector](v[lhs.count..<(lhs.count + rhs.count)])))
      )
    }
    return (lhs + rhs, pullback)
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
      defer { v.removeLast() }
      return v[appendedElementIndex]
    })
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
      let drhs = TangentVector(v.dropFirst(lhsCount).storage)
      let rhsCount = drhs.count
      v.removeLast(rhsCount)
      return drhs
    })
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
        switch v.storage {
        case .zero:
          return .zero
        case .oneHot(_, let value, _):
          return value
        case .full(let arr):
          return arr.reduce(.zero, +)
        }
      }
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
    let count = self.count
    var values: [Result] = []
    var pullbacks: [(Result.TangentVector) -> Element.TangentVector] = []
    values.reserveCapacity(count)
    pullbacks.reserveCapacity(count)
    for x in self {
      let (y, pb) = valueWithPullback(at: x, of: body)
      values.append(y)
      pullbacks.append(pb)
    }
    func pullback(_ tans: Array<Result>.TangentVector) -> Array.TangentVector {
      switch tans.storage {
      case .zero:
        return .zero
      case .oneHot(index: let index, value: let value, count: let count):
        return .init(.oneHot(index: index, value: pullbacks[index](value), count: count))
      case .full(let arr):
        return .init(.full(zip(arr, pullbacks).map { tan, pb in pb(tan) }))
      }
    }
    return (value: values, pullback: pullback)
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
      let (y, pb) = valueWithPullback(at: result, element, of: nextPartialResult)
      result = y
      pullbacks.append(pb)
    }
    return (
      value: result,
      pullback: { tangent in
        var resultTangent = tangent
        var elementTangents: [Element.TangentVector] = []
        elementTangents.reserveCapacity(count)
        for pullback in pullbacks.reversed() {
          let (newResultTangent, elementTangent) = pullback(resultTangent)
          resultTangent = newResultTangent
          elementTangents.append(elementTangent)
        }
        return (
          TangentVector(.full(Array<Element.TangentVector>(elementTangents.reversed()))),
          resultTangent
        )
      }
    )
  }
}
