//===--- Stride.swift - Components for stride(...) iteration --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Conforming types are notionally continuous, one-dimensional
/// values that can be offset and measured.
public protocol Strideable : Comparable {
  // FIXME: We'd like to name this type "Distance" but for
  // <rdar://problem/17619038>
  /// A type that can represent the distance between two values of `Self`.
  typealias Stride : SignedNumber

  /// Returns a stride `x` such that `self.advancedBy(x)` approximates
  /// `other`.
  ///
  /// - Complexity: O(1).
  ///
  /// - SeeAlso: `RandomAccessIndex`'s `distanceTo`, which provides a
  ///   stronger semantic guarantee.
  @warn_unused_result
  func distanceTo(other: Self) -> Stride

  /// Returns a `Self` `x` such that `self.distanceTo(x)` approximates
  /// `n`.
  ///
  /// - Complexity: O(1).
  ///
  /// - SeeAlso: `RandomAccessIndex`'s `advancedBy`, which
  ///   provides a stronger semantic guarantee.
  @warn_unused_result
  func advancedBy(n: Stride) -> Self
}


/// Compare two `Strideable`s.
public func < <T : Strideable>(x: T, y: T) -> Bool {
  return x.distanceTo(y) > 0
}

public func == <T : Strideable>(x: T, y: T) -> Bool {
  return x.distanceTo(y) == 0
}

@warn_unused_result
public func + <T : Strideable>(lhs: T, rhs: T.Stride) -> T {
  return lhs.advancedBy(rhs)
}

@warn_unused_result
public func + <T : Strideable>(lhs: T.Stride, rhs: T) -> T {
  return rhs.advancedBy(lhs)
}

@warn_unused_result
public func - <T : Strideable>(lhs: T, rhs: T.Stride) -> T {
  return lhs.advancedBy(-rhs)
}

@warn_unused_result
public func - <T : Strideable>(lhs: T, rhs: T) -> T.Stride {
  return rhs.distanceTo(lhs)
}

public func += <T : Strideable>(inout lhs: T, rhs: T.Stride) {
  lhs = lhs.advancedBy(rhs)
}

public func -= <T : Strideable>(inout lhs: T, rhs: T.Stride) {
  lhs = lhs.advancedBy(-rhs)
}

//===--- Deliberately-ambiguous operators for UnsignedIntegerTypes --------===//
// The UnsignedIntegerTypes all have a signed Stride type.  Without these     //
// overloads, expressions such as UInt(2) + Int(3) would compile.             //
//===----------------------------------------------------------------------===//

public func + <T : UnsignedInteger>(
  lhs: T, rhs: T._DisallowMixedSignArithmetic
) -> T {
  _sanityCheckFailure("Should not be callable.")
}

public func + <T : UnsignedInteger>(
  lhs: T._DisallowMixedSignArithmetic, rhs: T
) -> T {
  _sanityCheckFailure("Should not be callable.")
}

public func - <T : _DisallowMixedSignArithmetic>(
  lhs: T, rhs: T._DisallowMixedSignArithmetic
) -> T {
  _sanityCheckFailure("Should not be callable.")
}

public func - <T : _DisallowMixedSignArithmetic>(
  lhs: T, rhs: T
) -> T._DisallowMixedSignArithmetic {
  _sanityCheckFailure("Should not be callable.")
}

public func += <T : UnsignedInteger>(
  inout lhs: T, rhs: T._DisallowMixedSignArithmetic
) {
  _sanityCheckFailure("Should not be callable.")
}

public func -= <T : UnsignedInteger>(
  inout lhs: T, rhs: T._DisallowMixedSignArithmetic
) {
  _sanityCheckFailure("Should not be callable.")
}

//===----------------------------------------------------------------------===//

/// An `IteratorProtocol` for `StrideTo<Element>`.
public struct StrideToIterator<Element : Strideable> : IteratorProtocol {
  var current: Element
  let end: Element
  let stride: Element.Stride

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  public mutating func next() -> Element? {
    if stride > 0 ? current >= end : current <= end {
      return nil
    }
    let ret = current
    current += stride
    return ret
  }
}

/// A `Sequence` of values formed by striding over a half-open interval.
public struct StrideTo<Element : Strideable> : Sequence {
  // FIXME: should really be a Collection, as it is multipass

  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func iterator() -> StrideToIterator<Element> {
    return StrideToIterator(current: start, end: end, stride: stride)
  }

  init(start: Element, end: Element, stride: Element.Stride) {
    _precondition(stride != 0, "stride size must not be zero")
    // Unreachable endpoints are allowed; they just make for an
    // already-empty Sequence.
    self.start = start
    self.end = end
    self.stride = stride
  }

  let start: Element
  let end: Element
  let stride: Element.Stride
}

extension Strideable {
  /// Return the sequence of values (`self`, `self + stride`, `self +
  /// stride + stride`, ... *last*) where *last* is the last value in
  /// the progression that is less than `end`.
  @warn_unused_result
  public func stride(to end: Self, by stride: Stride) -> StrideTo<Self> {
    return StrideTo(start: self, end: end, stride: stride)
  }
}

/// An `IteratorProtocol` for `StrideThrough<Element>`.
public struct StrideThroughIterator<Element : Strideable> : IteratorProtocol {
  var current: Element
  let end: Element
  let stride: Element.Stride
  var done: Bool = false

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  public mutating func next() -> Element? {
    if done {
      return nil
    }
    if stride > 0 ? current >= end : current <= end {
      if current == end {
        done = true
        return current
      }
      return nil
    }
    let ret = current
    current += stride
    return ret
  }
}

/// A `Sequence` of values formed by striding over a closed interval.
public struct StrideThrough<Element : Strideable> : Sequence {
  // FIXME: should really be a Collection, as it is multipass

  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func iterator() -> StrideThroughIterator<Element> {
    return StrideThroughIterator(
      current: start, end: end, stride: stride, done: false)
  }

  init(start: Element, end: Element, stride: Element.Stride) {
    _precondition(stride != 0, "stride size must not be zero")
    self.start = start
    self.end = end
    self.stride = stride
  }

  let start: Element
  let end: Element
  let stride: Element.Stride
}

extension Strideable {
  /// Return the sequence of values (`self`, `self + stride`, `self +
  /// stride + stride`, ... *last*) where *last* is the last value in
  /// the progression less than or equal to `end`.
  ///
  /// - Note: There is no guarantee that `end` is an element of the sequence.
  @warn_unused_result
  public func stride(
    through end: Self, by stride: Stride
  ) -> StrideThrough<Self> {
    return StrideThrough(start: self, end: end, stride: stride)
  }
}

