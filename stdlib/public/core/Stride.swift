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

/// This protocol is an implementation detail of `Strideable`; do
/// not use it directly.
public protocol _Strideable {
  // FIXME: We'd like to name this type "Distance" but for
  // <rdar://problem/17619038>
  /// A type that can represent the distance between two values of `Self`.
  typealias Stride : SignedNumberType

  /// Returns a stride `x` such that `self.advancedBy(x)` approximates
  /// `other`.
  ///
  /// - Complexity: O(1).
  ///
  /// - SeeAlso: `RandomAccessIndexType`'s `distanceTo`, which provides a
  ///   stronger semantic guarantee.
  func distanceTo(other: Self) -> Stride

  /// Returns a `Self` `x` such that `self.distanceTo(x)` approximates
  /// `n`.
  ///
  /// - Complexity: O(1).
  ///
  /// - SeeAlso: `RandomAccessIndexType`'s `advancedBy`, which
  ///   provides a stronger semantic guarantee.
  func advancedBy(n: Stride) -> Self
}

/// Compare two `Strideable`s.
public func < <T : _Strideable>(x: T, y: T) -> Bool {
  return x.distanceTo(y) > 0
}

public func == <T : _Strideable>(x: T, y: T) -> Bool {
  return x.distanceTo(y) == 0
}

/// Conforming types are notionally continuous, one-dimensional
/// values that can be offset and measured.
///
/// - SeeAlso: `stride(from: to: by:)` and `stride(from: through: by:)`
public protocol Strideable : Comparable, _Strideable {
  // FIXME: We'd like to name this type "Distance" but for
  // <rdar://problem/17619038>
  /// A type that can represent the distance between two values of `Self`.
  typealias Stride : SignedNumberType

  /// Returns a stride `x` such that `self.advancedBy(x)` approximates
  /// `other`.
  ///
  /// - Complexity: O(1).
  ///
  /// - SeeAlso: `RandomAccessIndexType`'s `distanceTo`, which provides a
  ///   stronger semantic guarantee.
  func distanceTo(other: Self) -> Stride

  /// Returns a `Self` `x` such that `self.distanceTo(x)` approximates
  /// `n`.
  ///
  /// - Complexity: O(1).
  ///
  /// - SeeAlso: `RandomAccessIndexType`'s `advancedBy`, which
  ///   provides a stronger semantic guarantee.
  func advancedBy(n: Stride) -> Self
}

public func + <T : Strideable> (lhs: T, rhs: T.Stride) -> T {
  return lhs.advancedBy(rhs)
}

public func + <T : Strideable> (lhs: T.Stride, rhs: T) -> T {
  return rhs.advancedBy(lhs)
}

public func - <T : Strideable> (lhs: T, rhs: T.Stride) -> T {
  return lhs.advancedBy(-rhs)
}

public func - <T : Strideable> (lhs: T, rhs: T) -> T.Stride {
  return rhs.distanceTo(lhs)
}

public func += <T : Strideable> (inout lhs: T, rhs: T.Stride) {
  lhs = lhs.advancedBy(rhs)
}

public func -= <T : Strideable> (inout lhs: T, rhs: T.Stride) {
  lhs = lhs.advancedBy(-rhs)
}

//===--- Deliberately-ambiguous operators for UnsignedIntegerTypes --------===//
// The UnsignedIntegerTypes all have a signed Stride type.  Without these     //
// overloads, expressions such as UInt(2) + Int(3) would compile.             //
//===----------------------------------------------------------------------===//

public func + <T : UnsignedIntegerType> (
  lhs: T, rhs: T._DisallowMixedSignArithmetic
) -> T {
  _sanityCheckFailure("Should not be callable.")
}

public func + <T : UnsignedIntegerType> (
  lhs: T._DisallowMixedSignArithmetic, rhs: T
) -> T {
  _sanityCheckFailure("Should not be callable.")
}

public func - <T : _DisallowMixedSignArithmetic> (
  lhs: T, rhs: T._DisallowMixedSignArithmetic
) -> T {
  _sanityCheckFailure("Should not be callable.")
}

public func - <T : _DisallowMixedSignArithmetic> (
  lhs: T, rhs: T
) -> T._DisallowMixedSignArithmetic {
  _sanityCheckFailure("Should not be callable.")
}

public func += <T : UnsignedIntegerType> (
  inout lhs: T, rhs: T._DisallowMixedSignArithmetic
) {
  _sanityCheckFailure("Should not be callable.")
}

public func -= <T : UnsignedIntegerType> (
  inout lhs: T, rhs: T._DisallowMixedSignArithmetic
) {
  _sanityCheckFailure("Should not be callable.")
}

//===----------------------------------------------------------------------===//

/// A GeneratorType for `StrideTo<T>`.
public struct StrideToGenerator<T : Strideable> : GeneratorType {
  var current: T
  let end: T
  let stride: T.Stride

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  public mutating func next() -> T? {
    if stride > 0 ? current >= end : current <= end {
      return nil
    }
    let ret = current
    current += stride
    return ret
  }
}

/// A `SequenceType` of values formed by striding over a half-open interval.
public struct StrideTo<T : Strideable> : SequenceType {
  // FIXME: should really be a CollectionType, as it is multipass

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> StrideToGenerator<T> {
    return StrideToGenerator(current: start, end: end, stride: stride)
  }

  init(start: T, end: T, stride: T.Stride) {
    _precondition(stride != 0, "stride size must not be zero")
    // Unreachable endpoints are allowed; they just make for an
    // already-empty SequenceType.
    self.start = start
    self.end = end
    self.stride = stride
  }

  let start: T
  let end: T
  let stride: T.Stride
}

/// Return the sequence of values (`start`, `start + stride`, `start +
/// stride + stride`, ... *last*) where *last* is the last value in
/// the progression that is less than `end`.
public func stride<
  T : Strideable
>(from start: T, to end: T, by stride: T.Stride) -> StrideTo<T> {
  return StrideTo(start: start, end: end, stride: stride)
}

/// A GeneratorType for `StrideThrough<T>`.
public struct StrideThroughGenerator<T : Strideable> : GeneratorType {
  var current: T
  let end: T
  let stride: T.Stride
  var done: Bool = false

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  public mutating func next() -> T? {
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

/// A `SequenceType` of values formed by striding over a closed interval.
public struct StrideThrough<T : Strideable> : SequenceType {
  // FIXME: should really be a CollectionType, as it is multipass

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> StrideThroughGenerator<T> {
    return StrideThroughGenerator(
      current: start, end: end, stride: stride, done: false)
  }

  init(start: T, end: T, stride: T.Stride) {
    _precondition(stride != 0, "stride size must not be zero")
    self.start = start
    self.end = end
    self.stride = stride
  }

  let start: T
  let end: T
  let stride: T.Stride
}

/// Return the sequence of values (`start`, `start + stride`, `start +
/// stride + stride`, ... *last*) where *last* is the last value in
/// the progression less than or equal to `end`.
///
/// - Note: There is no guarantee that `end` is an element of the sequence.
public func stride<
  T : Strideable
>(from start: T, through end: T, by stride: T.Stride) -> StrideThrough<T> {
  return StrideThrough(start: start, end: end, stride: stride)
}
