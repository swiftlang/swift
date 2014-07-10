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
//
//  
//
//===----------------------------------------------------------------------===//

/// Base protocol for Strideable; allows the definition of < to be
/// inferred for Comparable conformance
public protocol _Strideable {
  // FIXME: We'd like to name this type "DistanceType" but for
  // <rdar://problem/17619038>
  typealias Stride : SignedNumber
  
  func distanceTo(Self) -> Stride
  func advancedBy(Stride) -> Self
}

/// Compare two Strideables
public func < <T: _Strideable>(x: T, y: T) -> Bool {
  return x.distanceTo(y) > 0
}

/// A protocol for types that can be stride()d over.
public protocol Strideable : Comparable, _Strideable {}

public func + <T: Strideable> (lhs: T, rhs: T.Stride) -> T {
  return lhs.advancedBy(rhs)
}

public func + <T: Strideable> (lhs: T.Stride, rhs: T) -> T {
  return rhs.advancedBy(lhs)
}

public func - <T: Strideable> (lhs: T, rhs: T.Stride) -> T {
  return lhs.advancedBy(-rhs)
}

public func - <T: Strideable> (lhs: T, rhs: T) -> T.Stride {
  return rhs.distanceTo(lhs)
}

@assignment
public func += <T: Strideable> (inout lhs: T, rhs: T.Stride) {
  lhs = lhs.advancedBy(rhs)
}

@assignment
public func -= <T: Strideable> (inout lhs: T, rhs: T.Stride) {
  lhs = lhs.advancedBy(-rhs)
}

/// A Generator for StrideTo<T>
public struct StrideToGenerator<T: Strideable> : Generator {
  var current: T
  let end: T
  let stride: T.Stride

  public mutating func next() -> T? {
    if stride > 0 ? current >= end : current <= end {
      return nil
    }
    let ret = current
    current += stride
    return ret
  }
}

/// A Sequence of values formed by striding over a half-open interval
/// FIXME: should really be a Collection, as it is multipass
public struct StrideTo<T: Strideable> : Sequence {
  public func generate() -> StrideToGenerator<T> {
    return StrideToGenerator(current: start, end: end, stride: stride)
  }

  init(start: T, end: T, stride: T.Stride) {
    _precondition(stride != 0, "stride size must not be zero")
    // Unreachable endpoints are allowed; they just make for an
    // already-empty Sequence.
    self.start = start
    self.end = end
    self.stride = stride
  }
  
  let start: T
  let end: T
  let stride: T.Stride
}

public func stride<
  T: Strideable
>(from start: T, to end: T, by stride: T.Stride) -> StrideTo<T> {
  return StrideTo(start: start, end: end, stride: stride)
}
