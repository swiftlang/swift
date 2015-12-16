//===----------------------------------------------------------------------===//
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
// Boolean
//===----------------------------------------------------------------------===//

/// Return the result of inverting `a`'s logic value.
@warn_unused_result
public prefix func !<T : Boolean>(a: T) -> Bool {
  return !a.boolValue
}

/// If `lhs` is `false`, return it.  Otherwise, evaluate `rhs` and
/// return its `boolValue`.
@inline(__always)
@warn_unused_result
public func && <T : Boolean, U : Boolean>(
  lhs: T, @autoclosure rhs: () throws -> U
) rethrows -> Bool {
  return lhs.boolValue ? try rhs().boolValue : false
}

/// If `lhs` is `true`, return it.  Otherwise, evaluate `rhs` and
/// return its `boolValue`.
@inline(__always)
@warn_unused_result
public func || <T : Boolean, U : Boolean>(
  lhs: T, @autoclosure rhs: () throws -> U
) rethrows -> Bool {
  return lhs.boolValue ? true : try rhs().boolValue
}

// FIXME: We can't make the above @_transparent due to
// rdar://problem/19418937, so here are some @_transparent overloads
// for Bool.  We've done the same for ObjCBool
@_transparent
@warn_unused_result
public func && <T : Boolean>(
  lhs: T, @autoclosure rhs: () throws -> Bool
) rethrows -> Bool {
  return lhs.boolValue ? try rhs().boolValue : false
}

@_transparent
@warn_unused_result
public func || <T : Boolean>(
  lhs: T, @autoclosure rhs: () throws -> Bool
) rethrows -> Bool {
  return lhs.boolValue ? true : try rhs().boolValue
}
