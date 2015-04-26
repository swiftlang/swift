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
// BooleanType
//===----------------------------------------------------------------------===//

/// Return the result of inverting `a`'s logic value
public prefix func !<T : BooleanType>(a: T) -> Bool {
  return !a.boolValue
}

/// If `lhs` is `false`, return it.  Otherwise, evaluate `rhs` and
/// return its `boolValue`.
@inline(__always)
public func && <T : BooleanType, U : BooleanType>(
  lhs: T, @autoclosure rhs: () -> U
) -> Bool {
  return lhs.boolValue ? rhs().boolValue : false
}

/// If `lhs` is `true`, return it.  Otherwise, evaluate `rhs` and
/// return its `boolValue`.
@inline(__always)
public func || <T : BooleanType, U : BooleanType>(
  lhs: T, @autoclosure rhs: () -> U
) -> Bool {
  return lhs.boolValue ? true : rhs().boolValue
}

// FIXME: We can't make the above @transparent due to
// rdar://problem/19418937, so here are some @transparent overloads
// for Bool.  We've done the same for ObjCBool
@transparent
public func && <T : BooleanType>(
  lhs: T, @autoclosure rhs: () -> Bool
) -> Bool {
  return lhs.boolValue ? rhs().boolValue : false
}

@transparent
public func || <T : BooleanType>(
  lhs: T, @autoclosure rhs: () -> Bool
) -> Bool {
  return lhs.boolValue ? true : rhs().boolValue
}
