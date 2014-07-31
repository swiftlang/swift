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

public prefix func !<T : BooleanType>(a: T) -> Bool {
  return !a.boolValue
}

// Short circuiting logical operators.

// FIXME: these operators should be fully generic
// BLOCKED ON: <rdar://problem/13251236> [remapping bound function type not
// implemented yet (deduced closure types)].
//
// FIXME: the generic versions of these operators probably shouldn't
// be @transparent; ideally they will be overloaded with transparent
// bool-specific operators.  BLOCKED ON: <rdar://problem/11510876>
// [Implement overload resolution].

public func && <T: BooleanType, U: BooleanType>(
  lhs: T, rhs: @autoclosure () -> U
) -> Bool {
  return lhs.boolValue ? rhs().boolValue : false
}

public func || <T: BooleanType, U: BooleanType>(
  lhs: T, rhs: @autoclosure () -> U
) -> Bool {
  return lhs.boolValue ? true : rhs().boolValue
}

// FIXME: We can't make the above @transparent due to
// rdar://problem/17872402, so here are some @transparent overloads
// for Bool.  We've done the same for ObjCBool
@transparent public
func && <T: BooleanType>(
  lhs: T, rhs: @autoclosure () -> Bool
) -> Bool {
  return lhs.boolValue ? rhs().boolValue : false
}

@transparent public
func || <T: BooleanType>(
  lhs: T, rhs: @autoclosure () -> Bool
) -> Bool {
  return lhs.boolValue ? true : rhs().boolValue
}

