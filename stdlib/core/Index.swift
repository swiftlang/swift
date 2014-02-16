//===--- Index.swift - A position in a Collection -------------------------===//
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
//  ForwardIndex, BidirectionalIndex, and RandomAccessIndex
//
//===----------------------------------------------------------------------===//

protocol ForwardIndex : Equatable {
  func succ() -> Self
}

@prefix @assignment @transparent
func ++ <T : ForwardIndex> (inout x: T) -> T {
  x = x.succ()
  return x
}

@postfix @assignment @transparent
func ++ <T : ForwardIndex> (inout x: T) -> T {
  var ret = x
  x = x.succ()
  return ret
}

protocol BidirectionalIndex : ForwardIndex {
  func pred() -> Self
}

@prefix @assignment @transparent
func -- <T: BidirectionalIndex> (inout x: T) -> T {
  x = x.pred()
  return x
}


@postfix @assignment @transparent
func -- <T: BidirectionalIndex> (inout x: T) -> T {
  var ret = x
  x = x.pred()
  return ret
}

protocol RandomAccessIndex : BidirectionalIndex, NumericOperations {
  typealias DistanceType
  class func sub(lhs: Self, rhs: Self) -> (DistanceType, Bool)
  class func sub(lhs: Self, rhs: DistanceType) -> (Self, Bool)
  class func add(lhs: Self, rhs: DistanceType) -> (Self, Bool)

  func < (lhs: Self, rhs: Self) -> Bool
  func <= (lhs: Self, rhs: Self) -> Bool
  func > (lhs: Self, rhs: Self) -> Bool
  func >= (lhs: Self, rhs: Self) -> Bool
}

@transparent
func - <T : RandomAccessIndex>(x: T, y: T) -> T.DistanceType {
  return overflowChecked(T.sub(x, y))
}

@transparent
func &- <T : RandomAccessIndex>(x: T, y: T) -> T.DistanceType {
  return T.sub(x, y).0
}

@transparent
func - <T : RandomAccessIndex>(x: T, y: T.DistanceType) -> T {
  return overflowChecked(T.sub(x, y))
}

@transparent
func &- <T : RandomAccessIndex>(x: T, y: T.DistanceType) -> T {
  return T.sub(x, y).0
}

@infix @assignment @transparent
func += <T : RandomAccessIndex> (inout lhs: T, rhs: T.DistanceType) {
  lhs = overflowChecked(T.add(lhs, rhs))
}

@infix @assignment @transparent
func -= <
  T: RandomAccessIndex where T.DistanceType: SignedNumber
> (inout lhs: T, rhs: T.DistanceType) {
  lhs = overflowChecked(T.add(lhs, -rhs))
}

@transparent
func + <T : RandomAccessIndex> (lhs: T, rhs: T.DistanceType) -> T {
  return overflowChecked(T.add(lhs, rhs))
}

@transparent
func + <T : RandomAccessIndex> (lhs: T.DistanceType, rhs: T) -> T {
  return overflowChecked(T.add(rhs, lhs))
}

@transparent
func &+ <T : RandomAccessIndex> (lhs: T, rhs: T) -> T {
  return T.add(lhs, rhs).0
}

@transparent
func &+ <T : RandomAccessIndex> (lhs: T, rhs: T.DistanceType) -> T {
  return T.add(lhs, rhs).0
}

@transparent
func &+ <T : RandomAccessIndex> (lhs: T.DistanceType, rhs: T) -> T {
  return T.add(rhs, lhs).0
}

@transparent
func - <T : RandomAccessIndex where T.DistanceType : SignedNumber> (
  lhs: T, rhs: T.DistanceType)
-> T {
  return overflowChecked(T.add(lhs, -rhs))
}

@transparent
func &- <T : RandomAccessIndex where T.DistanceType : SignedNumber> (
  lhs: T, rhs: T.DistanceType)
-> T {
  return T.add(lhs, -rhs).0
}
