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

public protocol Strideable {
  // FIXME: We'd like to name this type "DistanceType" but for
  // <rdar://problem/17619038>
  typealias Stride : SignedNumber
  
  func distanceTo(Self) -> Stride
  func advancedBy(Stride) -> Self
}

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

