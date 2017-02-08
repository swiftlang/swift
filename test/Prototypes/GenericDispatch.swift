//===--- GenericDispatch.swift - Demonstrate "partial specialization" -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Do stuff in Swift that we didn't think was possible.  This file
//  will ensure that the capability doesn't regress and give us a
//  prototype to which we can refer when building the standard library.
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// CHECK: testing...
print("testing...")

//===----------------------------------------------------------------------===//
//===--- Think of this code as being "in the library" ---------------------===//
//===----------------------------------------------------------------------===//

//===--- F "base" protocol (like ForwardIndex) ----------------------------===//

protocol F {
  func successor() -> Self
  // This requirement allows generic distance() to find default
  // implementations.  Only the author of F and the author of a
  // refinement of F having a non-default distance implementation need
  // to know about it.  These refinements are expected to be rare
  // (which is why defaulted requirements are a win)
  func ~> (_: Self, _: (_Distance, (Self))) -> Int
}

// Operation tag for distance
//
// Operation tags allow us to use a single operator (~>) for
// dispatching every generic function with a default implementation.
// Only authors of specialized distance implementations need to touch
// this tag.
struct _Distance {}

// This function cleans up the syntax of invocations
func _distance<I>(_ other: I) -> (_Distance, (I)) {
  return (_Distance(), (other))
}

// Default Implementation of distance for F's
func ~> <I: F>(self_:I, _: (_Distance, (I))) -> Int {
  self_.successor() // Use an F-specific operation
  print("F")
  return 0
}

//===--- Dispatching distance() function ----------------------------------===//

// This generic function is for user consumption; it dispatches to the
// appropriate implementation for T.
func distance<T: F>(_ x: T, _ y: T) -> Int {
  return x~>_distance(y)
}

//===--- R refined protocol (like RandomAccessIndex) ----------------------===//
protocol R : F {
  // Non-defaulted requirements of R go here, e.g. something to
  // measure the distance in O(1)
  static func sub(_ x: Self, y: Self)
}

// R has its own default implementation of distance, which is O(1).
// Only the author of R needs to see this implementation.
func ~> <I: R>(x: I, args: (_Distance, (I))) -> Int {
  let other = args.1
  I.sub(other, y: x)
  print("R")
  return 1
}

//===--- D refined protocol (like R, but with a custom distance) ----------===//
// Users who want to provide a custom distance function will use this protocol
protocol D : R {
  func distance(_ y: Self) -> Int
}

// Dispatch to D's distance() requirement
// Only the author of D needs to see this implementation.
func ~> <I: D>(x: I, args: (_Distance, (I))) -> Int {
  let other = args.1
  return x.distance(other)
}

//===----------------------------------------------------------------------===//
//===--- Think of this as being "user code" -------------------------------===//
//===----------------------------------------------------------------------===//

// This model of F automatically gets F's default implementation of distance
struct SlowIndex : F {
  func successor() -> SlowIndex { return self }
}

// This model of R automatically gets R's default implementation of distance
struct FastIndex : R {
  func successor() -> FastIndex { return self }
  static func sub(_ x: FastIndex, y: FastIndex) {}
}

struct X : D {
  // Customized distance implementation
  func distance(_ y: X) -> Int {
    print("X")
    return 3
  }
  // Inherited requirements
  func successor() -> X { return self }
  static func sub(_ x: X, y: X) {}
}

// Here's a generic function that uses our dispatching distance
func sort<T: F>(_ x: T) {
  // In here, we don't know whether T is an R or just a plain F, or
  // whether it has its own specialized implementation of
  distance(x, x)
}
// CHECK-NEXT: F
sort(SlowIndex())

// CHECK-NEXT: R
sort(FastIndex())

// CHECK-NEXT: X
sort(X())

// CHECK-NEXT: done
print("done.")
