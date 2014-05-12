//===--- GenericDispatch.swift - Demonstrate "partial specialization" -----===//
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
//  Do stuff in Swift that we didn't think was possible.  This file
//  will ensure that the capability doesn't regress and give us a
//  prototype to which we can refer when building the standard library.
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift | FileCheck %s

// CHECK: testing...
println("testing...")

//===----------------------------------------------------------------------===//
//===--- Think of this code as being "in the library" ---------------------===//
//===----------------------------------------------------------------------===//

//===--- F "base" protocol (like ForwardIndex) ----------------------------===//

// Protocols with default implementations are broken into two parts, a
// base and a more-refined part.  From the user's point-of-view,
// however, F_ and F should look like a single protocol.  This
// technique gets used throughout the standard library to break
// otherwise-cyclic protocol dependencies, which the compiler isn't
// yet smart enough to handle.
protocol F_ {
  // Non-defaulted requirements of F go here
  func succ() -> Self
}

protocol F : F_ {
  // This requirement allows generic distance() to find default
  // implementations.  Only the author of F and the author of a
  // refinement of F having a non-default distance implementation need
  // to know about it.  These refinements are expected to be rare
  // (which is why defaulted requirements are a win)
  func ~> (Self, (_Distance, (Self))) -> Int
}

// Operation tag for distance
//
// Operation tags allow us to use a single operator (~>) for
// dispatching every generic function with a default implementation.
// Only authors of specialized distance implementations need to touch
// this tag.
struct _Distance {}

// This function cleans up the syntax of invocations
func _distance<I>(other: I) -> (_Distance, (I)) {
  return (_Distance(), (other))
}

// Default Implementation of distance for F's
func ~> <I: F_>(self_:I, (_Distance, (I))) -> Int {
  self_.succ() // Use an F-specific operation
  println("F")
  return 0
}

//===--- Dispatching distance() function ----------------------------------===//

// This generic function is for user consumption; it dispatches to the
// appropriate implementation for T.
func distance<T: F>(x: T, y: T) -> Int {
  return x~>_distance(y)
}

//===--- R refined protocol (like RandomAccessIndex) ----------------------===//
protocol R_ : F_ {
  // Non-defaulted requirements of R go here, e.g. something to
  // measure the distance in O(1)
  class func sub(x: Self, y: Self)
}

protocol R : F, R_ {}

// R has its own default implementation of distance, which is O(1).
// Only the author of R needs to see this implementation.
func ~> <I: R_>(x: I, args: (_Distance, (I))) -> Int {
  let other = args.1
  I.sub(other, y: x)
  println("R")
  return 1
}

//===--- D refined protocol (like R, but with a custom distance) ----------===//
// Users who want to provide a custom distance function will use this protocol
protocol D_ : R_ {
  func distance(y: Self) -> Int
}

protocol D : R, D_ {}

// Dispatch to D's distance() requirement
// Only the author of D needs to see this implementation.
func ~> <I: D_>(x:I, args: (_Distance, (I))) -> Int {
  let other = args.1
  return x.distance(other)
}

//===----------------------------------------------------------------------===//
//===--- Think of this as being "user code" -------------------------------===//
//===----------------------------------------------------------------------===//

// This model of F automatically gets F's default implementation of distance
struct SlowIndex : F {
  func succ() -> SlowIndex { return self }
}

// This model of R automatically gets R's default implementation of distance
struct FastIndex : R {
  func succ() -> FastIndex { return self }
  static func sub(x: FastIndex, y: FastIndex) {}
}

struct X : D {
  // Customized distance implementation
  func distance(y: X) -> Int {
    println("X")
    return 3
  }
  // Inherited requirements
  func succ() -> X { return self }
  static func sub(x: X, y: X) {}
}

// Here's a generic function that uses our dispatching distance
func sort<T: F>(x: T) {
  // In here, we don't know whether T is an R or just a plain F, or
  // whether it has its own specialized impelementation of
  distance(x, x)
}
// CHECK-NEXT: F
sort(SlowIndex())

// CHECK-NEXT: R
sort(FastIndex())

// CHECK-NEXT: X
sort(X())

// CHECK-NEXT: done
println("done.")
