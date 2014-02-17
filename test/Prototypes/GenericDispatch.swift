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

// A generic dispatch point with a default implementation
operator infix --- {}

//===----------------------------------------------------------------------===//

// Think of F as ForwardIndex and "---" as the O(N) distance measuring function
protocol F : F_ {
  func --- (x: Self, y: Self)
}

protocol F_ {
  // Non-defaulted requirements of F go here
}

// Default implementation of --- for Fs
func --- <I: F_>(x: I, y: I) {
  println("slow")
}

//===----------------------------------------------------------------------===//

// Think of R as RandomAccessIndex
protocol R : F, R_ {
}

// Default impelementation of --- for Rs, which is O(1)
func --- <I: R_>(x: I, y: I) {
  println("fast")
  I.sub(x, y)
}

// R_ refines F_ so its --- overload will be preferred.
protocol R_ : F_ {
  // Non-defaulted requirements of R go here, e.g. something to
  // measure the distance in O(1)
  class func sub(x: Self, y: Self)
}

//===----------------------------------------------------------------------===//

// A couple of types conforming to F
struct SlowIndex : F {}

struct FastIndex : R {
  static func sub(a: FastIndex, b: FastIndex) {
  }
}

// A generic function operating on F's
func f<T: F>(x: T) {
  x --- x // In here, we don't know whether T is an R or just an F
}

// CHECK-NEXT: slow
f(SlowIndex())

// CHECK-NEXT: fast
f(FastIndex())

//===----------------------------------------------------------------------===//

// Just to prove our point, demonstrate the effect on associated types

// Think of C as Collection
protocol C {
  typealias I : F  // I is the IndexType
  func s() -> I
}

struct FastCollection : C {
  typealias I = FastIndex
  func s() -> I {
    return I()
  }
}

struct SlowCollection : C {
  typealias I = SlowIndex
  func s() -> I {
    return I()
  }
}

func f<T: C>(x: T) {
  x.s() --- x.s()
}

// CHECK-NEXT: slow
f(SlowCollection())

// CHECK-NEXT: fast
f(FastCollection())
