// RUN: %empty-directory(%t)

// Build the library and print its interface.
// RUN: %target-swift-frontend -emit-module -o %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface \
// RUN:   -enable-library-evolution -swift-version 5 -module-name Lib \
// RUN:   %s -DLIB
// RUN: %FileCheck %s < %t/Lib.swiftinterface

// The interface must compile back into a module...
// RUN: %target-swift-frontend -compile-module-from-interface -swift-version 5 \
// RUN:   -module-name Lib -o %t/LibFromInterface.swiftmodule %t/Lib.swiftinterface

// ...and a client must be able to use it through that module.
// RUN: %empty-directory(%t/mods)
// RUN: cp %t/LibFromInterface.swiftmodule %t/mods/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck -I %t/mods %s -DCLIENT

#if LIB

public struct NC: ~Copyable {
  public var v: Int
  public init(v: Int) { self.v = v }
}

// CHECK: public struct Pub {
public struct Pub {
  public var slots: [Int] = [0, 0]
  public init() {}

  // CHECK: public subscript(b i: borrowing Swift{{(::|\.)}}Int) -> Swift{{(::|\.)}}Int
  public subscript(b i: borrowing Int) -> Int {
    get { return slots[i] }
    set { slots[i] = newValue }
  }

  // CHECK: public subscript(io i: inout Swift{{(::|\.)}}Int) -> Swift{{(::|\.)}}Int
  public subscript(io i: inout Int) -> Int {
    get { return slots[i] }
    set { slots[i] = newValue; i += 1 }
  }

  // CHECK: public subscript(ncio n: inout Lib{{(::|\.)}}NC) -> Swift{{(::|\.)}}Int
  public subscript(ncio n: inout NC) -> Int {
    get { return slots[n.v] }
    set { slots[n.v] = newValue; n.v += 1 }
  }

  // A noncopyable index that is only borrowed. Note this one is deliberately
  // get-only: a *settable* resilient subscript with a `borrowing` noncopyable
  // index currently hits "copy of noncopyable typed value" in the move-only
  // checker, because the `modify` coroutine synthesized for resilient storage
  // forwards the index across its yield and the prologue copy of the index
  // cannot be eliminated there.
  // CHECK: public subscript(ncb n: borrowing Lib{{(::|\.)}}NC) -> Swift{{(::|\.)}}Int
  public subscript(ncb n: borrowing NC) -> Int {
    get { return slots[n.v] }
  }

  // CHECK: public subscript<T>(g t: borrowing T) -> Swift{{(::|\.)}}Int where T : ~Copyable
  public subscript<T: ~Copyable>(g t: borrowing T) -> Int { return 0 }
}

// CHECK: public protocol HasSubs {
public protocol HasSubs {
  // CHECK: subscript(pb i: borrowing Swift{{(::|\.)}}Int) -> Swift{{(::|\.)}}Int { get }
  subscript(pb i: borrowing Int) -> Int { get }
  // CHECK: subscript(pio i: inout Swift{{(::|\.)}}Int) -> Swift{{(::|\.)}}Int { get set }
  subscript(pio i: inout Int) -> Int { get set }
}

#endif

#if CLIENT

import Lib

func useBorrowing(p: Pub, n: borrowing NC) -> Int {
  return p[b: 0] + p[ncb: n] + p[g: n]
}

func useInOut(p: inout Pub, i: inout Int, n: inout NC) -> Int {
  var r = p[io: &i]
  p[io: &i] = 3
  p[io: &i] += 1
  r += p[ncio: &n]
  p[ncio: &n] = 4
  p[ncio: &n] += 1
  return r
}

func useProtocol<T: HasSubs>(t: inout T, i: inout Int) -> Int {
  var r = t[pb: 0]
  r += t[pio: &i]
  t[pio: &i] = 5
  t[pio: &i] += 1
  return r
}

#endif
