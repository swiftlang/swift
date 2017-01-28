// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -O %s -emit-sil | %FileCheck %s

// Make sure that we can dig all the way through the class hierarchy and
// protocol conformances.

// CHECK-LABEL: sil @_T028devirt_inherited_conformance6driveryyF : $@convention(thin) () -> () {
// CHECK: bb0
// CHECK: [[UNKNOWN2a:%.*]] = function_ref @unknown2a : $@convention(thin) () -> ()
// CHECK: apply [[UNKNOWN2a]]
// CHECK: apply [[UNKNOWN2a]]
// CHECK: [[UNKNOWN3a:%.*]] = function_ref @unknown3a : $@convention(thin) () -> ()
// CHECK: apply [[UNKNOWN3a]]
// CHECK: apply [[UNKNOWN3a]]
// CHECK: return

@_silgen_name("unknown1a")
func unknown1a() -> ()
@_silgen_name("unknown1b")
func unknown1b() -> ()
@_silgen_name("unknown2a")
func unknown2a() -> ()
@_silgen_name("unknown2b")
func unknown2b() -> ()
@_silgen_name("unknown3a")
func unknown3a() -> ()
@_silgen_name("unknown3b")
func unknown3b() -> ()

struct Int32 {}

protocol P {
  // We do not specialize typealias's correctly now.
  //typealias X
  func doSomething(_ x : Int32)

  // This exposes a SILGen bug. FIXME: Fix up this test in the future.
  // class func doSomethingMeta()
}

class B : P {
  // We do not specialize typealias's correctly now.
  //typealias X = B
  func doSomething(_ x : Int32) {
    unknown1a()
  }

  // See comment in protocol P
  //class func doSomethingMeta() {
  //  unknown1b()
  //}
}

class B2 : B {
  // When we have covariance in protocols, change this to B2.
  // We do not specialize typealias correctly now.
  //typealias X = B
  override func doSomething(_ x : Int32) {
    unknown2a()
  }

  // See comment in protocol P
  //override class func doSomethingMeta() {
  //  unknown2b()
  //}
}

class B3 : B {
  // When we have covariance in protocols, change this to B3.
  // We do not specialize typealias correctly now.
  //typealias X = B
  override func doSomething(_ x : Int32) {
    unknown3a()
  }

  // See comment in protocol P
  //override class func doSomethingMeta() {
  //  unknown3b()
  //}
}

func WhatShouldIDo<T : P>(_ t : T, _ x : Int32) {
  t.doSomething(x)
}

func WhatShouldIDo2(_ p : P, _ x : Int32) {
  p.doSomething(x)
}

public func driver() -> () {
  let b2 = B2()
  let b3 = B3()
  let x = Int32()

  WhatShouldIDo(b2, x)
  WhatShouldIDo2(b2, x)
  WhatShouldIDo(b3, x)
  WhatShouldIDo2(b3, x)
}

// Test that inherited conformances work properly with
// standard operators like == and custom operators like ---

// Comparable is similar to Equatable, but uses a usual method
// instead of an operator.
public protocol Comparable {
   func compare(_: Self, _: Self) -> Bool
}

// Define a custom operator to be used instead of ==
infix operator --- { associativity left precedence 140 } 

// Simple is a protocol that simply defines an operator and
// a few methods with different number of arguments.
public protocol Simple {
   func foo(_: Self) -> Bool
   func boo(_: Self, _: Self) -> Bool
   func ---(_: Self, _: Self) -> Bool
}

public class C: Equatable, Comparable, Simple {
  public func compare(_ c1:C, _ c2:C) -> Bool {
    return c1 == c2
  }
  
  public func foo(_ c:C) -> Bool {
    return true
  }

  public func boo(_ c1:C, _ c2:C) -> Bool {
    return false
  }
}

// D inherits a bunch of conformances from C.
// We want to check that compiler can handle
// them properly and is able to devirtualize
// them.

public class D: C {
}

public func ==(lhs: C, rhs: C) -> Bool {
  return true
}

public func ---(lhs: C, rhs: C) -> Bool {
  return true
}

public func compareEquals<T:Equatable>(_ x: T, _ y:T) -> Bool {
  return x == y
}

public func compareMinMinMin<T:Simple>(_ x: T, _ y:T) -> Bool {
  return x --- y
}


public func compareComparable<T:Comparable>(_ x: T, _ y:T) -> Bool {
  return x.compare(x, y)
}

// Check that a call of inherited Equatable.== can be devirtualized.
// CHECK-LABEL: sil @_T028devirt_inherited_conformance17testCompareEqualsSbyF : $@convention(thin) () -> Bool {
// CHECK: bb0
// CHECK-NEXT: integer_literal $Builtin.Int1, -1
// CHECK-NEXT: struct $Bool
// CHECK: return
// CHECK: }
public func testCompareEquals() -> Bool {
  return compareEquals(D(), D())
}



// Check that a call of inherited Simple.== can be devirtualized.
// CHECK-LABEL: sil @_T028devirt_inherited_conformance014testCompareMinfF0SbyF : $@convention(thin) () -> Bool {
// CHECK: bb0
// CHECK-NEXT: integer_literal $Builtin.Int1, -1
// CHECK-NEXT: struct $Bool
// CHECK: return
public func testCompareMinMinMin() -> Bool {
  return compareMinMinMin(D(), D())
}

// Check that a call of inherited Comparable.== can be devirtualized.
// CHECK-LABEL: sil @_T028devirt_inherited_conformance21testCompareComparableSbyF : $@convention(thin) () -> Bool {
// CHECK: bb0
// CHECK-NEXT: integer_literal $Builtin.Int1, -1
// CHECK-NEXT: struct $Bool
// CHECK: return
public func testCompareComparable() -> Bool {
  return compareComparable(D(), D())
}

public func BooCall<T:Simple>(_ x:T, _ y:T) -> Bool {
  return x.boo(y, y) 
}

// Check that a call of inherited Simple.boo can be devirtualized.
// CHECK-LABEL: sil @_T028devirt_inherited_conformance11testBooCallSbyF : $@convention(thin) () -> Bool {
// CHECK: bb0
// CHECK-NEXT: integer_literal $Builtin.Int1, 0
// CHECK-NEXT: struct $Bool
// CHECK: return
public func testBooCall() -> Bool {
  return BooCall(D(), D())
}
