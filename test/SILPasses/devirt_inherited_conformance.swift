// RUN: %target-swift-frontend -O %s -emit-sil -sil-inline-threshold 1000 | FileCheck %s

// Make sure that we can dig all the way through the class hierarchy and
// protocol conformances.

// CHECK-LABEL: sil @_TF28devirt_inherited_conformance6driverFT_T_ : $@thin () -> () {
// CHECK: bb0
// CHECK: function_ref unknown2a
// CHECK-NEXT: function_ref @unknown2a : $@thin () -> ()
// CHECK-NEXT: apply
// CHECK-NEXT: apply
// CHECK: function_ref unknown3a
// CHECK-NEXT: function_ref @unknown3a : $@thin () -> ()
// CHECK-NEXT: apply
// CHECK-NEXT: apply
// CHECK-NEXT: tuple
// CHECK-NEXT: return

@asmname("unknown1a")
func unknown1a() -> ()
@asmname("unknown1b")
func unknown1b() -> ()
@asmname("unknown2a")
func unknown2a() -> ()
@asmname("unknown2b")
func unknown2b() -> ()
@asmname("unknown3a")
func unknown3a() -> ()
@asmname("unknown3b")
func unknown3b() -> ()

struct Int32 {}

protocol P {
  // We do not specialize typealias's correctly now.
  //typealias X
  func doSomething(x : Int32)

  // This exposes a SILGen bug. FIXME: Fix up this test in the future.
  // class func doSomethingMeta()
}

class B : P {
  // We do not specialize typealias's correctly now.
  //typealias X = B
  func doSomething(x : Int32) {
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
  override func doSomething(x : Int32) {
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
  override func doSomething(x : Int32) {
    unknown3a()
  }

  // See comment in protocol P
  //override class func doSomethingMeta() {
  //  unknown3b()
  //}
}

func WhatShouldIDo<T : P>(t : T, x : Int32) {
  t.doSomething(x)
}

func WhatShouldIDo2(p : P, x : Int32) {
  p.doSomething(x)
}

public func driver() -> () {
  var b2 = B2()
  var b3 = B3()
  var x = Int32()

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
   func compare(Self, Self) -> Bool
}

// Define a custom operator to be used instead of ==
infix operator --- { associativity left precedence 140 } 

// Simple is a protocol tat simply defines an operator and
// a few methods with different number of arguments.
public protocol Simple {
   func foo(Self) -> Bool
   func boo(Self, Self) -> Bool
   func ---(Self, Self)->Bool
}

public class C: Equatable, Comparable, Simple {
  public func compare(c1:C, _ c2:C) -> Bool {
    return c1 == c2
  }
  
  public func foo(c:C) -> Bool {
    return true
  }

  public func boo(c1:C, _ c2:C) -> Bool {
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

public func compareEquals<T:Equatable>(x: T, y:T) -> Bool {
  return x == y
}

public func compareMinMinMin<T:Simple>(x: T, y:T) -> Bool {
  return x --- y
}


public func compareComparable<T:Comparable>(x: T, y:T) -> Bool {
  return x.compare(x, y)
}

// Check that a call of inherited Equatable.== can be devirtualized.
// CHECK-LABEL: sil @_TF28devirt_inherited_conformance17testCompareEqualsFT_Sb : $@thin () -> Bool {
// CHECK: bb0
// CHECK-NEXT: integer_literal $Builtin.Int1, -1
// CHECK-NEXT: struct $Bool
// CHECK: return
// CHECK: }
public func testCompareEquals() -> Bool {
  return compareEquals(D(), D())
}



// Check that  acall of inherited Simple.== can be devirtualized.
// CHECK-LABEL: sil @_TF28devirt_inherited_conformance20testCompareMinMinMinFT_Sb : $@thin () -> Bool {
// CHECK: bb0
// CHECK-NEXT: integer_literal $Builtin.Int1, -1
// CHECK-NEXT: struct $Bool
// CHECK: return
public func testCompareMinMinMin() -> Bool {
  return compareMinMinMin(D(), D())
}

// Check that a call of inherited Comparable.== can be devirtualized.
// CHECK-LABEL: sil @_TF28devirt_inherited_conformance21testCompareComparableFT_Sb : $@thin () -> Bool {
// CHECK: bb0
// CHECK-NEXT: integer_literal $Builtin.Int1, -1
// CHECK-NEXT: struct $Bool
// CHECK: return
public func testCompareComparable() -> Bool {
  return compareComparable(D(), D())
}

public func BooCall<T:Simple>(x:T, y:T) -> Bool {
  return x.boo(y, y) 
}

// Check that a call of inherited Simple.boo can be devirtualized.
// CHECK-LABEL: sil @_TF28devirt_inherited_conformance11testBooCallFT_Sb : $@thin () -> Bool {
// CHECK: bb0
// CHECK-NEXT: integer_literal $Builtin.Int1, 0
// CHECK-NEXT: struct $Bool
// CHECK: return
public func testBooCall() -> Bool {
  return BooCall(D(), D())
}
