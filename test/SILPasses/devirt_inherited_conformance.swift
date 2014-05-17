// RUN: %swift -O3 %s -emit-sil -sil-inline-threshold 1000 | FileCheck %s

// Make sure that we can dig all the way through the class hierarchy and
// protocol conformances.

// CHECK-LABEL: sil @_TF28devirt_inherited_conformance6driverFT_T_ : $@thin () -> () {
// CHECK: bb0
// CHECK: function_ref unknown2a
// CHECK-NEXT: function_ref @unknown2a : $@thin () -> ()
// CHECK-NEXT: apply
// CHECK: protocol_method
// CHECK: function_ref unknown3a
// CHECK-NEXT: function_ref @unknown3a : $@thin () -> ()
// CHECK-NEXT: apply
// CHECK: protocol_method
// CHECK: sil_vtable B {

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
  //unknown3b()
  //}
}

func WhatShouldIDo<T : P>(t : T, x : Int32) {
  t.doSomething(x)
}
func WhatShouldIDo2(p : P, x : Int32) {
  p.doSomething(x)
}

func driver() -> () {
  var b2 = B2()
  var b3 = B3()
  var x = Int32()

  WhatShouldIDo(b2, x)
  WhatShouldIDo2(b2, x)
  WhatShouldIDo(b3, x)
  WhatShouldIDo2(b3, x)
}

driver()
