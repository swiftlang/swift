// RUN: %swift -O3 %s -emit-sil -sil-inline-threshold 1000 | FileCheck %s

// Make sure that we can dig all the way through the class hierarchy and
// protocol conformances.

// CHECK-LABEL: sil @_TF28devirt_inherited_conformance6driverFT_T_ : $@thin () -> () {
// CHECK: bb0
// CHECK-NEXT: function_ref unknown2a
// CHECK-NEXT: function_ref @unknown2a : $@thin () -> ()
// CHECK-NEXT: apply
// CHECK-NEXT: apply
// CHECK-NEXT: function_ref unknown3a
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

protocol P {
  // We do not specialize typealias's correctly now.
  //typealias X
  func doSomething()

  // This exposes a SILGen bug. FIXME: Fix up this test in the future.
  // class func doSomethingMeta()
}

class B : P {
  // We do not specialize typealias's correctly now.
  //typealias X = B
  func doSomething() {
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
  override func doSomething() {
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
  override func doSomething() {
    unknown3a()
  }

  // See comment in protocol P
  //override class func doSomethingMeta() {
  //unknown3b()
  //}
}

func WhatShouldIDo<T : P>(t : T) {
  t.doSomething()
}
func WhatShouldIDo2(p : P) {
  p.doSomething()
}

func driver() -> () {
  var b2 = B2()
  var b3 = B3()

  WhatShouldIDo(b2)
  WhatShouldIDo2(b2)
  WhatShouldIDo(b3)
  WhatShouldIDo2(b3)
}

driver()
