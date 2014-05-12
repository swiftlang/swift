// RUN: %swift -sil-verify-all -O3 %s -emit-sil | FileCheck %s
// XFAIL: *

// This file consists of tests for making sure that protocol conformances and
// inherited conformances work well together when applied to each other. The
// check works by making sure we can blow through a long class hierarchy and
// expose the various "unknown" functions.
//
// *NOTE* If something like templated protocols is ever implemented this file
// needs to be updated.

// CHECK-LABEL: sil private @top_level_code : $@thin () -> () {
// CHECK: bb0:
// CHECK-NEXT: alloc_ref $A4<S>
// CHECK-NEXT: alloc_ref $B2<S>
// CHECK-NEXT: function_ref
// CHECK-NEXT: function_ref @unknown3 : $@thin () -> ()
// CHECK-NEXT: apply
// CHECK-NEXT: apply
// CHECK-NEXT: strong_release
// CHECK-NEXT: function_ref
// CHECK-NEXT: function_ref @unknown5 : $@thin () -> ()
// CHECK-NEXT: apply
// CHECK-NEXT: apply
// CHECK-NEXT: strong_release
// CHECK-NEXT: tuple
// CHECK-NEXT: return

@asmname("unknown0")
func unknown0() -> ()
@asmname("unknown1")
func unknown1() -> ()
@asmname("unknown2")
func unknown2() -> ()
@asmname("unknown3")
func unknown3() -> ()
@asmname("unknown4")
func unknown4() -> ()
@asmname("unknown5")
func unknown5() -> ()


struct S {}

protocol P {
  func doSomething()
}

// Normal conformance
class A1 : P {
  func doSomething() {
    unknown0()
  }
}

// Inherited conformance from P
class A2 : A1 {
  override func doSomething() {
    unknown1()
  }
}

// Specialized Inherited conformance from P
class A3<T> : A2 {
  override func doSomething() {
    unknown2()
  }
}

// Inherited Specialized Inherited conformance from P
class A4<T> : A3<T> {
  override func doSomething() {
    unknown3()
  }
}

// Specialized conformance from P
class B1<T> : P {
  func doSomething() {
    unknown4()
  }
}

// Inherited Specialized conformance from P
class B2<T> : B1<T> {
  override func doSomething() {
    unknown5()
  }
}

func WhatShouldIDo<T : P>(t : T) {
  t.doSomething()
}
func WhatShouldIDo2(p : P) {
  p.doSomething()
}

func driver() {
  var a = A4<S>()
  var b = B2<S>()

  WhatShouldIDo(a)
  WhatShouldIDo2(a)

  WhatShouldIDo(b)
  WhatShouldIDo2(b)
}

driver()
