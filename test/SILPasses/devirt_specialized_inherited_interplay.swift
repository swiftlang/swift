// RUN: %target-swift-frontend -sil-verify-all -O %s -emit-sil | FileCheck %s

// This file consists of tests for making sure that protocol conformances and
// inherited conformances work well together when applied to each other. The
// check works by making sure we can blow through a long class hierarchy and
// expose the various "unknown" functions.
//
// *NOTE* If something like templated protocols is ever implemented this file
// needs to be updated.

// CHECK-LABEL: sil @_TF38devirt_specialized_inherited_interplay6driverFT_T_ : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK-NEXT: alloc_ref $A3<S>
// CHECK-NEXT: alloc_ref $A4<S>
// CHECK-NEXT: alloc_ref $A5<S>
// CHECK-NEXT: alloc_ref $B1<S>
// CHECK-NEXT: alloc_ref $B2<S>
// CHECK-NEXT: alloc_ref $B3<S>
// CHECK-NEXT: alloc_ref $B4<S>
// CHECK-NEXT: function_ref
// CHECK-NEXT: function_ref @unknown0 : $@convention(thin) () -> ()
// CHECK-NEXT: apply
// CHECK-NEXT: apply
// CHECK-NEXT: function_ref
// CHECK-NEXT: function_ref @unknown1 : $@convention(thin) () -> ()
// CHECK-NEXT: apply
// CHECK-NEXT: apply
// CHECK-NEXT: function_ref
// CHECK-NEXT: function_ref @unknown2 : $@convention(thin) () -> ()
// CHECK-NEXT: apply
// CHECK-NEXT: apply
// CHECK-NEXT: function_ref
// CHECK-NEXT: function_ref @unknown3 : $@convention(thin) () -> ()
// CHECK-NEXT: apply
// CHECK-NEXT: apply
// CHECK-NEXT: function_ref
// CHECK-NEXT: function_ref @unknown4 : $@convention(thin) () -> ()
// CHECK-NEXT: apply
// CHECK-NEXT: apply
// CHECK-NEXT: function_ref
// CHECK-NEXT: function_ref @unknown5 : $@convention(thin) () -> ()
// CHECK-NEXT: apply
// CHECK-NEXT: apply
// CHECK-NEXT: function_ref
// CHECK-NEXT: function_ref @unknown6 : $@convention(thin) () -> ()
// CHECK-NEXT: apply
// CHECK-NEXT: apply
// CHECK-NEXT: apply
// CHECK-NEXT: apply
// Check that B4.doSomething cannot be completely devirtualized
// CHECK-NEXT: upcast
// CHECK-NEXT: class_method
// CHECK-NEXT: strong_retain
// CHECK-NEXT: apply
// CHECK-NEXT: apply
// CHECK-NEXT: strong_release
// CHECK-NEXT: strong_release
// CHECK-NEXT: strong_release
// CHECK-NEXT: strong_release
// CHECK-NEXT: strong_release
// CHECK-NEXT: strong_release
// CHECK-NEXT: strong_release
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
@asmname("unknown6")
func unknown6() -> ()
@asmname("unknown7")
func unknown7() -> ()
@asmname("unknown8")
func unknown8() -> ()

protocol P1 {
   func foo()
}

struct S:P1 {
   func foo() {
   }
}


public final class G1<T> {
}

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

class A5<E>: A3<Array<E>> {
  override func doSomething() {
    unknown4()
  }
}

// Specialized conformance from P
class B1<T> : P {
  func doSomething() {
    unknown5()
  }
}

// Inherited Specialized conformance from P
class B2<T> : B1<G1<T>> {
  override func doSomething() {
    unknown6()
  }
}

class B3<E>: B2<Array<E>> {
}

class B4<F>: B3<Array<Array<Int>>> {
  // Free generic arguments -> cannot be devirtualized.
  override func doSomething<X,Y,Z>() {
    unknown8()
  }
}

func WhatShouldIDo<T : P>(t : T) {
  t.doSomething()
}
func WhatShouldIDo2(p : P) {
  p.doSomething()
}

public func driver1<X>(x:X) {
  var b = B3<X>()
  WhatShouldIDo(b)
  WhatShouldIDo2(b)
}

public func driver2() {
  driver1(G1<S>())
}


public func driver() {
  var a1 = A1()
  var a2 = A2()
  var a3 = A3<S>()
  var a4 = A4<S>()
  var a5 = A5<S>()
  var b1 = B1<S>()
  var b2 = B2<S>()
  var b3 = B3<S>()
  var b4 = B4<S>()

  WhatShouldIDo(a1)
  WhatShouldIDo2(a1)

  WhatShouldIDo(a2)
  WhatShouldIDo2(a2)

  WhatShouldIDo(a3)
  WhatShouldIDo2(a3)

  WhatShouldIDo(a4)
  WhatShouldIDo2(a4)

  WhatShouldIDo(a5)
  WhatShouldIDo2(a5)


  WhatShouldIDo(b1)
  WhatShouldIDo2(b1)
  
  WhatShouldIDo(b2)
  WhatShouldIDo2(b2)
  
  WhatShouldIDo(b3)
  WhatShouldIDo2(b3)

  WhatShouldIDo(b4)
  WhatShouldIDo2(b4)
}
