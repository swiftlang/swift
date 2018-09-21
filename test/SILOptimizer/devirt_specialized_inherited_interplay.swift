// RUN: %target-swift-frontend -sil-verify-all -Xllvm -sil-inline-generics -O %s -emit-sil | %FileCheck %s

// This file consists of tests for making sure that protocol conformances and
// inherited conformances work well together when applied to each other. The
// check works by making sure we can blow through a long class hierarchy and
// expose the various "unknown" functions.
//
// As a side-test it also checks if all allocs can be promoted to the stack.
//
// *NOTE* If something like templated protocols is ever implemented this file
// needs to be updated.

// CHECK-LABEL: sil @$s38devirt_specialized_inherited_interplay6driveryyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK-NOT: alloc_ref
// CHECK: [[F0:%[0-9]+]] = function_ref @unknown0 : $@convention(thin) () -> ()
// CHECK: apply [[F0]]
// CHECK: apply [[F0]]
// CHECK: [[F1:%[0-9]+]] = function_ref @unknown1 : $@convention(thin) () -> ()
// CHECK: apply [[F1]]
// CHECK: apply [[F1]]
// CHECK: [[F2:%[0-9]+]] = function_ref @unknown2 : $@convention(thin) () -> ()
// CHECK: apply [[F2]]
// CHECK: apply [[F2]]
// CHECK: [[F3:%[0-9]+]] = function_ref @unknown3 : $@convention(thin) () -> ()
// CHECK: apply [[F3]]
// CHECK: apply [[F3]]
// CHECK: [[F4:%[0-9]+]] = function_ref @unknown4 : $@convention(thin) () -> ()
// CHECK: apply [[F4]]
// CHECK: apply [[F4]]
// CHECK: [[F5:%[0-9]+]] = function_ref @unknown5 : $@convention(thin) () -> ()
// CHECK: apply [[F5]]
// CHECK: apply [[F5]]
// CHECK: [[F6:%[0-9]+]] = function_ref @unknown6 : $@convention(thin) () -> ()
// CHECK: apply [[F6]]
// CHECK: apply [[F6]]
// CHECK: apply [[F6]]
// CHECK: apply [[F6]]
// CHECK: [[F8:%[0-9]+]] = function_ref @unknown8 :
// CHECK: apply [[F8]]
// CHECK: apply [[F8]]
// CHECK-NOT: dealloc_ref
// CHECK: return

@_silgen_name("unknown0")
func unknown0() -> ()
@_silgen_name("unknown1")
func unknown1() -> ()
@_silgen_name("unknown2")
func unknown2() -> ()
@_silgen_name("unknown3")
func unknown3() -> ()
@_silgen_name("unknown4")
func unknown4() -> ()
@_silgen_name("unknown5")
func unknown5() -> ()
@_silgen_name("unknown6")
func unknown6() -> ()
@_silgen_name("unknown7")
func unknown7() -> ()
@_silgen_name("unknown8")
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
  override func doSomething() {
    unknown8()
  }
}

func WhatShouldIDo<T : P>(_ t : T) {
  t.doSomething()
}
func WhatShouldIDo2(_ p : P) {
  p.doSomething()
}

public func driver1<X>(_ x:X) {
  let b = B3<X>()
  WhatShouldIDo(b)
  WhatShouldIDo2(b)
}

public func driver2() {
  driver1(G1<S>())
}


public func driver() {
  let a1 = A1()
  let a2 = A2()
  let a3 = A3<S>()
  let a4 = A4<S>()
  let a5 = A5<S>()
  let b1 = B1<S>()
  let b2 = B2<S>()
  let b3 = B3<S>()
  let b4 = B4<S>()

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
