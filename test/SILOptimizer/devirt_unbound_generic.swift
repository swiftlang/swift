// RUN: %target-swift-frontend -emit-sil -O -emit-sil %s | FileCheck %s

// We used to crash on this when trying to devirtualize t.boo(a, 1),
// because it is an "apply" with unbound generic arguments and
// devirtualizer is not able to devirtualize unbound generic
// invocations yet.
//
// rdar://19912272

protocol P {
   typealias Node
}

class C<T:P> {
   typealias Node = T.Node

   func foo(n:Node) {
   }

   func boo<S>(n:Node, s:S) {
   }
}

func test1<T>(t:C<T>, a: T.Node) {
   t.boo(a, s:1)
}


class Base<T> {
  func foo() {
  }

  class func boo() {
  }
}

class Derived<T> : Base<T> {
  override func foo() {
  }
  
  override class func boo() {
  }
}

// Check that the instance method Derived<T>.foo can be devirtualized, because Derived.foo is an internal function,
// Derived has no subclasses and it is a WMO compilation.
// CHECK: sil hidden [noinline] @_TF22devirt_unbound_generic5test2urFGCS_7Derivedx_T_
// CHECK-NOT: class_method
// CHECK: function_ref @_TFC22devirt_unbound_generic7Derived3foofT_T_
// CHECK-NOT: class_method
// CHECK: return
@inline(never)
func test2<T>(d: Derived<T>) {
   d.foo()
}

public func doTest2<T>(t:T) {
  test2(Derived<T>())
}

// Check that the class method Derived<T>.boo can be devirtualized, because Derived.boo is an internal function,
// Derived has no subclasses and it is a WMO compilation.
// CHECK: sil hidden [noinline] @_TF22devirt_unbound_generic5test3urFGCS_7Derivedx_T_
// CHECK-NOT: class_method
// CHECK: function_ref @_TZFC22devirt_unbound_generic7Derived3boofT_T_
// CHECK-NOT: class_method
// CHECK: return
@inline(never)
func test3<T>(d: Derived<T>) {
   d.dynamicType.boo()
}

public func doTest3<T>(t:T) {
  test3(Derived<T>())
}
