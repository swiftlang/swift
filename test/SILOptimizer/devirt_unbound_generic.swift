// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-sil -O %s | %FileCheck %s

// We used to crash on this when trying to devirtualize t.boo(a, 1),
// because it is an "apply" with unbound generic arguments and
// devirtualizer is not able to devirtualize unbound generic
// invocations yet.
//
// rdar://19912272

protocol P {
   associatedtype Node
}

class C<T:P> {
   typealias Node = T.Node

   func foo(_ n:Node) {
   }

   func boo<S>(_ n:Node, s:S) {
   }
}

func test1<T>(_ t:C<T>, a: T.Node) {
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
// CHECK: sil hidden [noinline] @_T022devirt_unbound_generic5test2yAA7DerivedCyxGlF
// CHECK-NOT: class_method
// CHECK: function_ref @_T022devirt_unbound_generic7DerivedC3fooyyF
// CHECK-NOT: class_method
// CHECK: return
@inline(never)
func test2<T>(_ d: Derived<T>) {
   d.foo()
}

public func doTest2<T>(_ t:T) {
  test2(Derived<T>())
}

// Check that the class method Derived<T>.boo can be devirtualized, because Derived.boo is an internal function,
// Derived has no subclasses and it is a WMO compilation.
// CHECK: sil hidden [noinline] @_T022devirt_unbound_generic5test3yAA7DerivedCyxGlF
// CHECK-NOT: class_method
// CHECK: function_ref @_T022devirt_unbound_generic7DerivedC3booyyFZ
// CHECK-NOT: class_method
// CHECK: return
@inline(never)
func test3<T>(_ d: Derived<T>) {
   type(of: d).boo()
}

public func doTest3<T>(_ t:T) {
  test3(Derived<T>())
}


public protocol ProtocolWithAssocType {
  associatedtype Element
}

private class CP<Base: ProtocolWithAssocType> {
   var value: Base.Element
   init(_ v: Base.Element) {
     value = v
   }

   func getCount() -> Int32 {
     return 1
   }
}

private class Base1: ProtocolWithAssocType {
   typealias Element = Int32
}


private class Base2<T>: ProtocolWithAssocType {
   typealias Element = Int32
}

private class CP2: CP<Base2<Int>> {
  init() {
    super.init(1)
  }

  override func getCount() -> Int32 {
    return 2
  }
}

private class CP3: CP<Base2<Int>> {
  init() {
    super.init(1)
  }

  override func getCount() -> Int32 {
    return 3
  }
}

public class CC<CT> {
   func next() -> CT? {
     return nil
   }
}

public protocol QQ {
  associatedtype Base: PP
}

public protocol PP {
  associatedtype Element
}

internal class D<DT: QQ> : CC<DT.Base.Element> {
  override func next() -> DT.Base.Element? {
    return nil
  }
}

public struct S: PP {
  public typealias Element = Int32
}

final public class E: QQ {
  public typealias Base = S
}

// Check that c.next() inside test4 gets completely devirtualized.
// CHECK-LABEL: sil @{{.*}}test4
// CHECK-NOT: class_method
// CHECK: return
public func test4() -> Int32? {
  let c: CC<Int32> = D<E>();
  return c.next()
}

public func test5() -> Int32? {
  return testDevirt(D<E>())
}

// Check that testDevirt is specialized and uses speculative devirtualization.
// CHECK-LABEL: sil shared [noinline] @{{.*}}testDevirt
// CHECK: checked_cast_br [exact] %{{.*}} : $CC<Int32> to $CC<Int32>
// CHECK: class_method
// CHECK: }
@inline(never)
public func testDevirt<T>(_ c: CC<T>) -> T? {
  return c.next()
}

// The compiler used to crash on this code, because of
// generic types involved in the devirtualization.
//
// rdar://25891588
//
// CHECK-LABEL: sil private [noinline] @{{.*}}test6
// CHECK-NOT: class_method
// CHECK-NOT: checked_cast_br
// CHECK-NOT: class_method
// CHECK: }
@inline(never)
private func test6<T: ProtocolWithAssocType>(_ c: CP<T>) -> T.Element {
  return c.value
}

public func doTest6() {
  test6(CP<Base1>(1))
}

// CHECK-LABEL: sil private [noinline] @{{.*}}test7
// CHECK-NOT: class_method
// CHECK: checked_cast_br
// CHECK-NOT: class_method
// CHECK: }
@inline(never)
private func test7<T: ProtocolWithAssocType>(_ c: CP<T>) -> Int32 {
  return c.getCount()
}

public func doTest7() {
  test7(CP2())
}


