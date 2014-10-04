// RUN: %swift -O  -module-name devirt_default_case -emit-sil  %s | FileCheck %s

// public class
public class Base1 {
  @inline(never) func inner() { println("Base")}
  func middle() { inner() }
// Check that call to Base1.middle cannot be devirtualized
//
// CHECK-LABEL: sil hidden @_TFC19devirt_default_case5Base15outerfS0_FT_T_
// CHECK: class_method 
// CHECK: }
  func outer() { 
    middle() 
  }
}

// public class
public class Derived1 : Base1 {
  override func inner() { println("Derived1") }
  @inline(never) final override func middle() { inner() }
}

// private class
private class Base2 {
  @inline(never) func inner() { println("Base")}
  func middle() { inner() }
  func outer() { middle() }
}

// private class
private class Derived2 : Base2 {
  override func inner() { println("Derived1") }
  @inline(never) final override func middle() { inner() }
}


// Check that call to Base2.middle can be devirtualized
//
// CHECK-LABEL: sil @_TF19devirt_default_case9callOuterFSiSi
// CHECK: function_ref @_TFC19devirt_default_caseP33_77424841540E67CC820F5E5F7940DCB08Derived26middlefS0_FT_T_ 
// CHECK: function_ref @_TFC19devirt_default_caseP33_77424841540E67CC820F5E5F7940DCB05Base25innerfS0_FT_T_
// CHECK-NOT: class_method
// CHECK: return
public func callOuter(x: Int) -> Int {

  var o:Base2
  
  if x == 1 {
    o = Base2()
  } else {
    o = Derived2()
  }
  
  o.outer()
  return x
}

// internl class
class Base3 {
  @inline(never) func inner() { println("Base")}
  @inline(never) func middle() { inner() }
// Check that call to Base3.middle can be devirtualized
// 
// CHECK-LABEL: sil hidden @_TFC19devirt_default_case5Base35outerfS0_FT_T_
// CHECK: function_ref @_TFC19devirt_default_caseP33_77424841540E67CC820F5E5F7940DCB08Derived36middlefS0_FT_T_
// CHECK: function_ref @_TFC19devirt_default_case5Base36middlefS0_FT_T_
// CHECK-NOT: class_method
// CHECK: }
  func outer() {
    middle()
  }
}

// private class
private class Derived3 : Base3 {
  override func inner() { println("Derived1") }
  @inline(never) final override func middle() { inner() }
}

class A2 { @inline(never) func f() -> Int { return 0 } }
class B2 : A2 {}
class C2 : A2 {}
class D2: B2 {}
class E2 :C2 {}

@inline(never)
func foo(a: A2) -> Int {
// Check that call to A2.f() can be devirualized.
//
// CHECK-LABEL: sil hidden [noinline] @_TF19devirt_default_case3fooFCS_2A2Si
// CHECK: function_ref @_TFC19devirt_default_case2A21ffS0_FT_Si
// CHECK-NOT: class_method
// CHECK: }
  return a.f()
}

println("foo(E()) = \(foo(E2()))")

class A3 { @inline(never) func f() -> Int { return 0 } }
class B3 : A3 { @inline(never) override func f() -> Int { return 1 }}
class C3 : A3 {}
class D3: C3 {}
class E3 :C3 {}

// A, C,D,E all use the same implementation. 
// B has its own implementation.
@inline(never)
func foo(a: A3) -> Int {
// Check that call to A3.f() can be devirualized.
//
// CHECK-LABEL: sil hidden [noinline] @_TF19devirt_default_case3fooFCS_2A3Si
// CHECK: function_ref @_TFC19devirt_default_case2B31ffS0_FT_Si
// CHECK: function_ref @_TFC19devirt_default_case2A31ffS0_FT_Si
// CHECK-NOT: class_method
// CHECK: }
  return a.f()
}

println("foo(E()) = \(foo(E3()))")

class Base4 {
  func test() { 
// Check that call to foo() can be devirtualized
//
// CHECK-LABEL: sil hidden @_TFC19devirt_default_case5Base44testfS0_FT_T_ 
// CHECK: function_ref @_TFC19devirt_default_case8Derived43foofS0_FT_T_ 
// CHECK: function_ref @_TFC19devirt_default_case5Base43foofS0_FT_T_
// CHECK-NOT: class_method
// CHECK: }
    foo() 
  }
  
  @inline(never) func foo() { }
}

class Derived4 : Base4 {
  @inline(never) override func foo() { }
}

public class Base5 {
  func test() { 
// Check that call to foo() does not use class_method, because
// it is a final method.
//
// CHECK-LABEL: sil hidden @_TFC19devirt_default_case5Base54testfS0_FT_T_ 
// CHECK: function_ref @_TFC19devirt_default_case5Base53foofS0_FT_T_
// CHECK-NOT: class_method
// CHECK: }
    foo() 
  }
  
  @inline(never) public final func foo() { }
}

class Derived5 : Base5 {
}

public class C6 { 
  func bar() -> Int { return 1 } 
}

class D6 : C6 { 
  override func bar() -> Int { return 2 } 
}

@inline(never)
func check_static_class_devirt(c: C6) -> Int { 
// Check that C.bar() and D.bar() are devirtualized.
//
// CHECK-LABEL: sil hidden [noinline] @_TF19devirt_default_case25check_static_class_devirtFCS_2C6Si
// CHECK: checked_cast_br [exact] %0 : $C6 to $D6
// CHECK: checked_cast_br [exact] %0 : $C6 to $C6
// CHECK: class_method
// CHECK: return
  return c.bar() 
}

println(check_static_class_devirt(D6()))
