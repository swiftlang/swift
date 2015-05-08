// RUN: %target-swift-frontend -O -emit-sil -disable-func-sig-opts %s | FileCheck %s

public class Base1 { @inline(never) func f() -> Int { return 0 } }

public class Base2: Base1 {
}

private class A: Base2 { }

private class B : A { 
    @inline(never) override func f() -> Int { return 1 }
}

private class C : A { 
    @inline(never) override func f() -> Int { return 2 }
}

@inline(never)
private func foo(a: A) -> Int {
// Check that a.f() call can be devirtualized, even
// though f is defined by one of the A's superclasses.
//
// CHECK-LABEL: sil private [noinline] @_TF17devirt_base_classP33_C1ED27807F941A622F32D66AB60A15CD3fooFCS_P33_C1ED27807F941A622F32D66AB60A15CD1ASi
// CHECK-NOT: class_method
// CHECK: checked_cast_br
// CHECK: function_ref @_TFC17devirt_base_classP33_C1ED27807F941A622F32D66AB60A15CD1B1ffS0_FT_Si
// CHECK: checked_cast_br
// CHECK: function_ref @_TFC17devirt_base_classP33_C1ED27807F941A622F32D66AB60A15CD1C1ffS0_FT_Si
  return a.f()
}

print("foo(C()) = \(foo(C()))")

private class F1 {
  init() {
  }

  func addConstraint() {
    addToGraph()
  }

  func addToGraph() {
  }
}

private class F2 : F1 {
  init (v : Int) {
    super.init()
    addConstraint()
  }

  override func addToGraph() {
  }
  
  @inline(never) 
  func test() {
// Check that invocation of addConstraint() gets completely devirtualized and inlined
//
// CHECK-LABEL: sil private [noinline] @_TFC17devirt_base_classP33_C1ED27807F941A622F32D66AB60A15CD2F24testfS0_FT_T_
// CHECK-NOT: class_method
// CHECK-NOT: function_ref
// CHECK: return
    addConstraint()
  }
  
}

private class F3 : F2 {
}

private var f = F2(v:1)
f.test()
print("unary constraint is: \(f)")
