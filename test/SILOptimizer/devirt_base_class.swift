// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

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
private func foo(_ a: A) -> Int {


// Check that a.f() call can be devirtualized, even
// though f is defined by one of the A's superclasses.
//

// CHECK-LABEL: sil private [noinline] @{{.*}}foo
// CHECK-NOT: class_method
// CHECK: checked_cast_br
// CHECK: function_ref
// CHECK: checked_cast_br
// CHECK: function_ref
  return a.f()
}

// Check that invocation of addConstraint() gets completely devirtualized and inlined
//
// CHECK-LABEL: sil private [noinline] @$s17devirt_base_class2F233_{{.*}}4test
// CHECK-NOT: class_method
// CHECK-NOT: function_ref
// CHECK: return

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
    addConstraint()
  }
  
}

private class F3 : F2 {
}

private var f = F2(v:1)
f.test()
print("unary constraint is: \(f)")
