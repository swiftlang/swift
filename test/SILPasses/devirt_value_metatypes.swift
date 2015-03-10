// RUN: %target-swift-frontend -emit-sil -O %s | FileCheck %s

public class A {
  @inline(never)
  class func foo() {
  }
}


class B: A {
  @inline(never)
  override class func foo() {
    println("B")
  }
}

// CHECK-LABEL: sil @_TF22devirt_value_metatypes17testValueMetatypeFCS_1AT_ 
// CHECK: value_metatype $@thick A.Type
// CHECK: checked_cast_br
// CHECK: checked_cast_br
// CHECK: class_method
// CHECK: }
public func testValueMetatype(x:A) {
    x.dynamicType.foo()
}

public class C {
  @inline(never)
  class func foo() -> Int { return 0 }
}

public class D : C {
  @inline(never)
  override class func foo() -> Int { return 1 }
}

// CHECK-LABEL: sil @_TF22devirt_value_metatypes5testDFCS_1DSi
// CHECK: value_metatype $@thick D.Type
// CHECK: checked_cast_br
// CHECK: function_ref
// CHECK: class_method
// CHECK: }
public func testD(x: D) -> Int {
  return (x.dynamicType as C.Type).foo()
}


public final class E : C {
  @inline(never)
  override class func foo() -> Int { return 1 }
}

// CHECK-LABEL: sil @_TF22devirt_value_metatypes5testEFCS_1ESi
// CHECK-NOT: value_metatype $@thick E.Type
// CHECK_NOT: checked_cast_br
// CHECK: function_ref
// CHECK: apply
// CHECK: return
public func testE(x: E) -> Int {
  return (x.dynamicType as C.Type).foo()
}
