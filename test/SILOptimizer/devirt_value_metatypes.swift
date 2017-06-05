// RUN: %target-swift-frontend -emit-sil -O %s | %FileCheck %s

open class A {
  @inline(never)
  class func foo() {
  }
}


class B: A {
  @inline(never)
  override class func foo() {}
}

// CHECK-LABEL: sil [thunk] [always_inline] @_T022devirt_value_metatypes17testValueMetatypeyAA1ACF 
// CHECK: value_metatype $@thick A.Type
// CHECK: checked_cast_br
// CHECK: checked_cast_br
// CHECK: class_method
// CHECK: }
public func testValueMetatype(_ x:A) {
    type(of: x).foo()
}

open class C {
  @inline(never)
  class func foo() -> Int { return 0 }
}

open class D : C {
  @inline(never)
  override class func foo() -> Int { return 1 }
}

// CHECK-LABEL: sil [thunk] [always_inline] @_T022devirt_value_metatypes5testDSiAA1DCF
// CHECK-NOT: value_metatype %
// D.foo is an internal method, D has no subclasses and it is a wmo compilation,
// therefore D.foo method invocation can be devirtualized.
// CHECK: function_ref @_T022devirt_value_metatypes1DC3fooSiyFZTf4d_n
// CHECK-NOT: value_metatype %
// CHECK-NOT: checked_cast_br
// CHECK-NOT: class_method
// CHECK: }
public func testD(_ x: D) -> Int {
  return (type(of: x) as C.Type).foo()
}


public final class E : C {
  @inline(never)
  override class func foo() -> Int { return 1 }
}

// CHECK-LABEL: sil [thunk] [always_inline] @_T022devirt_value_metatypes5testESiAA1ECF
// CHECK-NOT: value_metatype $@thick E.Type
// CHECK_NOT: checked_cast_br
// CHECK: function_ref
// CHECK: apply
// CHECK: return
public func testE(_ x: E) -> Int {
  return (type(of: x) as C.Type).foo()
}
