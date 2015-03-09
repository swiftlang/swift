// RUN: %target-swift-frontend -emit-sil -O -emit-object %s

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
// CHECK; value_metatype $@thick A.Type
// CHECK: checked_cast_br
// CHECK: checked_cast_br
// CHECK: class_method
// CHECK: }
public func testValueMetatype(x:A) {
    x.dynamicType.foo()
}
