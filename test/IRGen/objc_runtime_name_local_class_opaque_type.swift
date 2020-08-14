// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -disable-availability-checking -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

protocol MyProtocol {}

func returnsClass1() -> some MyProtocol {
  class MyClass1: MyProtocol {}
  return MyClass1()
}

var returnsClass2: some MyProtocol {
  class MyClass2: MyProtocol {}
  return MyClass2()
}

// CHECK: @_DATA__TtCF41objc_runtime_name_local_class_opaque_type13returnsClass1FT_QuL_8MyClass1 = internal constant
// CHECK: @_DATA__TtCF41objc_runtime_name_local_class_opaque_typeg13returnsClass2QuL_8MyClass2 = internal constant
