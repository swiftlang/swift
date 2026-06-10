// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -target %target-swift-5.1-abi-triple -emit-ir | %FileCheck %s

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

struct Impl: MyProtocol {}
extension MyProtocol {
  func decorate() -> some MyProtocol { Impl() }
}

func returnsClass3() {
  let c = {
    class MyClass3: MyProtocol {}
    _ = MyClass3()
    return Impl().decorate()
  }
  _ = c
}

// CHECK: @_DATA__TtCF41objc_runtime_name_local_class_opaque_type13returnsClass1FT_QuL_8MyClass1 = internal constant
// CHECK: @_DATA__TtCF41objc_runtime_name_local_class_opaque_typeg13returnsClass2QuL_8MyClass2 = internal constant
// CHECK: @{{[A-Za-z0-9_.]+}} = {{(private|internal)}} {{(unnamed_addr )?}}constant [{{[0-9]+}} x i8] c"_TtC{{[^"]*}}MyClass3\00", section "__TEXT,__objc_classname,cstring_literals"
