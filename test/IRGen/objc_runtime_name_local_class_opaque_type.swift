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

func returnsClass4() {
  let c: @MainActor () -> Void = {
    class MyClass4: MyProtocol {}
    _ = MyClass4()
  }
  _ = c
}

// CHECK: @_DATA__TtCF41objc_runtime_name_local_class_opaque_type13returnsClass1FT_QuL_8MyClass1 = internal constant
// CHECK: @_DATA__TtCF41objc_runtime_name_local_class_opaque_typeg13returnsClass2QuL_8MyClass2 = internal constant
// MyClass3 is nested in a scope that the old mangler can't represent (an
// opaque type), so the runtime name falls back to the new mangling.
// CHECK: @{{.*}} = {{(private|internal)}} {{(unnamed_addr )?}}constant [{{[0-9]+}} x i8] c"$s41objc_runtime_name_local_class_opaque_type13returnsClass3yyFAA10MyProtocolPAAE8decorateQryFQOyAA4ImplV_Qo_ycfU_0jI0L_C\00", section "__TEXT,__objc_classname,cstring_literals"
// MyClass4 is nested in a global-actor-isolated function type, which the old
// mangler can't represent either, so it also falls back to the new mangling.
// CHECK: @{{.*}} = {{(private|internal)}} {{(unnamed_addr )?}}constant [{{[0-9]+}} x i8] c"$s41objc_runtime_name_local_class_opaque_type13returnsClass4yyFyyScMYccfU_02MyI0L_C\00", section "__TEXT,__objc_classname,cstring_literals"
