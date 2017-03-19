// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

class TestClass {

  final
  var finalProperty : Int { return 42 }

  final
  func finalMethod() -> Int { return 12 }


  func baseMethod() {}
}

class TestDerived : TestClass {
  final
  override func baseMethod() {}
}


// CHECK-LABEL: sil hidden @{{.*}}testDirectDispatch{{.*}} : $@convention(thin) (@owned TestClass) -> Int {
// CHECK: bb0([[ARG:%.*]] : $TestClass):
// CHECK: [[FINALMETH:%[0-9]+]] = function_ref @_T05final9TestClassC0A6Method{{[_0-9a-zA-Z]*}}F
// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: apply [[FINALMETH]]([[BORROWED_ARG]])
// CHECK: end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: [[FINALPROP:%[0-9]+]] = function_ref @_T05final9TestClassC0A8PropertySifg
// CHECK: apply [[FINALPROP]]([[BORROWED_ARG]])
// CHECK: end_borrow [[BORROWED_ARG]] from [[ARG]]
func testDirectDispatch(c : TestClass) -> Int {
  return c.finalMethod()+c.finalProperty
}


// Verify that the non-overriding final methods don't get emitted to the vtable.
// CHECK-LABEL: sil_vtable TestClass {
// CHECK-NEXT:  #TestClass.baseMethod!1: {{.*}} : _T05final9TestClassC10baseMethod{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT:  #TestClass.deinit!
// CHECK-NEXT:  #TestClass.init!initializer.1: {{.*}} : _T05final9TestClassC{{[_0-9a-zA-Z]*}}fc
// CHECK-NEXT: }

// Verify that overriding final methods don't get emitted to the vtable.
// CHECK-LABEL: sil_vtable TestDerived {
// CHECK-NEXT:  #TestClass.baseMethod!1: {{.*}} : _T05final11TestDerivedC10baseMethod{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT:  #TestClass.init!initializer.1: {{.*}} : _T05final11TestDerivedC{{[_0-9a-zA-Z]*}}fc
// CHECK-NEXT:  #TestDerived.deinit!
// CHECK-NEXT: }
